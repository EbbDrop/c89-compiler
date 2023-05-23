// In this file, the term "reduced graph" is used to denote the graph formed by only preserving the
// edges formed by default successors/predecessor relations (and dropping the nodes/blocks that no
// longer have edges in this graph).
//
// This graph is extensively used, because one of the constraints that a fixed function should meet
// is that the _reduced graph_ is acyclic (a DAG).

#[cfg(test)]
mod test;

use crate::{Function, Root};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnfixableProblem {
    MissingEntryBlock,
    VirtualInstrOrReg,
    IncompleteGraph,
}

/// Checks if the root matches all the constraints required to output valid MIPS, and applies fixes
/// to make it valid if not. See [`crate::MipsOutputter`] for the list of constraints.
///
/// If it's not possible to meet all the constraints, an `Err` result will be returned containing
/// the first unfixable problem that was encountered. See [`UnfixableProblem`] for the list of
/// problems that are not fixable.
///
/// Note that this may fix more than strictly required to meet the [`crate::MipsOutputter`]
/// constraints.
pub fn fix_root(root: &mut Root) -> Result<(), UnfixableProblem> {
    for function in root.functions_mut() {
        fix_function(function)?;
    }
    Ok(())
}

/// Checks if the function/graph matches all the constraints for functions/graphs required to output
/// valid MIPS, and applies fixes to make it valid if not. See [`crate::MipsOutputter`] for the list
/// of constraints.
///
/// If it's not possible to meet all the constraints, an `Err` result will be returned containing
/// the first unfixable problem that was encountered. See [`UnfixableProblem`] for the list of
/// problems that are not fixable.
///
/// Note that this may fix more than strictly required to meet the [`crate::MipsOuputter`]
/// constraints. In particular, it might also fix problems with blocks that have no predecessors,
/// which is not required to be able to output valid mips (because they wouldn't show up in the
/// output anyway).
pub fn fix_function(function: &mut Function) -> Result<(), UnfixableProblem> {
    if check::function_contains_virtuals(function) {
        return Err(UnfixableProblem::VirtualInstrOrReg);
    }
    if !check::is_graph_complete(function) {
        return Err(UnfixableProblem::IncompleteGraph);
    }
    // First make sure the entry block doesn't have a default predecessor.
    let Some(entry_block_id) = function.entry_block().map(|b| b.id()) else {
        return Err(UnfixableProblem::MissingEntryBlock)
    };
    {
        let default_predecessors = function[entry_block_id]
            .dpreds_in(function)
            .map(|p| p.id())
            .collect::<Vec<_>>();
        for block_id in default_predecessors {
            let block = &function[block_id];
            let swap_candidates = block
                .ndsuccidxs()
                .filter(|&idx| block.succ_id(idx) != entry_block_id)
                .collect::<Vec<_>>(); // TODO: find optimal order of candidates
            fix::antichain_successor(
                function,
                block_id,
                function[block_id].dsuccidx().unwrap(),
                swap_candidates,
            );
        }
        // Fixing the default predecessors shouldn't have introduced any new default predecessors.
        assert!(!function[entry_block_id].has_dpreds_in(function));
    }
    // Then break any other possible cycles.
    'outer: loop {
        // Traverse the function starting from the entry point and following default successors
        // whenever possible. If we encounter a block while traversing of which the default
        // has already been traversed, we know that that block is part of a cycle in the reduced
        // graph.
        let mut traverser = function.traverse_all();

        // `cycle_base` will be the block of which we'll antichain the default successor to break
        // the cycle. If no cycle is found, we can successfully break from 'outer.
        let (cycle_base, dsucc_idx, dsucc_id) = loop {
            let Some(block) = traverser.next() else { break 'outer };
            if let Some(dsucc_idx) = block.dsuccidx_in(function) {
                let dsucc_id = block.succ_id(dsucc_idx);
                if traverser.has_traversed(dsucc_id) {
                    break (block, dsucc_idx, dsucc_id);
                }
            }
        };
        // Found a cycle, so fix it by either swapping the default successor with a
        // non-default successor, or by introducing indirection.
        let swap_candidates = cycle_base
            .ndsuccidxs()
            .filter(|&idx| {
                let id = cycle_base.succ_id(idx);
                id != dsucc_id && id != entry_block_id && !traverser.has_traversed(id)
            })
            .collect::<Vec<_>>(); // TODO: find optimal order of candidates
        fix::antichain_successor(function, cycle_base.id(), dsucc_idx, swap_candidates);
    }
    Ok(())
}

mod fix {
    use crate::{graph, term, BasicBlock, BlockId, Function};

    /// Changes the graph so the block with `id` no longer has a direct edge to its default
    /// successor in the _reduced graph_. This will only alter the given block, or insert new
    /// blocks. It is guaranteed that no other blocks are modified or removed.
    pub fn antichain_successor(
        function: &mut Function,
        id: BlockId,
        succ: graph::SuccIdx,
        swap_candidate_succ_idxs: impl IntoIterator<Item = graph::SuccIdx>,
    ) {
        try_antichain_successor_by_swap(&mut function[id], succ, swap_candidate_succ_idxs)
            .unwrap_or_else(|_| antichain_successor_by_indirection(function, id, succ))
    }

    pub fn try_antichain_successor_by_swap(
        block: &mut BasicBlock,
        succ: graph::SuccIdx,
        candidate_succ_idxs: impl IntoIterator<Item = graph::SuccIdx>,
    ) -> Result<(), ()> {
        for candidate in candidate_succ_idxs {
            match block.try_swap_succs(succ, candidate) {
                Ok(_) => return Ok(()),
                Err(_) => continue,
            }
        }
        Err(())
    }

    pub fn antichain_successor_by_indirection(
        function: &mut Function,
        id: BlockId,
        succ: graph::SuccIdx,
    ) {
        let old_bref = function[id].succ_bref(succ).clone();
        // Create the intermediary block that will become the new default successor of the given
        // block, and will jump to its old default successor.
        let ib_builder = function.start_new_block(function[old_bref.label.id()].arguments.clone());
        // Set the intermediary block as the new default successor of the given block.
        function[id].succ_bref_mut(succ).label = ib_builder.label().clone();
        // Make the intermediary block jump to the old default successors of the given block.
        let ib = ib_builder.terminate(term::jump(old_bref));
        function.add_block(ib);
    }
}

mod check {
    use crate::{Function, Instruction, PseudoInstruction, Terminator};

    pub fn is_graph_complete(function: &mut Function) -> bool {
        function.traverse().all(|b| {
            b.succidxs()
                .map(|idx| b.succ_id(idx))
                .all(|id| function.has_block(id))
        })
    }

    pub fn function_contains_virtuals(function: &mut Function) -> bool {
        function.blocks().any(|block| {
            block.instructions.iter().any(instruction_contains_virtuals)
                || terminator_contains_virtuals(&block.terminator)
        })
    }

    pub fn instruction_contains_virtuals(instr: &Instruction) -> bool {
        match instr {
            Instruction::Nop => false,
            Instruction::Reg3(_, rd, rs, rt) => {
                rd.is_virtual() || rs.is_virtual() || rt.is_virtual()
            }
            Instruction::Reg2(_, rd, rs) => rd.is_virtual() || rs.is_virtual(),
            Instruction::Reg1(_, rd) => rd.is_virtual(),
            Instruction::Imm2(_, rt, rs, _) => rt.is_virtual() || rs.is_virtual(),
            Instruction::Imm1(_, rt, _) => rt.is_virtual(),
            Instruction::FReg3(_, fd, fs, ft) => {
                fd.is_virtual() || fs.is_virtual() || ft.is_virtual()
            }
            Instruction::FReg2(_, fd, fs) => fd.is_virtual() || fs.is_virtual(),
            Instruction::FImm(_, ft, base, _) => ft.is_virtual() || base.is_virtual(),
            Instruction::MoveFromFpu(rt, fs) => rt.is_virtual() || fs.is_virtual(),
            Instruction::MoveToFpu(rt, fs) => rt.is_virtual() || fs.is_virtual(),
            Instruction::Syscall => false,
            Instruction::Break => false,
            Instruction::Pseudo(pseudo) => match pseudo {
                PseudoInstruction::LoadAddress(rt, _) => rt.is_virtual(),
            },
            Instruction::Virtual(_) => true,
        }
    }

    pub fn terminator_contains_virtuals(term: &Terminator) -> bool {
        match term {
            Terminator::BranchIf(_, rs, rt, _, _) => rs.is_virtual() || rt.is_virtual(),
            Terminator::BranchIfZ(_, rs, _, _) => rs.is_virtual(),
            Terminator::BranchIfZAndLink(_, rs, _, _) => rs.is_virtual(),
            Terminator::BranchIfFCond(_, _, _) => false,
            Terminator::Jump(_) => false,
            Terminator::JumpAndLink(_, _) => false,
            Terminator::ReturnToRa => false,
            Terminator::JumpAndLinkRa(rs, _) => rs.is_virtual(),
            Terminator::JumpAndLinkReg(rd, rs, _) => rd.is_virtual() || rs.is_virtual(),
            Terminator::Virtual(_) => true,
        }
    }
}
