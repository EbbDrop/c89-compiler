// In this file, the term "reduced graph" is used to denote the graph formed by only preserving the
// edges formed by default successors/predecessor relations (and dropping the nodes/blocks that no
// longer have edges in this graph).
//
// This graph is extensively used, because one of the constraints that a fixed function should meet
// is that the _reduced graph_ is acyclic (a DAG).

// #[cfg(test)]
// mod test;

use crate::{Function, Root};

/// Checks if the root matches all the constraints required to output valid MIPS, and applies fixes
/// to make it valid if not. See [`crate::MipsOutputter`] for the list of constraints.
///
/// If it's not possible to meet all the constraints, an `Err` result will be returned containing
/// the first unfixable problem that was encountered. See [`UnfixableProblem`] for the list of
/// problems that are not fixable.
///
/// Note that this may fix more than strictly required to meet the [`crate::MipsOutputter`]
/// constraints.
pub fn fix_root(root: &mut Root) {
    for function in root.functions_mut() {
        fix_function(function);
    }
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
pub fn fix_function(function: &mut Function) {
    // if check::function_contains_virtuals(function) {
    //     panic!("cannot fix function containing virtuals");
    // }
    // if !check::is_graph_complete(function) {
    //     panic!("cannot fix incomplete graph");
    // }
    // Break any possible cycles.
    'outer: loop {
        // Traverse the function starting from the entry point and following default successors
        // whenever possible. If we encounter a block while traversing of which the default
        // has already been traversed, we know that that block is part of a cycle in the reduced
        // graph.
        let mut traverser = function.traverse_all();

        // `cycle_base` will be the block of which we'll antichain the default successor to break
        // the cycle. If no cycle is found, we can successfully break from 'outer.
        let (cycle_base_id, dsucc_id) = loop {
            let Some((block_id, block)) = traverser.next() else { break 'outer };
            if let Some(bref) = block.terminator().default_target() {
                if traverser.has_traversed(bref.id) {
                    break (block_id, bref.id);
                }
            }
        };
        // Found a cycle, so fix it by either swapping the default successor with a
        // non-default successor, or by introducing indirection.
        let swap_candidates = function.cfg[cycle_base_id]
            .terminator()
            .targets()
            .filter(|bref| bref.id != dsucc_id && !traverser.has_traversed(bref.id))
            .map(|bref| bref.id)
            .collect::<Vec<_>>(); // TODO: find optimal order of candidates
        fix::antichain_successor(function, cycle_base_id, dsucc_id, swap_candidates);
    }
}

mod fix {
    use crate::{
        cfg::{BasicBlock, BlockId, BlockRef},
        Function,
    };

    /// Changes the graph so the block with `id` no longer has a direct edge to its default
    /// successor in the _reduced graph_. This will only alter the given block, or insert new
    /// blocks. It is guaranteed that no other blocks are modified or removed.
    pub fn antichain_successor(
        function: &mut Function,
        id: BlockId,
        succ_id: BlockId,
        succ_swap_candidates: impl IntoIterator<Item = BlockId>,
    ) {
        try_antichain_successor_by_swap(&mut function.cfg[id], succ_id, succ_swap_candidates)
            .unwrap_or_else(|| antichain_successor_by_indirection(function, id, succ_id))
    }

    pub fn try_antichain_successor_by_swap(
        block: &mut BasicBlock,
        succ_id: BlockId,
        succ_candidates: impl IntoIterator<Item = BlockId>,
    ) -> Option<()> {
        for candidate in succ_candidates {
            match block.terminator_mut().try_swap_targets(succ_id, candidate) {
                Ok(_) => return Some(()),
                Err(_) => continue,
            }
        }
        None
    }

    pub fn antichain_successor_by_indirection(
        function: &mut Function,
        id: BlockId,
        succ_id: BlockId,
    ) {
        let mut var_generator = function.cfg.var_generator();
        let bref = function.cfg[id].successor(succ_id).unwrap();
        let connector_args: Vec<_> = bref
            .arguments
            .iter()
            .map(|&r| var_generator.next_of_type(r))
            .collect();
        let connector_id = function.cfg.insert(BasicBlock {
            arguments: connector_args.clone(),
            instructions: Vec::new(),
            terminator: Some(crate::term::jump(BlockRef::new(succ_id, connector_args))),
            is_call_block: false,
        });
        function.cfg[id].successor_mut(succ_id).unwrap().id = connector_id;
    }
}

// mod check {
//     use crate::{Function, Instruction, PseudoInstruction, Terminator};

//     pub fn is_graph_complete(function: &mut Function) -> bool {
//         function.traverse().all(|b| {
//             b.succidxs()
//                 .map(|idx| b.succ_id(idx))
//                 .all(|id| function.has_block(id))
//         })
//     }

//     pub fn function_contains_virtuals(function: &mut Function) -> bool {
//         function.blocks().any(|block| {
//             block.instructions.iter().any(instruction_contains_virtuals)
//                 || terminator_contains_virtuals(&block.terminator)
//         })
//     }

//     pub fn instruction_contains_virtuals(instr: &Instruction) -> bool {
//         match instr {
//             Instruction::Nop => false,
//             Instruction::Reg3(_, rd, rs, rt) => {
//                 rd.is_virtual() || rs.is_virtual() || rt.is_virtual()
//             }
//             Instruction::Reg2(_, rd, rs) => rd.is_virtual() || rs.is_virtual(),
//             Instruction::Reg1(_, rd) => rd.is_virtual(),
//             Instruction::Imm2(_, rt, rs, _) => rt.is_virtual() || rs.is_virtual(),
//             Instruction::Imm1(_, rt, _) => rt.is_virtual(),
//             Instruction::FReg3(_, fd, fs, ft) => {
//                 fd.is_virtual() || fs.is_virtual() || ft.is_virtual()
//             }
//             Instruction::FReg2(_, fd, fs) => fd.is_virtual() || fs.is_virtual(),
//             Instruction::FImm(_, ft, base, _) => ft.is_virtual() || base.is_virtual(),
//             Instruction::MoveFromFpu(rt, fs) => rt.is_virtual() || fs.is_virtual(),
//             Instruction::MoveToFpu(rt, fs) => rt.is_virtual() || fs.is_virtual(),
//             Instruction::Syscall => false,
//             Instruction::Break => false,
//             Instruction::Pseudo(pseudo) => match pseudo {
//                 PseudoInstruction::LoadAddress(rt, _) => rt.is_virtual(),
//             },
//             Instruction::Virtual(_) => true,
//         }
//     }

//     pub fn terminator_contains_virtuals(term: &Terminator) -> bool {
//         match term {
//             Terminator::BranchIf(_, rs, rt, _, _) => rs.is_virtual() || rt.is_virtual(),
//             Terminator::BranchIfZ(_, rs, _, _) => rs.is_virtual(),
//             Terminator::BranchIfZAndLink(_, rs, _, _) => rs.is_virtual(),
//             Terminator::BranchIfFCond(_, _, _) => false,
//             Terminator::Jump(_) => false,
//             Terminator::JumpAndLink(_, _) => false,
//             Terminator::ReturnToRa => false,
//             Terminator::JumpAndLinkRa(rs, _) => rs.is_virtual(),
//             Terminator::JumpAndLinkReg(rd, rs, _) => rd.is_virtual() || rs.is_virtual(),
//             Terminator::Virtual(_) => true,
//         }
//     }
// }
