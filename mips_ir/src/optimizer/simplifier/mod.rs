use crate::{BlockId, Function, Instruction, Root, Terminator};
use std::collections::BTreeSet;

pub fn simplify_root(root: &mut Root) {
    root.functions_mut().for_each(simplify)
}

/// Simplifies the given function. Blocks with arguments might not be fully simplified. Might undo
/// isolation of call blocks.
///
/// Performs the following simplifications:
///
///  - Removal of `nop` instructions
///  - Merging blocks that are connected by unconditional jumps if possible
///  - ...
pub fn simplify(function: &mut Function) {
    merge_chains(function);
    remove_nops(function);
}

fn remove_nops(function: &mut Function) {
    for (_, block) in function.cfg.blocks_mut() {
        for i in (0..block.instructions.len()).rev() {
            if matches!(block.instructions[i].as_unhidden(), Instruction::Nop) {
                block.instructions.remove(i);
            }
        }
    }
}

fn merge_chains(function: &mut Function) {
    let mut visited: BTreeSet<BlockId> = BTreeSet::new();
    let mut to_visit: BTreeSet<BlockId> = BTreeSet::new();
    let mut next_block_id = Some(function.cfg.entry_block_id());
    while let Some(block_id) = next_block_id {
        match function.cfg[block_id].terminator() {
            Terminator::Jump(target) => {
                if !visited.contains(&target.id)
                    && function.cfg.n_predecessors(target.id) == 1
                    && target.arguments.is_empty()
                {
                    to_visit.remove(&target.id);
                    if function.exit_block_id == Some(target.id) {
                        function.exit_block_id = Some(block_id);
                    }
                    let mut succ = function.cfg.remove(target.id);
                    let block = &mut function.cfg[block_id];
                    block.is_call_block = (block.is_call_block && succ.instructions.is_empty())
                        || (succ.is_call_block && block.instructions.is_empty());
                    block.instructions.append(&mut succ.instructions);
                    block.terminator = succ.terminator;
                    // Recheck the same block, as it has a different terminator now.
                    continue;
                }
            }
            Terminator::BranchIf(_, _, _, _, _)
            | Terminator::BranchIfZ(_, _, _, _)
            | Terminator::BranchIfZAndLink(_, _, _, _)
            | Terminator::BranchIfFCond(_, _, _)
            | Terminator::ReturnToRa
            | Terminator::Syscall(_)
            | Terminator::JumpAndLinkRa(_, _)
            | Terminator::JumpAndLinkReg(_, _, _)
            | Terminator::Virtual(_) => {}
        }
        visited.insert(block_id);
        to_visit.extend(
            function
                .cfg
                .successor_ids(block_id)
                .filter(|id| !visited.contains(id)),
        );
        next_block_id = to_visit.pop_first();
    }
}
