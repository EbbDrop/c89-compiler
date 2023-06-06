//! Dead-Code Eliminiation. Removes all unused bb arguments, reg defs that are never read, functions
//! that are never referenced (and not external), etc.
//!
//! Note that global data items are left unchanged, because the code might depend on their order,
//! position, size, or alignment.
//!
//! Note that purging a function is required before doing register allocation!
use crate::{AnyReg, BlockId, Function, Reg, Root};
use std::collections::HashSet;

/// Assumes the root is validated.
pub fn purge_root(root: &mut Root) {
    root.functions_mut().for_each(purge_function);
}

/// Assumes the function is validated.
pub fn purge_function(function: &mut Function) {
    remove_unreachable_blocks(function);

    let mut to_eliminate = HashSet::new();

    let du_chains = function.cfg.du_chains();

    for reg in du_chains.defined_regs() {
        if !reg.is_virtual() {
            continue;
        }
        let du_chain = &du_chains[reg];
        if !du_chain.uses().any(|loc| {
            match function.cfg.instruction(loc) {
                Some(instr) => instr.has_side_effects(),
                None => true, // terminator is always a real use
            }
        }) {
            // This reg has no real uses
            to_eliminate.insert(reg);
        }
    }

    let mut changing = true;
    while changing {
        changing = false;
        for &reg in &to_eliminate {
            let du_chain = &du_chains[reg];
            if du_chain.uses().any(|loc| {
                let Some(instr) = function.cfg.instruction(loc) else { return true };
                instr.uses().any(|r| !to_eliminate.contains(&r))
                    || instr.defs().any(|r| !to_eliminate.contains(&r))
            }) || du_chain.phi_uses().any(|loc| {
                function.cfg[loc.block_id].successors().any(|bref| {
                    bref.arguments
                        .iter()
                        .zip(&function.cfg[bref.id].arguments)
                        .any(|(&arg, &param)| {
                            (arg == reg && !to_eliminate.contains(&param))
                                || (param == reg && !to_eliminate.contains(&arg))
                        })
                })
            }) {
                to_eliminate.remove(&reg);
                changing = true;
                break;
            }
        }
    }

    // Now remove all definitions and uses of registers not in `real_uses`.
    for block_id in function.cfg.blocks().map(|(id, _)| id).collect::<Vec<_>>() {
        eliminate_reg(function, block_id, &to_eliminate);
    }
}

fn remove_unreachable_blocks(function: &mut Function) {
    let reachables = HashSet::<_>::from_iter(function.traverse().map(|(id, _)| id));
    for &unreachable in
        HashSet::from_iter(function.cfg.blocks().map(|(id, _)| id)).difference(&reachables)
    {
        function.cfg.remove(unreachable);
    }
}

/// Eliminates all uses and defs of `reg` within `block`. Panics if a usage of `reg` couldn't be
/// removed (can happen when the usage is in an effectful instruction) or if `reg` is virtual.
/// Does not panic if the definition of `reg` could not be removed. Tries to replace it with $zero
/// if possible, or leaves it as is.
fn eliminate_reg(function: &mut Function, block_id: BlockId, to_eliminate: &HashSet<AnyReg>) {
    for i in (0..function.cfg[block_id].arguments.len()).rev() {
        if to_eliminate.contains(&function.cfg[block_id].arguments[i]) {
            function.cfg.remove_param(block_id, i);
        }
    }
    for i in (0..function.cfg[block_id].instructions.len()).rev() {
        let instr = &mut function.cfg[block_id].instructions[i];
        let mut should_remove = false;
        if instr.uses().any(|r| to_eliminate.contains(&r)) {
            if instr.has_side_effects() {
                panic!("cannot eliminate register used in an effectful instruction");
            }
            if instr
                .uses()
                .any(|r| r.is_virtual() && !to_eliminate.contains(&r))
            {
                panic!("cannot eliminate regiser used in an instruction that uses other virtual registers that must not be eliminated");
            }
            should_remove = true;
        }
        if instr.defs().any(|r| to_eliminate.contains(&r)) {
            if instr.has_side_effects() {
                instr.map_defs(|r| match r {
                    AnyReg::R(_) if to_eliminate.contains(&r) => Reg::ZERO.into(),
                    _ => r,
                });
            } else {
                should_remove = true;
            }
        }
        if should_remove {
            function.cfg[block_id].instructions.remove(i);
        }
    }
    if function.cfg[block_id]
        .terminator()
        .uses()
        .any(|r| to_eliminate.contains(&r))
    {
        panic!("cannot eliminate register used in a terminator");
    }
}
