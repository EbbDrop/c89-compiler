use super::coloring::Coloring;
use crate::cfg::BasicBlock;
use crate::{AnyReg, FReg, Function, Instruction, Reg};
use std::collections::HashMap;

pub fn apply_coloring(function: &mut Function, coloring: &Coloring) {
    for (_, block) in function.cfg.blocks_mut() {
        apply_coloring_to_block(block, coloring);
    }
    for block_id in function.cfg.blocks().map(|(id, _)| id).collect::<Vec<_>>() {
        let succ_ids: Vec<_> = function.cfg[block_id]
            .successors()
            .map(|bref| bref.id)
            .collect();
        for succ_id in succ_ids {
            let args = &function.cfg[block_id].successor(succ_id).unwrap().arguments;
            let params = &function.cfg[succ_id].arguments;
            let perm_instrs = perm_color(params, args);
            if !perm_instrs.is_empty() {
                function
                    .cfg
                    .insert_on_edge(block_id, succ_id, perm_instrs.as_slice());
            }
        }
    }
    for (_, block) in function.cfg.blocks_mut() {
        for bref in block.successors_mut() {
            bref.arguments.clear();
        }
        block.arguments.clear();
    }
}

fn apply_coloring_to_block(block: &mut BasicBlock, coloring: &Coloring) {
    for arg in &mut block.arguments {
        if arg.is_virtual() {
            if let Some(reg) = reg_to_color(*arg, coloring) {
                *arg = reg;
            } else {
                // This is an unused argument. Normally should have been removed by now by the
                // purging step before coloring. However, in the case of call blocks, there might
                // be extra arguments added that are now unused. We can just leave them in, since
                // they will be skipped over by the `perm_color`.
            }
        }
    }
    for instr in &mut block.instructions {
        instr
            .as_unhidden_mut()
            .map_uses(|reg| match reg.is_virtual() {
                true => {
                    reg_to_color(reg, coloring).unwrap_or_else(|| panic!("missing color for {reg}"))
                }
                false => reg,
            });
        instr
            .as_unhidden_mut()
            .map_defs(|reg| match reg.is_virtual() {
                true => match reg_to_color(reg, coloring) {
                    Some(reg) => reg,
                    // This can only be the case if the instruction defines a virtual register that
                    // is never used (and thus never assigned a color). The optimizer should
                    // optimize this away.
                    None => match reg {
                        AnyReg::R(_) => Reg::ZERO.into(),
                        AnyReg::F(_) => {
                            // This can only happen if there would exist an instruction with a
                            // side-effect that stores its result in a floating-point register.
                            // `ldc1` would be such an example...
                            // FIXME: let the optimizer optimize these things away
                            panic!("missing color for unused def of virtual fpu register")
                        }
                    },
                },
                false => reg,
            });
    }
    block
        .terminator_mut()
        .map_uses(|reg| match reg.is_virtual() {
            true => {
                reg_to_color(reg, coloring).unwrap_or_else(|| panic!("missing color for {reg}"))
            }
            false => reg,
        });
    for arg in block.successors_mut().flat_map(|bref| &mut bref.arguments) {
        if arg.is_virtual() {
            if let Some(reg) = reg_to_color(*arg, coloring) {
                *arg = reg;
            } else {
                // This is an unused argument. Normally should have been removed by now by the
                // purging step before coloring. However, in the case of call blocks, there might
                // be extra arguments added that are now unused. We can just leave them in, since
                // they will be skipped over by the `perm_color`.
            }
        }
    }
}

fn reg_to_color(reg: AnyReg, coloring: &Coloring) -> Option<AnyReg> {
    coloring.coloring.get(&reg).map(|color| color.physical_reg)
}

/// Generates instructions to permutate the `args` vector to the `params` vector.
fn perm_color(params: &[AnyReg], args: &[AnyReg]) -> Vec<Instruction> {
    // Maps each param to its arg
    let mut mapping = HashMap::new();
    for (&arg, &param) in args.iter().zip(params) {
        match (arg.is_virtual(), param.is_virtual()) {
            // argument and param are unused, would have been colored otherwise
            (true, true) => continue,
            (true, false) => {
                panic!("phi argument doesn't have a color, while param does (color {param:#})")
            }
            // phi argument has a color, while param doesn't
            (false, true) => {
                continue;
            }
            (false, false) => {}
        }
        if arg != param {
            mapping.insert(param, arg);
        }
    }
    let mut instructions = Vec::new();
    while let Some((&param, &arg)) = mapping.iter().next() {
        mapping.remove(&param);
        if param == arg {
            continue;
        }
        let mut swap = false;
        for a in mapping.values_mut() {
            if *a == param {
                *a = arg;
                swap = true;
            }
        }
        match swap {
            true => match (param, arg) {
                (AnyReg::R(p), AnyReg::R(a)) => {
                    instructions.push(crate::instr::xor(p, p, a));
                    instructions.push(crate::instr::xor(a, p, a));
                    instructions.push(crate::instr::xor(p, p, a));
                }
                (AnyReg::F(p), AnyReg::F(a)) => {
                    instructions.push(crate::instr::move_(a.ffmt(), FReg::F(2), a));
                    instructions.push(crate::instr::move_(p.ffmt(), a, p));
                    instructions.push(crate::instr::move_(p.ffmt(), p, FReg::F(2)));
                }
                (AnyReg::R(_), AnyReg::F(_)) | (AnyReg::F(_), AnyReg::R(_)) => {
                    unreachable!()
                }
            },
            false => instructions.push(crate::instr::virt::move_(param, arg).to_hidden()),
        }
    }
    instructions
}
