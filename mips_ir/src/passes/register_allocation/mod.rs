//! Register Allocation and SSA destruction
//!
//! See https://doi.org/10.1007/11688839_20 for the paper on which this implementation is mainly
//! based. For the pdf version, see https://compilers.cs.uni-saarland.de/papers/ra_ssa.pdf.

mod call_isolation;
mod coalescing;
mod coloring;
mod perm_optimizer;
mod spilling;
mod ssa_destruction;
pub(super) mod util;

use crate::{dfa, FReg, Reg};

const SAVED_CPU_REGS: [Reg; 8] = [
    Reg::S0,
    Reg::S1,
    Reg::S2,
    Reg::S3,
    Reg::S4,
    Reg::S5,
    Reg::S6,
    Reg::S7,
];
const TEMP_CPU_REGS: [Reg; 10] = [
    Reg::T0,
    Reg::T1,
    Reg::T2,
    Reg::T3,
    Reg::T4,
    Reg::T5,
    Reg::T6,
    Reg::T7,
    Reg::T8,
    Reg::T9,
];
const N_SAVED_CPU_REGS: usize = SAVED_CPU_REGS.len();
const N_TEMP_CPU_REGS: usize = TEMP_CPU_REGS.len();
const N_CPU_REGS: usize = N_SAVED_CPU_REGS + N_TEMP_CPU_REGS;

const SAVED_FPU_D_REGS: [FReg; 6] = [
    FReg::F(20),
    FReg::F(22),
    FReg::F(24),
    FReg::F(26),
    FReg::F(28),
    FReg::F(30),
];
const TEMP_FPU_D_REGS: [FReg; 6] = [
    FReg::F(4),
    FReg::F(6),
    FReg::F(8),
    FReg::F(10),
    FReg::F(16),
    FReg::F(18),
];
const N_SAVED_FPU_D_REGS: usize = SAVED_FPU_D_REGS.len();
const N_TEMP_FPU_D_REGS: usize = TEMP_FPU_D_REGS.len();
const N_FPU_D_REGS: usize = N_SAVED_FPU_D_REGS + N_TEMP_FPU_D_REGS;

pub fn run(root: &mut crate::Root) {
    root.functions_mut().for_each(run_function)
}

fn run_function(function: &mut crate::Function) {
    call_isolation::isolate_calls(function);

    spilling::spill_belady(function);

    // Removing unused (phi) defs is required for the following passes to work
    dfa::dce::purge_function(function);

    // Because spilling might have added loads to the call blocks, and purging removed unused phi
    // arguments, we need to regenerate the call blocks.
    for (_, block) in function.cfg.blocks_mut() {
        block.is_call_block = false;
    }
    call_isolation::isolate_calls(function);

    let (coloring, conflict_graph) = coloring::color(&function.cfg);
    let coalesed_color = coalescing::coales(&function.cfg, &coloring, &conflict_graph);

    ssa_destruction::apply_coloring(function, &coalesed_color);
}
