//! Utility methods that scan all instructions and terminators in a function for certain things.
use crate::{
    cfg::Cfg,
    dfa::uda::{GlobalLocation, Location},
    AnyReg, Function, FunctionCall, Instruction, VirtualInstruction,
};
use std::collections::HashSet;

pub mod function {
    use super::*;

    pub fn function_calls(function: &Function) -> impl Iterator<Item = &FunctionCall> {
        function
            .cfg
            .blocks()
            .flat_map(|(_, b)| bb::function_calls(b))
    }

    pub fn physical_defs(function: &Function) -> HashSet<AnyReg> {
        let mut physical_defs = HashSet::new();
        for (_, block) in function.cfg.blocks() {
            physical_defs.extend(bb::physical_defs(block));
        }
        physical_defs
    }

    pub fn declares(cfg: &Cfg) -> impl Iterator<Item = GlobalLocation> + '_ {
        cfg.blocks().flat_map(|(id, block)| {
            bb::declares(block).map(move |loc| GlobalLocation::new(id, loc))
        })
    }
}

pub mod bb {
    use crate::cfg::BasicBlock;

    use super::*;

    pub fn function_calls(block: &BasicBlock) -> impl Iterator<Item = &FunctionCall> {
        block.instructions.iter().filter_map(|instr| match instr {
            Instruction::Virtual(VirtualInstruction::FunctionCall(call)) => Some(call),
            _ => None,
        })
    }

    /// Returns all physical registers defined in this block (excluding phi defs). Also includes
    /// defs by hidden instructions.
    pub fn physical_defs(block: &BasicBlock) -> HashSet<AnyReg> {
        let mut physical_defs = HashSet::new();
        for instr in &block.instructions {
            physical_defs.extend(instr.as_unhidden().defs().filter(|r| !r.is_virtual()));
        }
        physical_defs
    }

    /// Returns a mutable reference to every declare instruction in this block. Also includes
    /// declares that are hidden.
    pub fn declares(block: &BasicBlock) -> impl Iterator<Item = Location> + '_ {
        block
            .instructions
            .iter()
            .enumerate()
            .filter_map(|(i, instr)| match instr.as_unhidden() {
                Instruction::Virtual(VirtualInstruction::Declare(_)) => Some(Location(i as isize)),
                _ => None,
            })
    }
}
