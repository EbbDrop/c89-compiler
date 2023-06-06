use crate::{
    cfg::{BasicBlock, BlockId, BlockRef},
    dfa::uda::{GlobalLocation, Location},
    Function, FunctionCall, Instruction, Root, VirtualInstruction,
};

use super::register_allocation::util::alias_reg_from;

pub fn run(root: &mut Root) {
    for function in root.functions_mut() {
        prepare_function(function);
    }
}

/// Prepare this function for further passes s.a. register allocation
fn prepare_function(function: &mut Function) {
    isolate_calls(function);
}

pub(crate) fn isolate_calls(function: &mut Function) {
    let block_ids: Vec<_> = function.cfg.blocks().map(|(id, _)| id).collect();
    for block_id in block_ids {
        isolate_calls_in_block(function, block_id);
    }
}

fn isolate_calls_in_block(function: &mut Function, mut block_id: BlockId) {
    'outer: loop {
        for (idx, instr) in function.cfg[block_id].instructions.iter().enumerate() {
            // TODO: what about syscalls?
            match instr {
                Instruction::Call(_)
                | Instruction::Virtual(VirtualInstruction::FunctionCall(FunctionCall { .. })) => {
                    let (_, succ_id) = extract_call_to_block(
                        function,
                        GlobalLocation::new(block_id, Location(idx as isize)),
                    );
                    block_id = succ_id;
                    continue 'outer;
                }
                _ => {}
            }
        }
        return;
    }
}

/// Returns the (connector id, successor id) pair. The id of the predecessor is the same as given
/// by `location.block`.
fn extract_call_to_block(function: &mut Function, location: GlobalLocation) -> (BlockId, BlockId) {
    let GlobalLocation { block_id, local } = location;

    if location.local.0 == function.cfg[block_id].instructions.len() as isize {
        panic!("won't extract terminator from block");
    }

    let mut live_accross_call = Vec::new();
    let du_chains = function.cfg.du_chains();
    let live_set = function.cfg.live_set(block_id);
    for reg in du_chains.defined_regs() {
        let du_chain = &du_chains[reg];

        let def_not_in_block = du_chain.def_block() != block_id;

        let def_in_block_before_call =
            du_chain.def_block() == block_id && du_chain.def_location().as_location() < local;

        let use_in_block_after_call = du_chain.uses_in(block_id).any(|loc| local < loc);

        if (def_not_in_block || def_in_block_before_call)
            && (use_in_block_after_call || live_set.live_outs.contains(&reg))
        {
            live_accross_call.push(reg);
        }
    }

    let succ_instructions = function.cfg[block_id]
        .instructions
        .split_off(local.0 as usize + 1);
    let instruction_to_extract = function.cfg[block_id].instructions.pop().unwrap();
    let original_terminator = function.cfg[block_id].terminator.take();

    let succ_id = function.cfg.insert(BasicBlock {
        arguments: Vec::new(),
        instructions: succ_instructions,
        terminator: original_terminator,
        is_call_block: false,
    });

    let connector_id = function.cfg.insert(BasicBlock {
        arguments: Vec::new(),
        instructions: vec![instruction_to_extract],
        terminator: Some(crate::term::jump(BlockRef::new(succ_id, Vec::new()))),
        is_call_block: true,
    });

    function.cfg[block_id].terminator =
        Some(crate::term::jump(BlockRef::new(connector_id, Vec::new())));

    for &reg in &live_accross_call {
        let (alias, _aliases) = alias_reg_from(
            &mut function.cfg,
            reg,
            GlobalLocation::new(connector_id, Location(0)),
        );
        function.cfg[connector_id].arguments.push(alias);
        function.cfg[block_id]
            .successor_mut(connector_id)
            .unwrap()
            .arguments
            .push(reg);
    }

    (connector_id, succ_id)
}
