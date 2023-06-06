use crate::{scanner, AnyReg, Function, Reg, Root};

pub fn run(root: &mut Root) {
    for function in root.functions_mut() {
        build_stack_frame(function);
    }
}

fn build_stack_frame(function: &mut Function) {
    generate_stack_frame(function);
    construct_stack_frame(function);
    destruct_stack_frame(function);
}

fn generate_stack_frame(function: &mut Function) {
    // params should already have been added to the stack frame when creating the function
    let mut spilled = Vec::new();
    for &stack_address in function.non_param_stack_addresses() {
        spilled.push((stack_address, function.stack_info(stack_address).clone()));
    }
    for (stack_address, stack_info) in spilled {
        function
            .stack_frame
            .add_spilled(stack_info, std::iter::once(stack_address));
    }

    // Retrieve all used callee-saved cpu and fpu registers and add them to the stack frame.
    for reg in scanner::function::physical_defs(function) {
        match reg {
            AnyReg::R(Reg::ZERO) => {}
            AnyReg::R(r) if r.is_saved() => function.stack_frame.add_saved_cpu_reg(r),
            AnyReg::F(r) if r.is_saved() => function.stack_frame.add_saved_fpu_reg(r),
            _ => {}
        }
    }

    // Find out the max space needed for the arguments of any call within this function.
    let max_call_arg_space = scanner::function::function_calls(function)
        .map(|call| {
            let mut size = 0;
            for (_, stack_info) in &call.arguments {
                size = stack_info.alignment.next_multiple_from(size);
                size += stack_info.size as u32;
            }
            size
        })
        .max()
        .unwrap_or(0);
    function
        .stack_frame
        .set_max_call_argument_space(max_call_arg_space);
}

fn construct_stack_frame(function: &mut Function) {
    let mut instructions = Vec::new();

    // Allocate the stack frame: set the stack pointer.
    let stack_frame_size = function.stack_frame.byte_size();
    instructions.push(crate::instr::add_u_imm(
        Reg::SP,
        Reg::SP,
        (-(stack_frame_size as i16)) as u16,
    ));

    // Save the return address.
    let (base, offset) = function.stack_frame.ra_slot_addr();
    instructions.push(crate::instr::store_word(Reg::RA, base, offset));

    // Save the frame pointer.
    let (base, offset) = function.stack_frame.fp_slot_addr();
    instructions.push(crate::instr::store_word(Reg::FP, base, offset));

    // Save used callee-saved cpu registers
    for reg in function.stack_frame.saved_cpu_regs().rev() {
        let (base, offset) = function.stack_frame.saved_cpu_reg_addr(reg).unwrap();
        instructions.push(crate::instr::store_word(reg, base, offset));
    }

    // Save used callee-saved fpu registers
    for freg in function.stack_frame.saved_fpu_regs().rev() {
        let (base, offset) = function.stack_frame.saved_fpu_reg_addr(freg).unwrap();
        instructions.push(crate::instr::store_doubleword_from_fpu(freg, base, offset));
    }

    // Store the stack pointer.
    instructions.push(crate::instr::virt::move_(Reg::FP.into(), Reg::SP.into()));

    let entry_block = function.cfg.entry_block_mut();
    instructions.extend_from_slice(&entry_block.instructions);
    entry_block.instructions = instructions;

    function.stack_frame.mark_fp_as_set();
}

fn destruct_stack_frame(function: &mut Function) {
    let mut builder = function.start_new_block(Vec::new());

    // Restore the stack pointer.
    builder.add_instruction(crate::instr::virt::move_(Reg::SP.into(), Reg::FP.into()));

    function.stack_frame.mark_fp_as_unset();

    // Restore the saved fpu registers
    for freg in function.stack_frame.saved_fpu_regs() {
        let (base, offset) = function.stack_frame.saved_fpu_reg_addr(freg).unwrap();
        builder.add_instruction(crate::instr::load_doubleword_to_fpu(freg, base, offset));
    }

    // Restore the saved cpu registers
    for reg in function.stack_frame.saved_cpu_regs() {
        let (base, offset) = function.stack_frame.saved_cpu_reg_addr(reg).unwrap();
        builder.add_instruction(crate::instr::load_word(reg, base, offset));
    }

    // Restore the saved frame pointer.
    let (base, offset) = function.stack_frame.fp_slot_addr();
    builder.add_instruction(crate::instr::load_word(Reg::FP, base, offset));

    // Restore the saved return address.
    let (base, offset) = function.stack_frame.ra_slot_addr();
    builder.add_instruction(crate::instr::load_word(Reg::RA, base, offset));

    // Deallocate the stack frame: reset the stack pointer.
    let stack_frame_size = function.stack_frame.byte_size();
    builder.add_instruction(crate::instr::add_u_imm(Reg::SP, Reg::SP, stack_frame_size));

    // Jump to $ra.
    let exit_block = builder.terminate(crate::term::return_to_ra());
    let exit_block_id = exit_block.id();
    function.add_block(exit_block);
    function.exit_block_id = Some(exit_block_id);
}
