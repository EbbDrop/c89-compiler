use crate::{Function, Label, Reg, Root};

pub fn link(root: &mut Root) -> Result<(), String> {
    let label_main = Label::from("main");
    if !root.exports_label(&label_main) {
        return Err("no main symbol exported".to_owned());
    }
    match root.function_mut(&label_main) {
        Some(func) => link_main(func),
        None => return Err("main is not a function".to_owned()),
    }
    if root.is_external(&"printf".into()) {
        link_printf(root);
    }
    if root.is_external(&"scanf".into()) {
        link_scanf(root);
    }
    Ok(())
}

fn link_main(main: &mut Function) {
    // Make sure the stack is 8-byte aligned before entering main.
    let entry_block = main.cfg.entry_block_mut();
    let mut new_entry_instructions = Vec::with_capacity(entry_block.instructions.len() + 4);
    new_entry_instructions.push(crate::instr::comment(
        " Make sure the stack is 8-byte aligned as required by the ABI".to_owned(),
    ));
    new_entry_instructions.push(crate::instr::add_u_imm(Reg::SP, Reg::SP, 0b111));
    new_entry_instructions.push(crate::instr::shift_right_logical_imm(Reg::SP, Reg::SP, 3));
    new_entry_instructions.push(crate::instr::shift_left_logical_imm(Reg::SP, Reg::SP, 3));
    new_entry_instructions.push(crate::instr::comment(
        " Reserve stack space for arg0 to arg3 as required by the ABI".to_owned(),
    ));
    new_entry_instructions.push(crate::instr::add_u_imm(Reg::SP, Reg::SP, (-16i16) as u16));
    new_entry_instructions.push(crate::instr::comment("#".repeat(63)));
    new_entry_instructions.append(&mut entry_block.instructions);
    entry_block.instructions = new_entry_instructions;

    // Exit the program after main. Note that exit_block may be the same as entry_block.
    if let Some(exit_block) = main.exit_block_id.map(|id| &mut main.cfg[id]) {
        exit_block
            .instructions
            .push(crate::instr::comment("#".repeat(63)));
        exit_block
            .instructions
            .push(crate::instr::or(Reg::A0, Reg::V0, Reg::ZERO));
        exit_block
            .instructions
            .push(crate::instr::or_imm(Reg::V0, Reg::ZERO, 17));
        *exit_block.terminator_mut() = crate::term::syscall(None);
    }
}

fn link_printf(root: &mut Root) {
    root.add_raw_text(include_str!("printf.asm").to_owned());
}

fn link_scanf(root: &mut Root) {
    root.add_raw_text(include_str!("scanf.asm").to_owned());
}
