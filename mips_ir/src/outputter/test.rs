use super::*;
use crate::{DataDirective, Function, GlobalData, Label};

#[test]
pub fn outputs_empty_root() {
    let root = Root::new();
    let mut output = String::new();
    MipsOutputter::new(&mut output).write_root(&root).unwrap();
    assert_eq!("", output);
}

#[test]
pub fn outputs_global_data() {
    let mut root = Root::new();
    root.add_data(GlobalData::new(
        "foobar".into(),
        DataDirective::AsciiZ("Lorem ipsum dolor sit amet.".as_bytes().to_vec()),
    ));
    let mut output = String::new();
    MipsOutputter::new(&mut output).write_root(&root).unwrap();
    assert_eq!(
        "	.data

foobar:
	.asciiz	\"Lorem ipsum dolor sit amet.\"
",
        output
    );
}

#[test]
pub fn outputs_exported_labels() {
    let mut root = Root::new();
    let label = Label::from("foobar");
    root.add_data(GlobalData::new(label.clone(), DataDirective::Space(1)));
    root.export_label(label);
    let mut output = String::new();
    MipsOutputter::new(&mut output).write_root(&root).unwrap();
    assert_eq!(
        "	.globl	foobar
	.data

foobar:
	.space	1
",
        output
    );
}

#[test]
pub fn outputs_empty_functions() {
    let mut root = Root::new();
    root.add_function(Function::new("main".into()));
    let mut output = String::new();
    MipsOutputter::new(&mut output).write_root(&root).unwrap();
    assert_eq!(
        "	.text

main:
",
        output
    );
}

#[test]
pub fn outputs_virtual_registers() {
    let mut function = Function::new("main".into());
    let mut builder = function.start_new_block(Vec::new());
    builder.add_instruction(crate::instr::add_u(
        Reg::Virtual(3),
        Reg::ZERO,
        Reg::Virtual(1),
    ));
    builder.add_instruction(crate::instr::convert(
        crate::FFmt::D,
        crate::FFmt::S,
        FReg::VirtualDouble(3),
        FReg::VirtualSingle(34),
    ));
    let block = builder.terminate(crate::term::return_to_ra());
    let id = function.add_block(block).id();
    function.set_entry_block(id);

    let mut root = Root::new();
    root.add_function(function);

    let mut output = String::new();
    MipsOutputter::new(&mut output)
        .with_config(MipsOutputConfig {
            allow_virtual_registers: true,
            ..Default::default()
        })
        .write_root(&root)
        .unwrap();
    assert_eq!(
        "	.text

main:
	addu	@3, $0, @1
	cvt.d.s	@fd3, @fs34
	jr	$31
",
        output
    );
}

#[test]
pub fn outputs_register_names() {
    let mut function = Function::new("main".into());
    let mut builder = function.start_new_block(Vec::new());
    builder.add_instruction(crate::instr::add_u(Reg::SP, Reg::ZERO, Reg::FP));
    builder.add_instruction(crate::instr::sub_u(Reg::T9, Reg::A0, Reg::V1));
    let block = builder.terminate(crate::term::return_to_ra());
    let id = function.add_block(block).id();
    function.set_entry_block(id);

    let mut root = Root::new();
    root.add_function(function);

    let mut output = String::new();
    MipsOutputter::new(&mut output)
        .with_config(MipsOutputConfig {
            use_register_names: true,
            ..Default::default()
        })
        .write_root(&root)
        .unwrap();
    assert_eq!(
        "	.text

main:
	addu	$sp, $zero, $fp
	subu	$t9, $a0, $v1
	jr	$ra
",
        output
    );
}

#[test]
pub fn outputs_block_arguments() {
    let mut function = Function::new("main".into());

    let true_target = function.create_block_label();
    let false_target = function.create_block_label();

    let entry_builder = function.start_new_block(Vec::new());
    let entry_block = entry_builder.terminate(crate::term::branch_if(
        crate::BCond::Eq,
        Reg::A0,
        Reg::A1,
        BlockRef::new(true_target.clone(), vec![Reg::A2.into()]),
        BlockRef::new(false_target.clone(), vec![Reg::A3.into(), Reg::T0.into()]),
    ));
    let entry_id = function.add_block(entry_block).id();
    function.set_entry_block(entry_id);

    let mut true_builder = function.start_block(true_target, vec![Reg::A2.into()]);
    true_builder.add_instruction(crate::instr::nop());
    let true_block = true_builder.terminate(crate::term::return_to_ra());
    function.add_block(true_block);

    let mut false_builder =
        function.start_block(false_target, vec![Reg::A3.into(), Reg::T0.into()]);
    false_builder.add_instruction(crate::instr::nop());
    let false_block = false_builder.terminate(crate::term::return_to_ra());
    function.add_block(false_block);

    let mut root = Root::new();
    root.add_function(function);

    let mut output = String::new();
    MipsOutputter::new(&mut output)
        .with_config(MipsOutputConfig {
            use_register_names: true,
            show_block_arguments: true,
            ..Default::default()
        })
        .write_root(&root)
        .unwrap();

    assert_eq!(
        "	.text

main:
	beq	$a0, $a1, $main.bb0[$a2], $main.bb1[$a3, $t0]
$main.bb1[$a3, $t0]:
	nop
	jr	$ra
$main.bb0[$a2]:
	nop
	jr	$ra
",
        output
    );
}
