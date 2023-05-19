use super::*;

#[test]
fn empty_functions() {
    let mut empty_cfg = Function::new("main".into());

    assert!(empty_cfg.is_empty());
    assert!(empty_cfg.entry_block().is_none());
    assert!(empty_cfg.blocks().next().is_none());
    assert!(empty_cfg.traverse().next().is_none());
    assert_eq!("main:\n", format!("{}", empty_cfg));

    let entry_id = empty_cfg.create_block_id();
    let block = empty_cfg
        .start_block(entry_id.clone(), Vec::new())
        .terminate(crate::term::return_to_ra());
    empty_cfg.add_block(block);

    assert!(empty_cfg.is_empty());
    assert!(empty_cfg.entry_block().is_none());
    {
        let mut blocks = empty_cfg.blocks();
        assert!(blocks.next().is_some());
        assert!(blocks.next().is_none());
    }
    assert!(empty_cfg.traverse().next().is_none());
    assert_eq!("main:\n", format!("{}", empty_cfg));

    empty_cfg.set_entry_block(entry_id);

    assert!(!empty_cfg.is_empty());
    assert!(empty_cfg.entry_block().is_some());
    {
        let mut blocks = empty_cfg.blocks();
        assert!(blocks.next().is_some());
        assert!(blocks.next().is_none());
    }
    {
        let mut blocks = empty_cfg.traverse();
        assert!(blocks.next().is_some());
        assert!(blocks.next().is_none());
    }
    assert_eq!(
        "\
main:
	jr	$31
",
        format!("{}", empty_cfg)
    );
}

#[test]
fn displays_only_referenced_labels() {
    let mut function = Function::new("main".into());

    let target = function.create_block_id();

    // This block will be added to the graph, but will have no predecessors. This means it won't
    // show up when rendering or traversing the graph.
    let mut builder = function.start_new_block(Vec::new());
    builder.add_instruction(Instruction::Nop);
    function
        .add_block(builder.terminate(crate::term::jump(BlockRef::new(target.clone(), Vec::new()))));

    let mut builder = function.start_block(target.clone(), Vec::new());
    builder.add_instruction(Instruction::Nop);
    function
        .add_block(builder.terminate(crate::term::jump(BlockRef::new(target.clone(), Vec::new()))));

    function.set_entry_block(target);

    assert_eq!(
        "\
main:
$main.bb0:
	nop
	j	$main.bb0
",
        format!("{}", function)
    );
}
