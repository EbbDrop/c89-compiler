use super::*;

#[test]
fn empty_functions() {
    let mut empty_cfg = Function::new("main".into());

    assert!(empty_cfg.is_empty());
    assert!(empty_cfg.entry_block().is_none());
    assert!(empty_cfg.blocks().next().is_none());
    assert!(empty_cfg.traverse().next().is_none());
    assert!(empty_cfg.label().as_ref() == "main");

    let entry_label = empty_cfg.create_block_label();
    let block = empty_cfg
        .start_block(entry_label.clone(), Vec::new())
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

    empty_cfg.set_entry_block(entry_label.id());

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
    let entry_block = empty_cfg.entry_block();
    assert!(entry_block.is_some());
    assert!(*entry_block.unwrap().label() == entry_label);
}

#[test]
fn traverses_only_referenced_labels() {
    let mut function = Function::new("main".into());

    let target = function.create_block_label();

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

    function.set_entry_block(target.id());

    let mut traverse = function.traverse();
    let first = traverse.next();
    assert!(first.is_some());
    assert!(*first.unwrap().label() == target);
    assert!(traverse.next().is_none());
}
