use super::*;
use crate::{term, BZCond, BlockRef, Reg};

/// If a block has the entry block as its default successor, this should be fixed.
#[test]
fn fixes_block_with_entry_block_as_default_successors() {
    let mut function = Function::new("main".into());
    let entry_block_label = function.create_block_label();

    // Add a block that has the entry block as its default successor, and itself as a
    // non-default successor.
    let block_id = {
        let builder = function.start_new_block(Vec::new());
        let block_label = builder.label().clone();
        let block = builder.terminate(term::branch_if_z(
            BZCond::GeZ,
            Reg::T0,
            BlockRef::new(block_label, Vec::new()),
            BlockRef::new(entry_block_label.clone(), Vec::new()),
        ));
        let block = function.add_block(block);
        assert_eq!(
            block.succ_id(block.dsuccidx().unwrap()),
            entry_block_label.id()
        );
        assert!(block.has_ndsucc());
        block.id()
    };

    // Finally, add the entry block.
    let builder = function.start_block(entry_block_label, Vec::new());
    let entry_block = builder.terminate(crate::term::return_to_ra());
    let entry_block_id = function.add_block(entry_block).id();
    function.set_entry_block(entry_block_id);

    // Now see if the fixer can fix it.
    fix_function(&mut function).unwrap();

    assert_eq!(entry_block_id, function.entry_block().unwrap().id());
    assert!(!function[entry_block_id].has_dpreds_in(&function));
    // Fixer cannot just swap the default and non-default successor, because then the non-entry
    // block would have a self-loop (i.e. its default successor wos_any);
    assert_eq!(
        function[block_id]
            .ndsuccs_in(&function)
            .next()
            .map(|b| b.id()),
        Some(entry_block_id)
    );
    assert_eq!(
        function[block_id]
            .dsucc_in(&function)
            .and_then(|b| b.ndsuccs_in(&function).next().map(|b| b.id())),
        Some(block_id)
    );
}

/// If a block has the entry block as both its default successor and all non-default successors,
/// this should be fixed.
#[test]
fn fixes_block_with_entry_block_as_all_successors() {
    let mut function = Function::new("main".into());
    let entry_block_label = function.create_block_label();

    // Add a block that has the entry block as its default successor, and as all non-default
    // successors.
    let builder = function.start_new_block(Vec::new());
    let block = builder.terminate(term::branch_if_z(
        BZCond::GeZ,
        Reg::T0,
        BlockRef::new(entry_block_label.clone(), Vec::new()),
        BlockRef::new(entry_block_label.clone(), Vec::new()),
    ));
    let block = function.add_block(block);
    assert_eq!(
        block.dsuccidx().map(|idx| block.succ_id(idx)),
        Some(entry_block_label.id())
    );

    // Finally, add the entry block.
    let builder = function.start_block(entry_block_label, Vec::new());
    let entry_block = builder.terminate(crate::term::return_to_ra());
    let entry_block_id = function.add_block(entry_block).id();
    function.set_entry_block(entry_block_id);

    // Now see if the fixer can fix it.
    fix_function(&mut function).unwrap();
    assert_eq!(entry_block_id, function.entry_block().unwrap().id());
    assert!(!function[entry_block_id].has_dpreds_in(&function));
}

/// If a block has itself as default successor, this should be fixed.
#[test]
fn fixes_direct_recursive_default_successor() {
    let mut function = Function::new("main".into());
    let entry_block_label = function.create_block_label();

    // Add a block that has itself as default successor.
    let block_id = {
        let builder = function.start_new_block(Vec::new());
        let block_label = builder.label().clone();
        let block = builder.terminate(term::branch_if_z(
            BZCond::GeZ,
            Reg::T0,
            BlockRef::new(entry_block_label.clone(), Vec::new()),
            BlockRef::new(block_label, Vec::new()),
        ));
        let block = function.add_block(block);
        assert_eq!(
            block.dsuccidx().map(|idx| block.succ_id(idx)),
            Some(block.id())
        );
        block.id()
    };

    // Add the entry block.
    let builder = function.start_block(entry_block_label, Vec::new());
    let entry_block = builder.terminate(crate::term::return_to_ra());
    let entry_block_id = function.add_block(entry_block).id();
    function.set_entry_block(entry_block_id);

    // Now see if the fixer can fix it.
    fix_function(&mut function).unwrap();
    assert_eq!(entry_block_id, function.entry_block().unwrap().id());
    assert!(!function[entry_block_id].has_dpreds_in(&function));
    assert_ne!(
        function[block_id].dsucc_in(&function).map(|b| b.id()),
        Some(block_id)
    );
}

/// If the graph has a default successor loop, this should be fixed.
#[test]
fn fixes_default_successor_loop() {
    let mut function = Function::new("main".into());

    let block1_label = function.create_block_label();
    let block1_id = block1_label.id();

    // block3 has block1 as its default successor
    let builder = function.start_new_block(Vec::new());
    let block3_label = builder.label().clone();
    let block3_id = block3_label.id();
    let block = builder.terminate(term::branch_if_z(
        BZCond::GeZ,
        Reg::T0,
        BlockRef::new(block3_label.clone(), Vec::new()),
        BlockRef::new(block1_label.clone(), Vec::new()),
    ));
    function.add_block(block);
    assert_eq!(
        function[block3_id]
            .dsuccidx()
            .map(|idx| function[block3_id].succ_id(idx)),
        Some(block1_id)
    );

    // block2 has block3 as its default successor
    let builder = function.start_new_block(Vec::new());
    let block2_label = builder.label().clone();
    let block2_id = block2_label.id();
    let block = builder.terminate(term::branch_if_z(
        BZCond::GeZ,
        Reg::T0,
        BlockRef::new(block2_label.clone(), Vec::new()),
        BlockRef::new(block3_label.clone(), Vec::new()),
    ));
    function.add_block(block);
    assert_eq!(
        function[block2_id].dsucc_in(&function).map(|b| b.id()),
        Some(block3_id)
    );

    // block1 has block2 as its default successor
    let builder = function.start_block(block1_label.clone(), Vec::new());
    let block = builder.terminate(term::branch_if_z(
        BZCond::GeZ,
        Reg::T0,
        BlockRef::new(block1_label.clone(), Vec::new()),
        BlockRef::new(block2_label.clone(), Vec::new()),
    ));
    function.add_block(block);
    assert_eq!(
        function[block1_id].dsucc_in(&function).map(|b| b.id()),
        Some(block2_id)
    );

    // Add the entry block with a reference to block1.
    let builder = function.start_new_block(Vec::new());
    let entry_block = builder.terminate(crate::term::jump(BlockRef::new(
        block1_label.clone(),
        Vec::new(),
    )));
    let entry_block_id = function.add_block(entry_block).id();
    function.set_entry_block(entry_block_id);

    // Now see if the fixer can fix it.
    fix_function(&mut function).unwrap();
    assert_eq!(entry_block_id, function.entry_block().unwrap().id());
    assert!(!function[entry_block_id].has_dpreds_in(&function));
    // Fixer should have broken the default link between block3->block1.
    assert_eq!(
        function[block1_id].dsucc_in(&function).map(|b| b.id()),
        Some(block2_id)
    );
    assert_eq!(
        function[block2_id].dsucc_in(&function).map(|b| b.id()),
        Some(block3_id)
    );
    assert_ne!(
        function[block3_id].dsucc_in(&function).map(|b| b.id()),
        Some(block1_id)
    );
}

/// If the entry block has itself as default successor, this should be fixed.
#[test]
fn fixes_entry_block_direct_recursive_default_successor() {
    let mut function = Function::new("main".into());
    let entry_block_label = function.create_block_label();

    // Add the entry block with itself as default (and non-default successor).
    let builder = function.start_block(entry_block_label.clone(), Vec::new());
    let entry_block = builder.terminate(term::branch_if_z(
        BZCond::GeZ,
        Reg::T0,
        BlockRef::new(entry_block_label.clone(), Vec::new()),
        BlockRef::new(entry_block_label, Vec::new()),
    ));
    let entry_block_id = function.add_block(entry_block).id();
    function.set_entry_block(entry_block_id);

    // Now see if the fixer can fix it.
    fix_function(&mut function).unwrap();
    assert_eq!(entry_block_id, function.entry_block().unwrap().id());
    assert!(!function[entry_block_id].has_dpreds_in(&function));
    assert_ne!(
        function[entry_block_id].dsucc_in(&function).map(|b| b.id()),
        Some(entry_block_id)
    );
}
