#[cfg(test)]
mod test;

use crate::Label;
use crate::{AnyReg, Instruction, Terminator};
use std::collections::{BTreeMap, BTreeSet, HashMap};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlockId {
    function_label: Label,
    uid: usize,
}

impl std::fmt::Display for BlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}.bb{}", self.function_label, self.uid)
    }
}

#[derive(Debug, Clone)]
pub struct BlockRef {
    pub block: BlockId,
    pub arguments: Vec<AnyReg>,
}

impl BlockRef {
    pub fn new(block: BlockId, arguments: Vec<AnyReg>) -> Self {
        Self { block, arguments }
    }
}

impl std::fmt::Display for BlockRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.block.fmt(f)
    }
}

/// A Control Flow Graph of Basic Blocks with a global label.
///
/// The graph has at most one entry point. If no entry point is set, the graph should be considered
/// empty. Most methods do not take this account, though. It is for example still possible to
/// retrieve a block, and iterate over all blocks. The most notable method that depends on whether
/// an entry point was set is [`traverse`]. Without entry point, no blocks will be traversed.
///
/// Each node in the graph is a Basic Block. The terminator instruction of each Basic Block is used
/// to form the edges of the graph. For example, a _jump_ terminator causes the Basic Block to have
/// exactly one successor, namely the target of the jump. Note that the graph is directed.
///
/// [`BlockId`]s are used to identify the nodes in the graph. A [`BlockId`] combines the function
/// label with a unique (within this graph) identifier for each block. This identifier will never
/// change, but might get invalidated if the node gets removed from the graph. However it is
/// guaranteed that the id will never be reused for other nodes in the future.
///
/// It is possible to create what would essentially be 'forward references' to a node using
/// [`create_block_id`]. It is not enforced nor required to actually add a node for which an id was
/// created to the graph. In other words, **dangling references are allowed**. This is the case as
/// well for references to global labels. E.g. a _jump and link_ terminator has a global label as
/// its target, which is essentially an edge pointing outside the graph. Dangling reference are
/// skipped over in methods s.a. [`successors`].
///
/// # Creating [`Basic Block`]s
///
/// The only way to create a new node is using one of the provided methods on the graph:
/// [`start_block`] or [`start_new_block`]. These will return a [`BasicBlockBuilder`], which can be
/// used to add (body) instruction and finally terminate the block with a [`Terminator`]. This will
/// give you a [`Basic Block`] instance, which can then be added to the graph using the
/// [`add_block`] method.
#[derive(Debug)]
pub struct Function {
    label: Label,
    entry_block: Option<BlockId>,
    blocks: HashMap<usize, BasicBlock>,
    next_uid: usize,
}

impl Function {
    pub fn new(label: Label) -> Self {
        Self {
            label,
            entry_block: None,
            blocks: HashMap::new(),
            next_uid: 0,
        }
    }

    pub fn label(&self) -> &Label {
        &self.label
    }

    pub fn entry_block(&self) -> Option<&BasicBlock> {
        self.entry_block.as_ref().map(|id| &self[id])
    }

    /// Returns `true` if the graph is considered empty, i.e. if there's no entry block.
    pub fn is_empty(&self) -> bool {
        self.entry_block.is_none()
    }

    /// Iterate over all blocks in the graph in an undefined order.
    pub fn blocks(&self) -> impl Iterator<Item = &BasicBlock> {
        self.blocks.values()
    }

    /// Returns an iterator that traverses the graph starting from the entry point, visiting every
    /// reachable node exactly once. The default successor of a node is always traversed first (if
    /// it hasn't been traversed yet).
    pub fn traverse(&self) -> Traverse {
        Traverse::new(self, self.entry_block())
    }

    /// Returns a reference to the block identified by `id`, or `None` if the block isn't (yet)
    /// added to the graph.
    pub fn get_block(&self, id: &BlockId) -> Option<&BasicBlock> {
        self.blocks.get(&id.uid)
    }

    /// Returns `true` if a block with `id` has been added to the graph.
    pub fn contains_block(&self, id: &BlockId) -> bool {
        self.blocks.contains_key(&id.uid)
    }

    /// Returns the successors of the provided block. Note that this doesn't include any dangling
    /// references the block has, nor any references to global labels. Only references to blocks
    /// that currently exist in the graph are iterated.
    pub fn successors(&self, block: &BlockId) -> impl Iterator<Item = &BasicBlock> {
        match &self[block].terminator {
            Terminator::BranchIf(_, _, _, true_target, false_target) => {
                [Some(true_target), Some(false_target)].into_iter()
            }
            Terminator::BranchIfZ(_, _, true_target, false_target) => {
                [Some(true_target), Some(false_target)].into_iter()
            }
            Terminator::BranchIfZAndLink(_, _, _, false_target) => {
                [Some(false_target), None].into_iter()
            }
            Terminator::BranchIfFCond(_, true_target, false_target) => {
                [Some(true_target), Some(false_target)].into_iter()
            }
            Terminator::Jump(target) => [Some(target), None].into_iter(),
            Terminator::JumpAndLink(_, next_block) => [Some(next_block), None].into_iter(),
            Terminator::ReturnToRa => [None, None].into_iter(),
            Terminator::JumpAndLinkRa(_, next_block) => [Some(next_block), None].into_iter(),
            Terminator::JumpAndLinkReg(_, _, next_block) => [Some(next_block), None].into_iter(),
        }
        .flat_map(|bref| bref.and_then(|bref| self.get_block(&bref.block)))
    }

    /// Returns the predecessors of the provided block. This is equivalent to all the blocks which
    /// have the provided block as a successor (following the rules of [`successors`]).
    pub fn predecessors<'a: 'b, 'b>(
        &'a self,
        block: &'b BlockId,
    ) -> impl Iterator<Item = &'a BasicBlock> + 'b {
        self.blocks().filter(|b| self.has_successor(&b.id, block))
    }

    /// Returns `true` if `potential_successor` is a successor. Panics if `base` is not added to the
    /// graph. `potential_successor` doesn't have to be in the graph.
    pub fn has_successor(&self, base: &BlockId, potential_successor: &BlockId) -> bool {
        match &self[base].terminator {
            Terminator::BranchIf(_, _, _, true_target, false_target) => {
                &true_target.block == potential_successor
                    || &false_target.block == potential_successor
            }
            Terminator::BranchIfZ(_, _, true_target, false_target) => {
                &true_target.block == potential_successor
                    || &false_target.block == potential_successor
            }
            Terminator::BranchIfZAndLink(_, _, _, false_target) => {
                &false_target.block == potential_successor
            }
            Terminator::BranchIfFCond(_, true_target, false_target) => {
                &true_target.block == potential_successor
                    || &false_target.block == potential_successor
            }
            Terminator::Jump(target) => &target.block == potential_successor,
            Terminator::JumpAndLink(_, next_block) => &next_block.block == potential_successor,
            Terminator::ReturnToRa => false,
            Terminator::JumpAndLinkRa(_, next_block) => &next_block.block == potential_successor,
            Terminator::JumpAndLinkReg(_, _, next_block) => {
                &next_block.block == potential_successor
            }
        }
    }

    /// Returns `true` if `potential_predecessor` is a predecessor of `base`. Panics if
    /// `potential_predecessor` is not added to the graph. `base` doesn't have to be in the graph.
    pub fn has_predecessor(&self, base: &BlockId, potential_predecessor: &BlockId) -> bool {
        self.has_successor(potential_predecessor, base)
    }

    /// Returns `true` if the provided block has any successors within this graph. Semantically
    /// equivalent to `self.successors().next().is_some()`, but with a more efficient
    /// implementation. Panics if the block is not added to the graph.
    pub fn has_successors(&self, block: &BlockId) -> bool {
        match &self[block].terminator {
            Terminator::BranchIf(_, _, _, _, _) => true,
            Terminator::BranchIfZ(_, _, _, _) => true,
            Terminator::BranchIfZAndLink(_, _, _, _) => true,
            Terminator::BranchIfFCond(_, _, _) => true,
            Terminator::Jump(_) => true,
            Terminator::JumpAndLink(_, _) => true,
            Terminator::ReturnToRa => false,
            Terminator::JumpAndLinkRa(_, _) => true,
            Terminator::JumpAndLinkReg(_, _, _) => true,
        }
    }

    /// Returns `true` if the provided block has any predecessors within this graph. Semantically
    /// equivalent to `self.predecessors().next().is_some()`, but with a more efficient
    /// implementation. Panics if the block is not added to the graph.
    pub fn has_predecessors(&self, block: &BlockId) -> bool {
        self.blocks().any(|b| self.has_successor(&b.id, block))
    }

    /// Returns the successor that should directly follow the provided block if the graph were
    /// represented sequentially (as MIPS instructions). Panics if the blockis not added to the
    /// graph.
    pub fn default_successor<'a>(&'a self, block: &BlockId) -> Option<&'a BasicBlock> {
        match &self[block].terminator {
            Terminator::BranchIf(_, _, _, _, false_target) => Some(false_target),
            Terminator::BranchIfZ(_, _, _, false_target) => Some(false_target),
            Terminator::BranchIfZAndLink(_, _, _, false_target) => Some(false_target),
            Terminator::BranchIfFCond(_, _, false_target) => Some(false_target),
            Terminator::Jump(_) => None,
            Terminator::JumpAndLink(_, next_block) => Some(next_block),
            Terminator::ReturnToRa => None,
            Terminator::JumpAndLinkRa(_, next_block) => Some(next_block),
            Terminator::JumpAndLinkReg(_, _, next_block) => Some(next_block),
        }
        .map(|bref| &self[&bref.block])
    }

    /// Returns all successors that are not the default successor. Panics if the given block is not
    /// added to this graph. Panics if the block is not added to the graph.
    pub fn other_successors(&self, block: &BlockId) -> impl IntoIterator<Item = &BasicBlock> {
        match &self[block].terminator {
            Terminator::BranchIf(_, _, _, true_target, _) => Some(true_target),
            Terminator::BranchIfZ(_, _, true_target, _) => Some(true_target),
            Terminator::BranchIfZAndLink(_, _, _, _) => None,
            Terminator::BranchIfFCond(_, true_target, _) => Some(true_target),
            Terminator::Jump(target) => Some(target),
            Terminator::JumpAndLink(_, _) => None,
            Terminator::ReturnToRa => None,
            Terminator::JumpAndLinkRa(_, _) => None,
            Terminator::JumpAndLinkReg(_, _, _) => None,
        }
        .map(|bref| &self[&bref.block])
    }

    /// Returns `true` if the specified label is referenced anywhere in this function.
    pub fn references_label(&self, label: &Label) -> bool {
        for block in self.blocks() {
            match &block.terminator {
                Terminator::BranchIfZAndLink(_, _, target, _)
                | Terminator::JumpAndLink(target, _)
                    if target == label =>
                {
                    return true
                }
                _ => {}
            }
        }
        false
    }

    pub fn create_block_id(&mut self) -> BlockId {
        let uid = self.next_uid;
        self.next_uid += 1;
        BlockId {
            function_label: self.label.clone(),
            uid,
        }
    }

    pub fn start_block(&mut self, id: BlockId, arguments: Vec<AnyReg>) -> BBBuilder {
        BBBuilder {
            id,
            arguments,
            instructions: Vec::new(),
        }
    }

    pub fn start_new_block(&mut self, arguments: Vec<AnyReg>) -> BBBuilder {
        let id = self.create_block_id();
        self.start_block(id, arguments)
    }

    /// Adds the provided block to the graph. Returns a reference to it for convenience.
    pub fn add_block(&mut self, block: BasicBlock) -> &BasicBlock {
        if block.id.uid >= self.next_uid || self.blocks.contains_key(&block.id.uid) {
            panic!("attempt to add block with invalid id");
        }
        let uid = block.id.uid;
        self.blocks.insert(uid, block);
        &self.blocks[&uid]
    }

    /// Marks the provided block as the graph's entry block.
    pub fn set_entry_block(&mut self, block: BlockId) {
        if !self.blocks.contains_key(&block.uid) {
            panic!("attempt to set nonexistent block as entry block");
        }
        self.entry_block = Some(block)
    }
}

impl std::ops::Index<&BlockId> for Function {
    type Output = BasicBlock;

    fn index(&self, index: &BlockId) -> &Self::Output {
        &self.blocks[&index.uid]
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.label)?;
        // TODO: FIXME: Patch the graph such that there are no two nodes with the same default
        // successor (i.e. such that every node has exactly one predecessor for which it is the
        // default successor) and such that there is no node which has the entry point as default
        // successor.
        for block in self.traverse() {
            if self.has_predecessors(&block.id) {
                writeln!(f, "{}:", block.id)?;
            }
            write!(f, "{block}")?;
        }
        Ok(())
    }
}

pub struct Traverse<'a> {
    function: &'a Function,
    previous: Option<&'a BasicBlock>,
    already_traversed: BTreeSet<usize>,
    to_traverse: BTreeMap<usize, &'a BasicBlock>,
}

impl<'a> Traverse<'a> {
    fn new(function: &'a Function, entry_block: Option<&'a BasicBlock>) -> Self {
        let mut to_traverse = BTreeMap::new();
        if let Some(entry_block) = entry_block {
            to_traverse.insert(entry_block.id.uid, entry_block);
        }
        Self {
            function,
            previous: None,
            already_traversed: BTreeSet::new(),
            to_traverse,
        }
    }
}

impl<'a> Iterator for Traverse<'a> {
    type Item = &'a BasicBlock;

    fn next(&mut self) -> Option<Self::Item> {
        let current_uid = match self.previous.take() {
            None => *self.to_traverse.keys().next()?,
            Some(previous) => match self.function.default_successor(&previous.id) {
                None => *self.to_traverse.keys().next()?,
                Some(s) => s.id.uid,
            },
        };
        let current = self.to_traverse.remove(&current_uid).unwrap();
        self.previous = Some(current);
        self.already_traversed.insert(current_uid);
        for successor in self.function.successors(&current.id) {
            if !self.already_traversed.contains(&successor.id.uid) {
                self.to_traverse.insert(successor.id.uid, successor);
            }
        }
        Some(current)
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    id: BlockId,
    arguments: Vec<AnyReg>,
    instructions: Vec<Instruction>,
    terminator: Terminator,
}

impl BasicBlock {
    pub fn id(&self) -> &BlockId {
        &self.id
    }

    pub fn arguments(&self) -> &[AnyReg] {
        &self.arguments
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    pub fn terminator(&self) -> &Terminator {
        &self.terminator
    }
}

impl std::fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for instr in &self.instructions {
            writeln!(f, "\t{instr}")?;
        }
        writeln!(f, "\t{}", self.terminator)
    }
}

#[derive(Debug)]
pub struct BBBuilder {
    id: BlockId,
    arguments: Vec<AnyReg>,
    instructions: Vec<Instruction>,
}

impl BBBuilder {
    pub fn id(&self) -> &BlockId {
        &self.id
    }

    pub fn arguments(&self) -> &[AnyReg] {
        &self.arguments
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    pub fn add_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn terminate(self, terminator: Terminator) -> BasicBlock {
        BasicBlock {
            id: self.id,
            arguments: self.arguments,
            instructions: self.instructions,
            terminator,
        }
    }
}
