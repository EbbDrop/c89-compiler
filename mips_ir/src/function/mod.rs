#[cfg(test)]
mod test;

use crate::Label;
use crate::{AnyReg, Instruction, Terminator};
use std::collections::{BTreeMap, BTreeSet, HashMap};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(usize);

impl BlockId {
    pub fn increment(&mut self) {
        self.0 += 1;
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlockLabel {
    function_label: Label,
    id: BlockId,
}

impl BlockLabel {
    pub fn id(&self) -> BlockId {
        self.id
    }
}

impl std::fmt::Display for BlockLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}.bb{}", self.function_label, self.id.0)
    }
}

#[derive(Debug, Clone)]
pub struct BlockRef {
    pub label: BlockLabel,
    pub arguments: Vec<AnyReg>,
}

impl BlockRef {
    pub fn new(label: BlockLabel, arguments: Vec<AnyReg>) -> Self {
        Self { label, arguments }
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
    blocks: HashMap<BlockId, BasicBlock>,
    next_id: BlockId,
}

impl Function {
    pub fn new(label: Label) -> Self {
        Self {
            label,
            entry_block: None,
            blocks: HashMap::new(),
            next_id: BlockId(0),
        }
    }

    pub fn label(&self) -> &Label {
        &self.label
    }

    /// Returns a reference to the entry block of the graph, or `None` if no entry point has been
    /// set (yet).
    pub fn entry_block(&self) -> Option<&BasicBlock> {
        // If an entry block was set, it is guaranteed to not be a dangling reference.
        self.entry_block.map(|id| &self[id])
    }

    /// Returns `true` if the graph is considered empty, i.e. if there's no entry block.
    pub fn is_empty(&self) -> bool {
        self.entry_block.is_none()
    }

    /// Iterate over the id's of all blocks in the graph in an undefined order.
    pub fn block_ids(&self) -> impl Iterator<Item = BlockId> + '_ {
        self.blocks.keys().copied()
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
        self.blocks.get(id)
    }

    /// Returns `true` if a block with `id` has been added to the graph.
    pub fn has_block(&self, id: BlockId) -> bool {
        self.blocks.contains_key(&id)
    }

    /// Returns all successors of the block. This **does** include any dangling references to other
    /// blocks that are not (yet) added to the graph. No references to global labels are returned.
    pub fn all_successors(&self, id: BlockId) -> impl Iterator<Item = BlockId> {
        match &self[id].terminator {
            Terminator::BranchIf(_, _, _, true_target, false_target) => {
                [Some(true_target.label.id), Some(false_target.label.id)].into_iter()
            }
            Terminator::BranchIfZ(_, _, true_target, false_target) => {
                [Some(true_target.label.id), Some(false_target.label.id)].into_iter()
            }
            Terminator::BranchIfZAndLink(_, _, _, false_target) => {
                [Some(false_target.label.id), None].into_iter()
            }
            Terminator::BranchIfFCond(_, true_target, false_target) => {
                [Some(true_target.label.id), Some(false_target.label.id)].into_iter()
            }
            Terminator::Jump(target) => [Some(target.label.id), None].into_iter(),
            Terminator::JumpAndLink(_, next_block) => [Some(next_block.label.id), None].into_iter(),
            Terminator::ReturnToRa => [None, None].into_iter(),
            Terminator::JumpAndLinkRa(_, next_block) => {
                [Some(next_block.label.id), None].into_iter()
            }
            Terminator::JumpAndLinkReg(_, _, next_block) => {
                [Some(next_block.label.id), None].into_iter()
            }
        }
        .flatten()
    }

    /// Returns the successors of the provided block. This does **not** include dangling references
    /// to other blocks that are not (yet) added to the graph. No references to global labels are
    /// returned.
    ///
    /// This is equivalent to filtering [`all_successors`] with [`has_block`].
    pub fn successors(&self, id: BlockId) -> impl Iterator<Item = BlockId> + '_ {
        self.all_successors(id).filter(|&b| self.has_block(b))
    }

    /// Returns the predecessors of the provided block. This does **not** include dangling
    /// references to other blocks that are not (yet) added to the graph. No references to global
    /// labels are returned.
    ///
    /// This is equivalent to all the blocks which have the provided block as a successor (following
    /// the rules of [`successors`]).
    pub fn predecessors(&self, id: BlockId) -> impl Iterator<Item = BlockId> + '_ {
        self.block_ids()
            .filter(move |&b| self.successors(b).any(|s| s == id))
    }

    /// Returns `true` if the provided block has any successors within this graph. Dangling
    /// references are ignored. Semantically equivalent to `self.successors().next().is_some()`, but
    /// with a more efficient implementation. Panics if the block is not added to the graph.
    pub fn has_any_successors(&self, id: BlockId) -> bool {
        match &self[id].terminator {
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

    /// Returns `true` if the provided block has any predecessors within this graph. Dangling
    /// references are ignored. Semantically equivalent to `self.predecessors().next().is_some()`,
    /// but with a more efficient implementation. Panics if the block is not added to the graph.
    pub fn has_any_predecessors(&self, id: BlockId) -> bool {
        self.block_ids()
            .any(move |b| self.successors(b).any(|s| s == id))
    }

    /// Returns the successor that should directly follow the provided block if the graph were
    /// represented sequentially (as MIPS instructions). Panics if the block is not added to the
    /// graph. The returned block might be a dangling reference.
    pub fn default_successor(&self, id: BlockId) -> Option<BlockId> {
        match &self[id].terminator {
            Terminator::BranchIf(_, _, _, _, false_target) => Some(false_target.label.id),
            Terminator::BranchIfZ(_, _, _, false_target) => Some(false_target.label.id),
            Terminator::BranchIfZAndLink(_, _, _, false_target) => Some(false_target.label.id),
            Terminator::BranchIfFCond(_, _, false_target) => Some(false_target.label.id),
            Terminator::Jump(_) => None,
            Terminator::JumpAndLink(_, next_block) => Some(next_block.label.id),
            Terminator::ReturnToRa => None,
            Terminator::JumpAndLinkRa(_, next_block) => Some(next_block.label.id),
            Terminator::JumpAndLinkReg(_, _, next_block) => Some(next_block.label.id),
        }
    }

    /// Returns all successors that are not the default successor (see [`default_successor`]). This
    /// might include dangling references. Panics if the block is not added to the graph.
    pub fn non_default_successors(&self, id: BlockId) -> impl Iterator<Item = BlockId> {
        match &self[id].terminator {
            Terminator::BranchIf(_, _, _, true_target, _) => Some(true_target.label.id),
            Terminator::BranchIfZ(_, _, true_target, _) => Some(true_target.label.id),
            Terminator::BranchIfZAndLink(_, _, _, _) => None,
            Terminator::BranchIfFCond(_, true_target, _) => Some(true_target.label.id),
            Terminator::Jump(target) => Some(target.label.id),
            Terminator::JumpAndLink(_, _) => None,
            Terminator::ReturnToRa => None,
            Terminator::JumpAndLinkRa(_, _) => None,
            Terminator::JumpAndLinkReg(_, _, _) => None,
        }
        .into_iter()
    }

    /// Returns `true` if the block has any non default successors in the graph. Panics if the block
    /// is not added to the graph.
    ///
    /// Semantically equivalent to `self.non_default_successors(id).next().is_some()`.
    pub fn has_any_non_default_successors(&self, id: BlockId) -> bool {
        self.non_default_successors(id).next().is_some()
    }

    /// Returns all blocks in the graph of which the given block is the default successor. The
    /// given block doesn't have to be in the graph.
    pub fn default_predecessors(&self, id: BlockId) -> impl Iterator<Item = BlockId> + '_ {
        self.block_ids()
            .filter(move |&b| self.default_successor(b) == Some(id))
    }

    /// Returns `true` if the graph contains any predecessor of which the given block is the default
    /// successor. The given block doesn't have to be in the graph.
    ///
    /// Semantically equivalent to `self.default_predecessors(id).next().is_some()`, but with a more
    /// efficient implementation.
    pub fn has_any_default_predecessors(&self, id: BlockId) -> bool {
        self.block_ids()
            .any(|b| self.default_successor(b) == Some(id))
    }

    /// Returns all blocks in the graph of which the given block is a non-default successor.
    /// Panics if the given block is not added to the graph.
    pub fn non_default_predecessors(&self, id: BlockId) -> impl Iterator<Item = BlockId> + '_ {
        self.block_ids()
            .filter(move |&b| self.non_default_successors(b).any(|s| s == id))
    }

    /// Returns `true` if the graph contains any predecessor of which the given block is a
    /// non-default successor. The given block doesn't have to be in the graph.
    ///
    /// Semantically equivalent to `self.non_default_predecessors(id).next().is_some()`, but with a
    /// more efficient implementation.
    pub fn has_any_non_default_predecessors(&self, id: BlockId) -> bool {
        self.block_ids()
            .any(|b| self.non_default_successors(b).any(|s| s == id))
    }

    /// Returns `true` if the specified global label is referenced anywhere in this function.
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

    /// Creates a new block label that can be used to create forward references to block that yet
    /// have to be added to the graph.
    ///
    /// See also [`start_block`].
    pub fn create_block_label(&mut self) -> BlockLabel {
        let id = self.next_id;
        self.next_id.increment();
        BlockLabel {
            function_label: self.label.clone(),
            id,
        }
    }

    /// Start a basic block with a previously created label.
    pub fn start_block(&mut self, label: BlockLabel, arguments: Vec<AnyReg>) -> BBBuilder {
        BBBuilder {
            label,
            arguments,
            instructions: Vec::new(),
        }
    }

    /// Starts a new block with a new label. The label of the block can be retrieved from the
    /// returned [`BBBuilder`].
    pub fn start_new_block(&mut self, arguments: Vec<AnyReg>) -> BBBuilder {
        let id = self.create_block_label();
        self.start_block(id, arguments)
    }

    /// Adds the provided block to the graph. Returns a reference to it for convenience.
    pub fn add_block(&mut self, block: BasicBlock) -> &BasicBlock {
        if block.id() >= self.next_id || self.has_block(block.id()) {
            panic!("attempt to add block with invalid or already used label");
        }
        let id = block.id();
        self.blocks.insert(id, block);
        &self.blocks[&id]
    }

    /// Marks a block as the graph's entry block. Panics if the block is not added to the graph.
    pub fn set_entry_block(&mut self, id: BlockId) {
        if !self.has_block(id) {
            panic!("attempt to set nonexistent block as entry block");
        }
        self.entry_block = Some(id)
    }
}

impl std::ops::Index<BlockId> for Function {
    type Output = BasicBlock;

    fn index(&self, index: BlockId) -> &Self::Output {
        &self.blocks[&index]
    }
}

pub struct Traverse<'a> {
    function: &'a Function,
    previous: Option<&'a BasicBlock>,
    already_traversed: BTreeSet<BlockId>,
    to_traverse: BTreeMap<BlockId, &'a BasicBlock>,
}

impl<'a> Traverse<'a> {
    fn new(function: &'a Function, entry_block: Option<&'a BasicBlock>) -> Self {
        let mut to_traverse = BTreeMap::new();
        if let Some(entry_block) = entry_block {
            to_traverse.insert(entry_block.id(), entry_block);
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
        let current_id = match self.previous.take() {
            None => *self.to_traverse.keys().next()?,
            Some(previous) => match self.function.default_successor(previous.id()) {
                None => *self.to_traverse.keys().next()?,
                Some(s) => s,
            },
        };
        let current = self.to_traverse.remove(&current_id).unwrap();
        self.previous = Some(current);
        self.already_traversed.insert(current_id);
        for s_id in self.function.successors(current_id) {
            if !self.already_traversed.contains(&s_id) {
                self.to_traverse.insert(s_id, &self.function[s_id]);
            }
        }
        Some(current)
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    label: BlockLabel,
    arguments: Vec<AnyReg>,
    instructions: Vec<Instruction>,
    terminator: Terminator,
}

impl BasicBlock {
    pub fn id(&self) -> BlockId {
        self.label.id
    }

    pub fn label(&self) -> &BlockLabel {
        &self.label
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

#[derive(Debug)]
pub struct BBBuilder {
    label: BlockLabel,
    arguments: Vec<AnyReg>,
    instructions: Vec<Instruction>,
}

impl BBBuilder {
    pub fn id(&self) -> BlockId {
        self.label.id
    }

    pub fn label(&self) -> &BlockLabel {
        &self.label
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
            label: self.label,
            arguments: self.arguments,
            instructions: self.instructions,
            terminator,
        }
    }
}
