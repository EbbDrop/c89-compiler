#[cfg(test)]
mod test;

pub mod graph;

use crate::{
    AlignBoundary, AnyReg, BCond, BZCond, FunctionCall, Instruction, Label, Reg, Terminator,
    VirtualInstruction,
};
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

/// Used to specify witch registers need to start with pointers to stack allocated space.
#[derive(Debug, Clone)]
pub struct ReferenceRegister {
    pub register: Reg,
    pub stack_info: StackInfo,
}

#[derive(Debug, Clone)]
pub struct StackInfo {
    pub size: u128,
    pub alignment: AlignBoundary,
    pub signed: bool,
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
    /// The registers here are expected to have references to places on the stack before the
    /// entry block is started.
    reference_registers: Vec<ReferenceRegister>,
    params_info: Vec<StackInfo>,
    next_id: BlockId,
}

impl Function {
    pub fn new(label: Label, params_info: Vec<StackInfo>) -> Self {
        Self {
            label,
            entry_block: None,
            blocks: HashMap::new(),
            reference_registers: Vec::new(),
            params_info,
            next_id: BlockId(0),
        }
    }

    /// Returns the global label of this function. It's not possible to change this.
    pub fn label(&self) -> &Label {
        &self.label
    }

    pub fn params_info(&self) -> &[StackInfo] {
        &self.params_info
    }

    /// Returns a reference to the entry block of the graph, or `None` if no entry point has been
    /// set (yet).
    pub fn entry_block(&self) -> Option<&BasicBlock> {
        // If an entry block was set, it is guaranteed to not be a dangling reference.
        self.entry_block.map(|id| &self[id])
    }

    /// Returns `true` if the graph is completely empty, i.e. if doesn't contain any blocks. This
    /// will return `false` if the graph has any blocks even if the entry block is not set.
    ///
    /// To just check whether the graph is considered empty, use `self.entry_block().is_none()`.
    pub fn is_empty(&self) -> bool {
        self.blocks.is_empty()
    }

    /// Iterate over the id's of all blocks in the graph in an undefined order.
    pub fn block_ids(&self) -> impl Iterator<Item = BlockId> + '_ {
        self.blocks.keys().copied()
    }

    /// Iterate over all blocks in the graph in an undefined order.
    pub fn blocks(&self) -> impl Iterator<Item = &BasicBlock> {
        self.blocks.values()
    }

    pub fn reference_registers(&self) -> &[ReferenceRegister] {
        &self.reference_registers
    }

    pub fn add_reference_register(&mut self, ref_reg: ReferenceRegister) {
        self.reference_registers.push(ref_reg);
    }

    /// Returns an iterator that traverses the graph starting from the entry point, visiting every
    /// reachable node exactly once. The default successor of a node is always traversed first (if
    /// it hasn't been traversed yet).
    pub fn traverse(&self) -> Traverser {
        Traverser::new(self, self.entry_block().into_iter())
    }

    /// Same as [`traverse`], but this will also traverse unreachable blocks (i.e. block without
    /// predecessors).
    pub fn traverse_all(&self) -> Traverser {
        Traverser::new(self, self.entry_block().into_iter().chain(self.blocks()))
    }

    /// Returns a reference to the block identified by `id`, or `None` if the block isn't (yet)
    /// added to the graph.
    pub fn get_block(&self, id: BlockId) -> Option<&BasicBlock> {
        self.blocks.get(&id)
    }

    /// Returns a mutable reference to the block identified by `id`, or `None` if the block isn't
    /// (yet) added to the graph.
    // This is safe, since the label of a block can't be changed.
    pub fn get_block_mut(&mut self, id: BlockId) -> Option<&mut BasicBlock> {
        self.blocks.get_mut(&id)
    }

    /// Returns `true` if a block with `id` has been added to the graph.
    pub fn has_block(&self, id: BlockId) -> bool {
        self.blocks.contains_key(&id)
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
    // TODO: guarantee that arguments are unique?
    pub fn start_block(&mut self, label: BlockLabel, arguments: Vec<AnyReg>) -> BBBuilder {
        BBBuilder {
            label,
            arguments,
            instructions: Vec::new(),
        }
    }

    /// Starts a new block with a new label. The label of the block can be retrieved from the
    /// returned [`BBBuilder`].
    // TODO: guarantee that arguments are unique?
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

    /// Removes the block with the given id from the graph. Returns the removed [`BasicBlock`].
    /// If this was the entry block, the graph's entry block will be unset.
    pub fn remove_block(&mut self, id: BlockId) -> BasicBlock {
        if self.entry_block == Some(id) {
            self.entry_block = None;
        }
        self.blocks
            .remove(&id)
            .expect("attempt to remove block that is not in the graph")
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

impl std::ops::IndexMut<BlockId> for Function {
    // This is safe, since the label of a block can't be changed.
    fn index_mut(&mut self, index: BlockId) -> &mut Self::Output {
        self.blocks.get_mut(&index).unwrap()
    }
}

pub struct Traverser<'a> {
    function: &'a Function,
    /// If `Some(_)`, this will be traversed next. Otherwise a random block from `to_traverse` is
    /// chosen.
    next: Option<BlockId>,
    already_traversed: BTreeSet<BlockId>,
    to_traverse: BTreeMap<BlockId, &'a BasicBlock>,
}

impl<'a> Traverser<'a> {
    /// Creates a new traverser over the blocks of `function`, starting from the first block in
    /// `must_traverse`, and always following the default successor if the previously traversed
    /// block has one, and if it's not already traversed. Guaranteed to (also) visit all other
    /// blocks in `must_traverse`. Blocks without predecessors that are not included in
    /// `must_traverse` will not be traversed.
    fn new(
        function: &'a Function,
        mut must_traverse: impl Iterator<Item = &'a BasicBlock>,
    ) -> Self {
        let next = must_traverse.next().map(|b| b.id());
        let mut to_traverse = BTreeMap::new();
        for block in must_traverse {
            to_traverse.insert(block.id(), block);
        }
        Self {
            function,
            next,
            already_traversed: BTreeSet::new(),
            to_traverse,
        }
    }

    /// Returns `true` if this traverser already traversed the given block.
    pub fn has_traversed(&self, block: BlockId) -> bool {
        self.already_traversed.contains(&block)
    }
}

impl<'a> Iterator for Traverser<'a> {
    type Item = &'a BasicBlock;

    fn next(&mut self) -> Option<Self::Item> {
        let current_id = self
            .next
            .take()
            .or_else(|| self.to_traverse.keys().next().copied())?;
        let current = self
            .to_traverse
            .remove(&current_id)
            .unwrap_or(&self.function[current_id]);
        self.already_traversed.insert(current_id);
        for block in current.succs_in(self.function) {
            if !self.already_traversed.contains(&block.id()) {
                self.to_traverse.insert(block.id(), block);
            }
        }
        if let Some(default_successor) = current.dsucc_in(self.function) {
            if !self.already_traversed.contains(&default_successor.id()) {
                self.next = Some(default_successor.id());
            }
        }
        Some(current)
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    // It's important that this fields stays private, to prevent direct construction. `BBBuilder`
    // should be used for construction instead.
    label: BlockLabel,
    // TODO: guarantee that arguments are unique?
    pub arguments: Vec<AnyReg>,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

impl BasicBlock {
    #[inline]
    pub fn id(&self) -> BlockId {
        self.label.id
    }

    #[inline]
    pub fn label(&self) -> &BlockLabel {
        &self.label
    }

    /// Returns `true` if the specified global label is referenced anywhere in this block.
    pub fn references_label(&self, label: &Label) -> bool {
        match &self.terminator {
            Terminator::BranchIfZAndLink(_, _, target, _) | Terminator::JumpAndLink(target, _) => {
                target == label
            }
            _ => false,
        }
    }

    /// Returns all the global labels that are referenced anywhere in this block.
    pub fn referenced_labels(&self) -> impl Iterator<Item = &Label> {
        self.instructions
            .iter()
            .filter_map(|instr| match instr {
                Instruction::Virtual(VirtualInstruction::FunctionCall(FunctionCall {
                    label,
                    ..
                })) => Some(label),
                _ => None,
            })
            .chain(match &self.terminator {
                Terminator::BranchIfZAndLink(_, _, target, _)
                | Terminator::JumpAndLink(target, _) => Some(target),
                _ => None,
            })
    }

    pub fn calls_function(&self, label: &Label) -> bool {
        self.instructions.iter().any(|instr| match instr {
            Instruction::Virtual(VirtualInstruction::FunctionCall(call)) => &call.label == label,
            _ => false,
        })
    }

    pub fn function_calls(&self) -> impl Iterator<Item = &FunctionCall> {
        self.instructions.iter().filter_map(|instr| match instr {
            Instruction::Virtual(VirtualInstruction::FunctionCall(call)) => Some(call),
            _ => None,
        })
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

    pub fn add_argument(&mut self, arg: AnyReg) {
        self.arguments.push(arg);
    }

    #[must_use = "A BasicBlock nees to be added to a function to do something"]
    pub fn terminate(self, terminator: Terminator) -> BasicBlock {
        BasicBlock {
            label: self.label,
            arguments: self.arguments,
            instructions: self.instructions,
            terminator,
        }
    }
}
