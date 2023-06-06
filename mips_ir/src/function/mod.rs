mod stack_frame;

use crate::cfg::{self, BlockId, BlockRef, Cfg};
use crate::{scanner, AnyReg, Instruction, Label, Reg, Terminator, VirtualInstruction};
use std::collections::{BTreeMap, BTreeSet, HashMap};

pub use stack_frame::*;

/// Used to specify which registers need to start with pointers to stack allocated space.
#[derive(Debug, Clone)]
pub struct ReferenceRegister {
    pub register: Reg,
    pub stack_info: StackInfo,
}

#[derive(Debug)]
pub struct Function {
    label: Label,
    pub(crate) cfg: Cfg,
    pub(crate) exit_block_id: Option<BlockId>,
    /// The registers here are expected to have references to places on the stack before they're
    /// first used.
    pub(crate) reference_registers: Vec<ReferenceRegister>,
    /// Gives the StackAddress of the item that the reference registers points to, not of the
    /// reference register itself! (Check `reg_to_stack_address` for that.)
    pub(crate) reference_register_to_stack_address: HashMap<Reg, StackAddress>,
    /// If registers have been spilled to memory, their stack_addresses will be given here.
    pub(crate) reg_to_stack_address: HashMap<AnyReg, StackAddress>,
    pub(crate) params: Vec<StackAddress>,
    pub(crate) stack_frame: StackFrame,
    stack_info: BTreeMap<StackAddress, StackInfo>,
}

impl Function {
    pub fn new(label: Label, params_info: Vec<StackInfo>) -> Self {
        let mut function = Self {
            label,
            cfg: Cfg::new(cfg::BasicBlock::new_incomplete()),
            exit_block_id: None,
            reference_registers: Vec::new(),
            reference_register_to_stack_address: HashMap::new(),
            reg_to_stack_address: HashMap::new(),
            params: Vec::new(),
            stack_info: BTreeMap::new(),
            stack_frame: StackFrame::new(),
        };

        let function_entry_block_id = function.cfg.insert(cfg::BasicBlock::new_incomplete());
        function.cfg.entry_block_mut().terminator = Some(crate::term::jump(BlockRef::new(
            function_entry_block_id,
            // Note that this may be incorrect, because a param could be a FReg. It's up to
            // `finish` to fix this.
            vec![Reg::ZERO.into(); params_info.len()],
        )));

        for stack_info in params_info {
            let stack_address = function.create_stack_address(stack_info.clone());
            function.params.push(stack_address);
            function.stack_frame.add_param(stack_info, stack_address);
        }

        function
    }

    /// Returns the global label of this function. It's not possible to change this.
    pub fn label(&self) -> &Label {
        &self.label
    }

    /// Returns a label for the given block. This label remains only valid while the function is
    /// not modified!
    pub fn block_label(&self, block: BlockId) -> Label {
        Label::from(format!(
            "${}.bb{}",
            self.label,
            block.as_id_in_cfg_of_len(self.cfg.len())
        ))
    }

    // Note that the function's entry block is actually the successor of the CFG's entry block.
    pub(crate) fn entry_block_id(&self) -> Option<BlockId> {
        let bref = self.cfg.entry_block().successors().next().unwrap();
        self.cfg[bref.id].is_complete().then_some(bref.id)
    }

    /// Returns a mutable reference to the entry block of the function, or `None` if the entry
    /// block hasn't been started yet.
    pub(crate) fn entry_block(&self) -> Option<&cfg::BasicBlock> {
        self.entry_block_id().map(|id| &self.cfg[id])
    }

    /// Iterate over the id's of all blocks in the graph in an undefined order.
    fn block_ids(&self) -> impl Iterator<Item = BlockId> + '_ {
        self.cfg
            .blocks()
            .filter_map(|(id, block)| block.is_complete().then_some(id))
    }

    pub fn add_reference_register(&mut self, ref_reg: ReferenceRegister) {
        self.reference_registers.push(ref_reg);
    }

    pub(crate) fn non_param_stack_addresses(&self) -> impl Iterator<Item = &StackAddress> {
        self.stack_info
            .keys()
            .filter(|sa| !self.params.contains(sa))
    }

    pub(crate) fn stack_info(&self, stack_address: StackAddress) -> &StackInfo {
        &self.stack_info[&stack_address]
    }

    pub(crate) fn create_stack_address(&mut self, stack_info: StackInfo) -> StackAddress {
        let last_n = self
            .stack_info
            .last_key_value()
            .map(|(k, _)| k.0)
            .unwrap_or(0);
        let stack_address = StackAddress(last_n + 1);
        self.stack_info.insert(stack_address, stack_info);
        stack_address
    }

    /// Returns an iterator that traverses the graph starting from the entry point, visiting every
    /// reachable node exactly once. The default successor of a node is always traversed first (if
    /// it hasn't been traversed yet).
    pub fn traverse(&self) -> Traverser {
        Traverser::new(self, std::iter::once(self.cfg.entry_block_id()))
    }

    /// Same as [`traverse`], but this will also traverse unreachable blocks (i.e. blocks without
    /// predecessors).
    pub fn traverse_all(&self) -> Traverser {
        Traverser::new(
            self,
            std::iter::once(self.cfg.entry_block_id()).chain(self.block_ids()),
        )
    }

    /// Creates a new [`BlockId`] that can be used to create forward references to a block that yet
    /// has to be added to the graph.
    ///
    /// See also [`start_block`].
    pub fn create_block_label(&mut self) -> BlockId {
        self.cfg.insert(cfg::BasicBlock::new_incomplete())
    }

    /// Start a basic block with a previously created [`BlockId`].
    pub fn start_block(&mut self, label: BlockId, arguments: Vec<AnyReg>) -> BBBuilder {
        // TODO: guarantee that arguments are unique?
        BBBuilder {
            id: label,
            arguments,
            instructions: Vec::new(),
        }
    }

    pub fn start_entry_block(&mut self, arguments: Vec<AnyReg>) -> BBBuilder {
        // TODO: guarantee that arguments are unique?
        let succ_id = self.cfg.entry_block().successors().next().unwrap().id;
        BBBuilder {
            id: succ_id,
            arguments,
            instructions: Vec::new(),
        }
    }

    /// Starts a block with a new [`BlockId`]. The id can be retrieved from the returned
    /// [`BBBuilder`].
    pub fn start_new_block(&mut self, arguments: Vec<AnyReg>) -> BBBuilder {
        // TODO: guarantee that arguments are unique?
        let id = self.create_block_label();
        self.start_block(id, arguments)
    }

    /// Adds the provided block to the graph. Returns a reference to it for convenience.
    pub fn add_block(&mut self, block: BasicBlock) -> &cfg::BasicBlock {
        let BasicBlock(id, block) = block;
        if self.cfg[id].is_complete() {
            panic!("attempt to add block with already used label");
        }
        self.cfg[id] = block;
        &self.cfg[id]
    }

    pub fn finish(&mut self) {
        fn declare_params_in_entry_block(function: &mut Function) {
            let mut var_generator = function.cfg.var_generator();
            let arguments = function.entry_block().unwrap().arguments.clone();
            let entry_block = function.cfg.entry_block_mut();

            for ((i, &stack_addr), arg_reg) in function.params.iter().enumerate().zip(arguments) {
                let reg = var_generator.next_of_type(arg_reg);
                match arg_reg {
                    AnyReg::R(arg_reg) => {
                        match function
                            .reference_registers
                            .iter_mut()
                            .find(|r| r.register == arg_reg)
                        {
                            Some(ref_reg) => {
                                let reg = reg.try_into().unwrap();
                                ref_reg.register = reg;
                                function
                                    .reference_register_to_stack_address
                                    .insert(reg, stack_addr);
                            }
                            None => {
                                function.reg_to_stack_address.insert(reg, stack_addr);
                            }
                        }
                    }
                    AnyReg::F(_) => {
                        function.reg_to_stack_address.insert(reg, stack_addr);
                    }
                }
                entry_block
                    .instructions
                    .push(crate::instr::virt::load_from_stack(reg, stack_addr));
                entry_block.successors_mut().next().unwrap().arguments[i] = reg;
            }
        }
        fn insert_loads_for_reference_registers(function: &mut Function) {
            for decl_location in scanner::function::declares(&function.cfg).collect::<Vec<_>>() {
                let reg = match function.cfg.instruction(decl_location) {
                    Some(&Instruction::Virtual(VirtualInstruction::Declare(reg))) => reg,
                    _ => unreachable!(),
                };
                let AnyReg::R(reg) = reg else { continue };
                if let Some(ref_reg) = function
                    .reference_registers
                    .iter()
                    .find(|r| r.register == reg)
                    .cloned()
                {
                    let stack_addr = match function
                        .reference_register_to_stack_address
                        .get(&ref_reg.register)
                    {
                        Some(stack_addr) => *stack_addr,
                        None => {
                            let stack_addr = function.create_stack_address(ref_reg.stack_info);
                            function
                                .reference_register_to_stack_address
                                .insert(ref_reg.register, stack_addr);
                            stack_addr
                        }
                    };
                    *function.cfg.instruction_mut(decl_location).unwrap() =
                        crate::instr::virt::load_stack_address(ref_reg.register, stack_addr);
                }
            }
        }
        declare_params_in_entry_block(self);
        insert_loads_for_reference_registers(self);
    }
}

pub struct Traverser<'a> {
    function: &'a Function,
    /// If `Some(_)`, this will be traversed next. Otherwise a random block from `to_traverse` is
    /// chosen.
    next: Option<BlockId>,
    already_traversed: BTreeSet<BlockId>,
    to_traverse: BTreeSet<BlockId>,
}

impl<'a> Traverser<'a> {
    /// Creates a new traverser over the blocks of `function`, starting from the first block in
    /// `must_traverse`, and always following the default successor if the previously traversed
    /// block has one, and if it's not already traversed. Guaranteed to (also) visit all other
    /// blocks in `must_traverse`. Blocks without predecessors that are not included in
    /// `must_traverse` will not be traversed.
    fn new(function: &'a Function, mut must_traverse: impl Iterator<Item = BlockId>) -> Self {
        let next = must_traverse.next();
        Self {
            function,
            next,
            already_traversed: BTreeSet::new(),
            to_traverse: BTreeSet::from_iter(must_traverse),
        }
    }

    /// Returns `true` if this traverser already traversed the given block.
    pub fn has_traversed(&self, block: BlockId) -> bool {
        self.already_traversed.contains(&block)
    }
}

impl<'a> Iterator for Traverser<'a> {
    type Item = (BlockId, &'a cfg::BasicBlock);

    fn next(&mut self) -> Option<Self::Item> {
        let current_id = self.next.take().or_else(|| {
            self.to_traverse
                .iter()
                .find(|&&b| Some(b) != self.function.exit_block_id)
                .or(self.to_traverse.first())
                .copied()
        })?;
        self.to_traverse.remove(&current_id);
        let current = &self.function.cfg[current_id];
        self.already_traversed.insert(current_id);
        for block in self.function.cfg.successor_ids(current_id) {
            if !self.already_traversed.contains(&block) {
                self.to_traverse.insert(block);
            }
        }
        if let Some(default_successor) = current.terminator().default_target().map(|bref| bref.id) {
            if !self.already_traversed.contains(&default_successor) {
                self.next = Some(default_successor);
            }
        }
        Some((current_id, current))
    }
}

#[derive(Debug)]
pub struct BasicBlock(BlockId, cfg::BasicBlock);

#[derive(Debug)]
pub struct BBBuilder {
    id: BlockId,
    arguments: Vec<AnyReg>,
    instructions: Vec<Instruction>,
}

impl BBBuilder {
    pub fn id(&self) -> BlockId {
        self.id
    }

    pub fn add_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn add_argument(&mut self, arg: AnyReg) {
        self.arguments.push(arg);
    }

    #[must_use = "A BasicBlock nees to be added to a function to do something"]
    pub fn terminate(self, terminator: Terminator) -> BasicBlock {
        BasicBlock(
            self.id,
            cfg::BasicBlock {
                arguments: self.arguments,
                instructions: self.instructions,
                terminator: Some(terminator),
                is_call_block: false,
            },
        )
    }
}
