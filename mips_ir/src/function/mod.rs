// #[cfg(test)]
// mod test;

use crate::cfg::{self, BlockId, Cfg};
use crate::{
    scanner, AlignBoundary, AnyReg, BlockRef, FReg, FunctionCall, Instruction, Label,
    PseudoInstruction, Reg, Terminator, VirtualInstruction,
};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::ops::Range;

/// Used to specify witch registers need to start with pointers to stack allocated space.
#[derive(Debug, Clone)]
pub struct ReferenceRegister {
    pub register: Reg,
    pub stack_info: StackInfo,
}

/// Unique identifier for stack space.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StackAddress(pub u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackInfo {
    pub size: u128,
    pub alignment: AlignBoundary,
    pub signed: bool,
}

/// Assumes the stack is 8-byte aligned. As such, cannot handle alignments bigger than 8. It will
/// treat them incorrectly.
#[derive(Debug, Clone)]
pub struct StackFrame {
    /// `true` when $fp has been set correctly and can be used to refer to the static part of the
    /// stack frame.
    is_fp_set: bool,
    /// The params from the previous stack frame that are passed to us. Ordered from arg1 to argN.
    /// Should always contain at least 4 words, but these might not necessarily have addresses
    /// assigned, so don't need to be in the vec.
    params: Vec<StackInfo>,
    /// Maps a stack address to its index in `params`.
    stack_address_to_param_idx: BTreeMap<StackAddress, usize>,
    /// Ordered from low address to high address. Order can change when adding or removing spilled
    /// vars (in order to reduce inner holes for padding).
    spilled: Vec<StackInfo>,
    /// Maps a stack address to its index in `spilled`.
    stack_address_to_spilled_idx: BTreeMap<StackAddress, usize>,
    /// Ordered from low ($s0) to high ($s7)
    saved_cpu_regs: Vec<Reg>,
    /// Ordered from low ($f20) to high ($f30). Only even register allowed for now. Corresponding
    /// odd registers will automatically be
    saved_fpu_regs: Vec<FReg>,
    /// Does not include outer padding; space in bytes
    max_call_arguments_space: u32,
}

impl StackFrame {
    pub fn new() -> Self {
        Self {
            is_fp_set: false,
            params: Vec::new(),
            stack_address_to_param_idx: BTreeMap::new(),
            spilled: Vec::new(),
            stack_address_to_spilled_idx: BTreeMap::new(),
            saved_cpu_regs: Vec::new(),
            saved_fpu_regs: Vec::new(),
            max_call_arguments_space: 0,
        }
    }

    pub fn mark_fp_as_set(&mut self) {
        self.is_fp_set = true;
    }

    /// Undoes the effect of `mark_fp_as_set`.
    pub fn mark_fp_as_unset(&mut self) {
        self.is_fp_set = false;
    }

    fn static_base(&self) -> Reg {
        match self.is_fp_set {
            true => Reg::FP,
            false => Reg::SP,
        }
    }

    /// Iterates over the saved cpu registers that are saved in this stack frame from the lowest
    /// (e.g. $s0) to the highest (e.g. $s7).
    pub fn saved_cpu_regs(&self) -> impl DoubleEndedIterator<Item = Reg> + '_ {
        self.saved_cpu_regs.iter().copied()
    }

    pub fn saved_fpu_regs(&self) -> impl DoubleEndedIterator<Item = FReg> + '_ {
        self.saved_fpu_regs.iter().copied()
    }

    pub fn fp_slot_addr(&self) -> (Reg, u16) {
        (self.static_base(), self.fp_slot_static_offset_range().start)
    }

    pub fn ra_slot_addr(&self) -> (Reg, u16) {
        (self.static_base(), self.ra_slot_static_offset_range().start)
    }

    pub fn saved_cpu_reg_addr(&self, reg: Reg) -> Option<(Reg, u16)> {
        let position = self.saved_cpu_regs.iter().position(|&r| r == reg)? as u16;
        let offset = self.cpu_reg_save_area_static_offset_range().start
            + crate::size::WORD as u16 * position;
        Some((self.static_base(), offset))
    }

    pub fn saved_fpu_reg_addr(&self, freg: FReg) -> Option<(Reg, u16)> {
        let position = self.saved_fpu_regs.iter().position(|&r| r == freg)? as u16;
        let offset = self.fpu_reg_save_area_static_offset_range().start
            + crate::size::DOUBLE as u16 * position;
        Some((self.static_base(), offset))
    }

    pub fn addr_of(&self, stack_address: StackAddress) -> Option<(Reg, u16)> {
        self.addr_of_spilled(stack_address)
            .or_else(|| self.addr_of_param(stack_address))
    }

    fn addr_of_param(&self, stack_address: StackAddress) -> Option<(Reg, u16)> {
        let index = *self.stack_address_to_param_idx.get(&stack_address)?;
        let mut offset = self.byte_size() as u32;
        // This is not necessary, since `offset` should already be 8-byte aligned at this point.
        offset = self.params[0].alignment.next_multiple_from(offset);
        for i in 1..=index {
            offset += self.params[i - 1].size as u32;
            offset = self.params[i].alignment.next_multiple_from(offset);
        }
        Some((self.static_base(), offset as u16))
    }

    fn addr_of_spilled(&self, stack_address: StackAddress) -> Option<(Reg, u16)> {
        let index = *self.stack_address_to_spilled_idx.get(&stack_address)?;
        let mut offset = self.spilled_area_static_offset_range().start as u32;
        for i in 1..=index {
            offset += self.spilled[i - 1].size as u32;
            offset = self.spilled[i].alignment.next_multiple_from(offset);
        }
        Some((self.static_base(), offset as u16))
    }

    pub fn stack_info(&self, stack_address: StackAddress) -> Option<&StackInfo> {
        self.stack_address_to_spilled_idx
            .get(&stack_address)
            .map(|&idx| &self.spilled[idx])
            .or_else(|| {
                self.stack_address_to_param_idx
                    .get(&stack_address)
                    .map(|&idx| &self.params[idx])
            })
    }

    pub fn call_arg_addrs<'a, I>(&self, call_args: I) -> impl Iterator<Item = (Reg, u16)> + 'a
    where
        I: Iterator<Item = &'a StackInfo> + Clone + 'a,
    {
        struct Tmp<'a, I: Iterator<Item = &'a StackInfo>> {
            offset: u16,
            call_args: I,
        }

        impl<'a, I: Iterator<Item = &'a StackInfo>> Iterator for Tmp<'a, I> {
            type Item = (Reg, u16);

            fn next(&mut self) -> Option<Self::Item> {
                let stack_info = self.call_args.next()?;
                self.offset = stack_info.alignment.next_multiple_from(self.offset as u32) as u16;
                let offset = self.offset;
                self.offset += stack_info.size as u16;
                Some((Reg::SP, offset))
            }
        }

        Tmp {
            offset: self.call_arguments_area_static_offset_range().start,
            call_args,
        }
    }

    /// Returns the total size in bytes of this stack frame, including padding
    /// (always multiple of 8).
    pub fn byte_size(&self) -> u16 {
        self.ra_slot_static_offset_range().end
    }

    fn call_arguments_area_static_offset_range(&self) -> Range<u16> {
        0..(self.max_call_arguments_space as u16)
    }

    fn fpu_reg_save_area_static_offset_range(&self) -> Range<u16> {
        let prev_end = self.call_arguments_area_static_offset_range().end;
        let size = self.saved_fpu_regs.len() as u32 * crate::size::DOUBLE;
        if size == 0 {
            return prev_end..prev_end;
        }
        let start = AlignBoundary::DOUBLE.next_multiple_from(prev_end as u32) as u16;
        start..(start + size as u16)
    }

    fn cpu_reg_save_area_static_offset_range(&self) -> Range<u16> {
        let prev_end = self.fpu_reg_save_area_static_offset_range().end;
        let size = self.saved_cpu_regs.len() as u32 * crate::size::WORD;
        if size == 0 {
            return prev_end..prev_end;
        }
        let start = AlignBoundary::DOUBLE.next_multiple_from(prev_end as u32) as u16;
        start..(start + size as u16)
    }

    fn spilled_area_static_offset_range(&self) -> Range<u16> {
        let prev_end = self.cpu_reg_save_area_static_offset_range().end;
        if self.spilled.is_empty() {
            return prev_end..prev_end;
        }
        let start = self.spilled[0]
            .alignment
            .next_multiple_from(prev_end as u32);
        let mut end = start + self.spilled[0].size as u32;
        for stack_info in &self.spilled[1..] {
            end = stack_info.alignment.next_multiple_from(end);
            end += stack_info.size as u32;
        }
        (start as u16)..(end as u16)
    }

    fn fp_slot_static_offset_range(&self) -> Range<u16> {
        let prev_end = self.spilled_area_static_offset_range().end;
        let start = AlignBoundary::DOUBLE.next_multiple_from(prev_end as u32) as u16;
        start..(start + crate::size::WORD as u16)
    }

    fn ra_slot_static_offset_range(&self) -> Range<u16> {
        let prev_end = self.fp_slot_static_offset_range().end;
        let start = AlignBoundary::WORD.next_multiple_from(prev_end as u32) as u16;
        start..(start + crate::size::WORD as u16)
    }

    pub fn add_saved_cpu_reg(&mut self, reg: Reg) {
        match reg {
            Reg::S0 | Reg::S1 | Reg::S2 | Reg::S3 | Reg::S4 | Reg::S5 | Reg::S6 | Reg::S7 => {
                let position = self
                    .saved_cpu_regs
                    .iter()
                    .position(|r| reg.phy_num() <= r.phy_num());
                match position.map(|i| (i, self.saved_cpu_regs[i])) {
                    Some((_, r)) if r == reg => (),
                    Some((i, _)) => self.saved_cpu_regs.insert(i, reg),
                    None => self.saved_cpu_regs.push(reg),
                }
            }
            _ => panic!("attempt to save virtual or non-saved cpu register"),
        }
    }

    /// This must be a double-precision fp register for now
    pub fn add_saved_fpu_reg(&mut self, freg: FReg) {
        match freg {
            FReg::F(20..=31) => {
                if !freg.is_double() {
                    panic!("can only save double-precision fpu registers for now");
                }
                let position = self
                    .saved_fpu_regs
                    .iter()
                    .position(|r| freg.phy_num() <= r.phy_num());
                match position.map(|i| (i, self.saved_fpu_regs[i])) {
                    Some((_, r)) if r == freg => (),
                    Some((i, _)) => self.saved_fpu_regs.insert(i, freg),
                    None => self.saved_fpu_regs.push(freg),
                }
            }
            _ => panic!("attempt to save virtual or non-saved fpu register"),
        }
    }

    pub fn add_spilled(
        &mut self,
        stack_info: StackInfo,
        stack_addresses: impl Iterator<Item = StackAddress>,
    ) {
        // Just append for now.
        let index = self.spilled.len();
        self.spilled.push(stack_info);
        for stack_address in stack_addresses {
            self.stack_address_to_spilled_idx
                .insert(stack_address, index);
            self.stack_address_to_param_idx.remove(&stack_address);
        }
    }

    pub fn add_param(&mut self, stack_info: StackInfo, stack_address: StackAddress) {
        let index = self.params.len();
        self.params.push(stack_info);
        self.stack_address_to_param_idx.insert(stack_address, index);
        self.stack_address_to_spilled_idx.remove(&stack_address);
    }

    pub fn set_max_call_argument_space(&mut self, space: u32) {
        self.max_call_arguments_space = space;
    }
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        crate::MipsOutputter::new(f)
            .with_config(crate::MipsOutputConfig {
                use_register_names: true,
                allow_virtuals: true,
                allow_hidden_instructions: true,
                show_block_arguments: true,
                show_all_blocks: true,
                show_comments: false,
            })
            .write_function(self)
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
#[derive()]
pub struct Function {
    label: Label,
    // TODO: make this pub(crate)
    pub cfg: Cfg,
    pub(crate) exit_block_id: Option<BlockId>,
    /// The registers here are expected to have references to places on the stack before the
    /// entry block is started.
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

    /// Returns a label for the given block. This label remains only valid while this function is
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

    /// Returns `true` if the graph is completely empty, i.e. if doesn't contain any blocks. This
    /// will return `false` if the graph has any blocks even if the entry block is not set.
    ///
    /// To just check whether the graph is considered empty, use `self.entry_block().is_none()`.
    // pub fn is_empty(&self) -> bool {
    //     let eb = self.cfg.entry_block_id();
    //     !self.cfg.blocks().any(|(id, b)| id != eb && b.is_complete())
    // }

    /// Iterate over the id's of all blocks in the graph in an undefined order.
    fn block_ids(&self) -> impl Iterator<Item = BlockId> + '_ {
        self.cfg
            .blocks()
            .filter_map(|(id, block)| block.is_complete().then_some(id))
    }

    // /// Iterate over all blocks in the graph in an undefined order.
    // pub fn blocks(&self) -> impl Iterator<Item = &cfg::BasicBlock> {
    //     let eb = self.cfg.entry_block_id();
    //     self.cfg
    //         .blocks()
    //         .filter_map(move |(id, block)| (id != eb && block.is_complete()).then_some(block))
    // }

    // /// Iterate over all blocks in the graph in an undefined order.
    // pub fn blocks_mut(&mut self) -> impl Iterator<Item = &mut cfg::BasicBlock> {
    //     let eb = self.cfg.entry_block_id();
    //     self.cfg
    //         .blocks_mut()
    //         .filter_map(move |(id, block)| (id != eb && block.is_complete()).then_some(block))
    // }

    // pub fn reference_registers(&self) -> &[ReferenceRegister] {
    //     &self.reference_registers
    // }

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

    // pub(crate) fn stack_info_mut(&mut self, stack_address: StackAddress) -> &mut StackInfo {
    //     self.stack_info.get_mut(&stack_address).unwrap()
    // }

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

    // pub fn bfs(&self, start: BlockId) -> BfsTraverser {
    //     BfsTraverser::new(self, start)
    // }

    // /// Returns a reference to the block identified by `id`, or `None` if the block isn't (yet)
    // /// added to the graph.
    // pub fn get_block(&self, id: BlockId) -> Option<&BasicBlock> {
    //     self.blocks.get(&id)
    // }

    // /// Returns a mutable reference to the block identified by `id`, or `None` if the block isn't
    // /// (yet) added to the graph.
    // // This is safe, since the label of a block can't be changed.
    // pub fn get_block_mut(&mut self, id: BlockId) -> Option<&mut BasicBlock> {
    //     self.blocks.get_mut(&id)
    // }

    // /// Returns `true` if a block with `id` has been added to the graph.
    // pub fn has_block(&self, id: BlockId) -> bool {
    //     self.blocks.contains_key(&id)
    // }

    /// Creates a new block label that can be used to create forward references to block that yet
    /// have to be added to the graph.
    ///
    /// See also [`start_block`].
    pub fn create_block_label(&mut self) -> BlockId {
        self.cfg.insert(cfg::BasicBlock::new_incomplete())
    }

    /// Start a basic block with a previously created label.
    // TODO: guarantee that arguments are unique?
    pub fn start_block(&mut self, label: BlockId, arguments: Vec<AnyReg>) -> BBBuilder {
        BBBuilder {
            label,
            arguments,
            instructions: Vec::new(),
        }
    }

    pub fn start_entry_block(&mut self, arguments: Vec<AnyReg>) -> BBBuilder {
        let succ_id = self.cfg.entry_block().successors().next().unwrap().id;
        BBBuilder {
            label: succ_id,
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
    pub fn add_block(&mut self, block: BasicBlock) -> &cfg::BasicBlock {
        if self.cfg[block.id].is_complete() {
            panic!("attempt to add block with already used label");
        }
        self.cfg[block.id] = cfg::BasicBlock {
            arguments: block.arguments,
            instructions: block.instructions,
            terminator: Some(block.terminator),
            is_call_block: false,
        };
        &self.cfg[block.id]
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

    // /// Removes the block with the given id from the graph. Returns the removed [`BasicBlock`].
    // /// If this was the entry block, the graph's entry block will be unset.
    // pub fn remove_block(&mut self, id: BlockId) -> BasicBlock {
    //     if self.entry_block == Some(id) {
    //         self.entry_block = None;
    //     }
    //     self.blocks
    //         .remove(&id)
    //         .expect("attempt to remove block that is not in the graph")
    // }

    // /// Marks a block as the graph's entry block. Panics if the block is not added to the graph.
    // pub fn set_entry_block(&mut self, id: BlockId) {
    //     // if !self.has_block(id) {
    //     //     panic!("attempt to set nonexistent block as entry block");
    //     // }
    //     if self.cfg.entry_block().is_complete() {
    //         self.cfg.set_entry_block(id)
    //     } else {
    //         let prev_entry_block_id = self.cfg.entry_block_id();
    //         self.cfg.set_entry_block(id);
    //         self.cfg.remove(prev_entry_block_id);
    //     }
    // }

    // /// Returns `true` if the specified global label is referenced anywhere in this function.
    // pub fn references_label(&self, label: &Label) -> bool {
    //     self.blocks().any(|b| b.references_label(label))
    // }

    // /// Returns all the global labels that are referenced anywhere in this function.
    // pub fn referenced_labels(&self) -> impl Iterator<Item = &Label> {
    //     self.blocks().flat_map(|b| b.referenced_labels())
    // }

    // pub fn calls_function(&self, label: &Label) -> bool {
    //     self.blocks().any(|b| b.calls_function(label))
    // }

    // pub fn function_calls(&self) -> impl Iterator<Item = &FunctionCall> {
    //     self.cfg.blocks().flat_map(|(_, b)| b.function_calls())
    // }
}

// impl std::ops::Index<BlockId> for Function {
//     type Output = BasicBlock;

//     fn index(&self, index: BlockId) -> &Self::Output {
//         &self.blocks[&index]
//     }
// }

// impl std::ops::IndexMut<BlockId> for Function {
//     // This is safe, since the label of a block can't be changed.
//     fn index_mut(&mut self, index: BlockId) -> &mut Self::Output {
//         self.blocks.get_mut(&index).unwrap()
//     }
// }

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

// pub struct BfsTraverser<'a> {
//     function: &'a Function,
//     visited: BTreeSet<BlockId>,
//     // This might include blocks that are already in `visited`. These will be skipped.
//     to_visit: VecDeque<(u32, BlockId)>,
// }

// impl<'a> BfsTraverser<'a> {
//     pub fn new(function: &'a Function, start: BlockId) -> Self {
//         Self {
//             function,
//             visited: BTreeSet::new(),
//             to_visit: VecDeque::from([(0, start)]),
//         }
//     }
// }

// impl<'a> Iterator for BfsTraverser<'a> {
//     type Item = (u32, &'a BasicBlock);

//     fn next(&mut self) -> Option<Self::Item> {
//         while let Some((depth, next)) = self.to_visit.pop_front() {
//             let Some(block) = self.function.get_block(next) else { continue };
//             if self.visited.insert(next) {
//                 for succ_id in block.succidxs().map(|idx| block.succ_id(idx)) {
//                     if !self.visited.contains(&succ_id) {
//                         self.to_visit.push_back((depth + 1, succ_id));
//                     }
//                 }
//                 return Some((depth, block));
//             }
//         }
//         None
//     }
// }

#[derive(Debug)]
pub struct BasicBlock {
    id: BlockId,
    // TODO: guarantee that arguments are unique?
    arguments: Vec<AnyReg>,
    instructions: Vec<Instruction>,
    terminator: Terminator,
}

impl BasicBlock {
    #[inline]
    pub fn id(&self) -> BlockId {
        self.id
    }

    /// Returns `true` if the specified global label is referenced anywhere in this block.
    pub fn references_label(&self, label: &Label) -> bool {
        let mut has_ref = false;
        for instr in &self.instructions {
            match instr {
                Instruction::Call(target) => has_ref |= target == label,
                Instruction::Pseudo(pseudo) => match pseudo {
                    PseudoInstruction::LoadAddress(_, lbl) => has_ref |= lbl == label,
                },
                Instruction::Virtual(VirtualInstruction::FunctionCall(FunctionCall {
                    label: lbl,
                    ..
                })) => has_ref |= lbl == label,
                Instruction::Virtual(_)
                | Instruction::Nop
                | Instruction::Reg3(_, _, _, _)
                | Instruction::Reg2(_, _, _)
                | Instruction::Reg1(_, _)
                | Instruction::Imm2(_, _, _, _)
                | Instruction::Mem(_, _, _, _)
                | Instruction::Imm1(_, _, _)
                | Instruction::FReg3(_, _, _, _)
                | Instruction::FReg2(_, _, _)
                | Instruction::FImm(_, _, _, _)
                | Instruction::MoveFromFpu(_, _)
                | Instruction::MoveToFpu(_, _)
                | Instruction::Break
                | Instruction::Hidden(_)
                | Instruction::Comment(_) => {}
            }
        }
        match &self.terminator {
            Terminator::BranchIfZAndLink(_, _, target, _) => has_ref |= target == label,
            Terminator::BranchIf(_, _, _, _, _)
            | Terminator::BranchIfZ(_, _, _, _)
            | Terminator::BranchIfFCond(_, _, _)
            | Terminator::Jump(_)
            | Terminator::ReturnToRa
            | Terminator::Syscall(_)
            | Terminator::JumpAndLinkRa(_, _)
            | Terminator::JumpAndLinkReg(_, _, _)
            | Terminator::Virtual(_) => {}
        }
        has_ref
    }

    /// Returns all the global labels that are referenced anywhere in this block.
    pub fn referenced_labels(&self) -> impl Iterator<Item = &Label> {
        self.instructions
            .iter()
            .filter_map(|instr| match instr {
                Instruction::Call(target) => Some(target),
                Instruction::Pseudo(pseudo) => match pseudo {
                    PseudoInstruction::LoadAddress(_, label) => Some(label),
                },
                Instruction::Virtual(VirtualInstruction::FunctionCall(FunctionCall {
                    label,
                    ..
                })) => Some(label),
                _ => None,
            })
            .chain(match &self.terminator {
                Terminator::BranchIfZAndLink(_, _, target, _) => Some(target),
                _ => None,
            })
    }

    /// Only takes virtual function calls into account, to match what is returned by
    /// `function_calls`.
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
    label: BlockId,
    arguments: Vec<AnyReg>,
    instructions: Vec<Instruction>,
}

impl BBBuilder {
    pub fn id(&self) -> BlockId {
        self.label
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
            id: self.label,
            arguments: self.arguments,
            instructions: self.instructions,
            terminator,
        }
    }
}
