//! Control-Flow Graph

// Contains impl's for `Terminator`.
mod terminator;

use crate::{
    dfa::{
        liveness,
        uda::{self, GlobalLocation},
    },
    AnyReg, Instruction, Terminator, VARGenerator,
};
use generational_arena::{Arena, Index as ArenaIndex};
use std::collections::BTreeMap;
use std::{cell::RefCell, collections::BTreeSet};

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub arguments: Vec<AnyReg>,
    pub instructions: Vec<Instruction>,
    pub(crate) terminator: Option<Terminator>,
    pub(crate) is_call_block: bool,
}

impl BasicBlock {
    pub(crate) fn new_incomplete() -> Self {
        Self {
            arguments: Vec::new(),
            instructions: Vec::new(),
            terminator: None,
            is_call_block: false,
        }
    }

    pub(crate) fn is_complete(&self) -> bool {
        self.terminator.is_some()
    }

    pub fn terminator(&self) -> &Terminator {
        self.terminator
            .as_ref()
            .expect("invalid basic block terminator")
    }

    pub fn terminator_mut(&mut self) -> &mut Terminator {
        self.terminator
            .as_mut()
            .expect("invalid basic block terminator")
    }

    pub fn n_successors(&self) -> usize {
        self.terminator().targets().count()
    }

    pub fn successors(&self) -> impl Iterator<Item = &BlockRef> {
        self.terminator().targets()
    }

    pub fn successors_mut(&mut self) -> impl Iterator<Item = &mut BlockRef> {
        self.terminator_mut().targets_mut()
    }

    pub fn successor(&self, id: BlockId) -> Option<&BlockRef> {
        self.terminator().target(id)
    }

    pub fn successor_mut(&mut self, id: BlockId) -> Option<&mut BlockRef> {
        self.terminator_mut().target_mut(id)
    }
}

type Predecessors = BTreeMap<BlockId, Vec<BlockId>>;

#[derive(Debug, Default)]
struct Cache {
    predecessors: Option<Predecessors>,
    dominator_tree: Option<DominatorTree>,
    du_chains: Option<uda::DuChains>,
    live_sets: Option<liveness::LiveSets>,
    var_generator: Option<VARGenerator>,
}

impl Cache {
    fn invalidate(&mut self) {
        *self = Self::default()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(ArenaIndex);

impl BlockId {
    pub fn as_unique_id(self) -> u128 {
        let (a, b) = self.0.into_raw_parts();
        ((a as u128) << 64) | b as u128
    }

    pub(crate) fn as_id_in_cfg_of_len(self, len: usize) -> u128 {
        let (a, b) = self.0.into_raw_parts();
        b as u128 * len as u128 + a as u128
    }
}

#[derive(Debug, Clone)]
pub struct BlockRef {
    pub id: BlockId,
    pub arguments: Vec<AnyReg>,
}

impl BlockRef {
    pub fn new(id: BlockId, arguments: Vec<AnyReg>) -> Self {
        Self { id, arguments }
    }
}

#[derive(Debug)]
pub struct Cfg {
    entry_block: BlockId,
    blocks: Arena<BasicBlock>,
    cache: RefCell<Cache>,
}

impl Cfg {
    pub fn new(entry_block: BasicBlock) -> Self {
        let mut blocks = Arena::new();
        Self {
            entry_block: BlockId(blocks.insert(entry_block)),
            blocks,
            cache: RefCell::new(Cache::default()),
        }
    }

    /// Returns the number of blocks in the CFG. This includes incomplete blocks s.a. the entry
    /// block after constrution with [`new`].
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.blocks.len()
    }

    pub fn entry_block_id(&self) -> BlockId {
        self.entry_block
    }

    /// Sets the `new_entry_block` as the new entry block. Panics if that block isn't in the CFG.
    pub fn set_entry_block(&mut self, new_entry_block: BlockId) {
        self.invalidate_cache();
        if !self.blocks.contains(new_entry_block.0) {
            panic!("setting unknown block as entry block");
        }
        self.entry_block = new_entry_block;
    }

    pub fn entry_block(&self) -> &BasicBlock {
        &self[self.entry_block]
    }

    pub fn entry_block_mut(&mut self) -> &mut BasicBlock {
        let id = self.entry_block;
        &mut self[id]
    }

    /// Retrieves a reference to an existing block in the CFG by its id.
    pub fn get(&self, block: BlockId) -> Option<&BasicBlock> {
        self.blocks.get(block.0)
    }

    /// Retrieves a mutable reference to an existing block in the CFG by its id.
    pub fn get_mut(&mut self, block: BlockId) -> Option<&mut BasicBlock> {
        self.invalidate_cache();
        self.blocks.get_mut(block.0)
    }

    /// Returns a reference to the instruction at `location`.
    pub fn instruction(&self, location: GlobalLocation) -> Option<&Instruction> {
        if location.local.0 < 0 {
            return None;
        }
        self[location.block_id]
            .instructions
            .get(location.local.0 as usize)
    }

    pub fn instruction_mut(&mut self, location: GlobalLocation) -> Option<&mut Instruction> {
        if location.local.0 < 0 {
            return None;
        }
        self[location.block_id]
            .instructions
            .get_mut(location.local.0 as usize)
    }

    /// Inserts a new block in the CFG and returns it id.
    pub fn insert(&mut self, block: BasicBlock) -> BlockId {
        self.invalidate_cache();
        BlockId(self.blocks.insert(block))
    }

    /// Removes a block from the CFG and returns it. Any predecessors or successor reference will
    /// **NOT** be updated. This can be relied upon.
    pub fn remove(&mut self, block: BlockId) -> BasicBlock {
        self.invalidate_cache();
        self.blocks.remove(block.0).unwrap()
    }

    /// Returns an iterator over all blocks in the CFG.
    pub fn blocks(&self) -> impl Iterator<Item = (BlockId, &BasicBlock)> {
        self.blocks.iter().map(|(idx, bb)| (BlockId(idx), bb))
    }

    /// Returns an iterator over all blocks in the CFG.
    pub fn blocks_mut(&mut self) -> impl Iterator<Item = (BlockId, &mut BasicBlock)> {
        self.invalidate_cache();
        self.blocks.iter_mut().map(|(idx, bb)| (BlockId(idx), bb))
    }

    pub fn n_successors(&self, block: BlockId) -> usize {
        self[block].n_successors()
    }

    pub fn successor_ids(&self, block: BlockId) -> impl Iterator<Item = BlockId> + '_ {
        self[block].successors().map(|bref| bref.id)
    }

    pub fn successors(&self, block: BlockId) -> impl Iterator<Item = (BlockId, &BasicBlock)> {
        self.successor_ids(block).map(|id| (id, &self[id]))
    }

    /// Returns the number of predecessors the given block has.
    pub fn n_predecessors(&self, block: BlockId) -> usize {
        if let Some(predecessors) = &self.cache.borrow().predecessors {
            return predecessors[&block].len();
        }
        let predecessors = self.build_predecessors();
        self.cache
            .borrow_mut()
            .predecessors
            .get_or_insert(predecessors)[&block]
            .len()
    }

    pub fn predecessor_ids(&self, block: BlockId) -> impl Iterator<Item = BlockId> {
        if let Some(predecessors) = &self.cache.borrow().predecessors {
            return predecessors[&block].clone().into_iter();
        }
        let predecessors = self.build_predecessors();
        self.cache
            .borrow_mut()
            .predecessors
            .get_or_insert(predecessors)[&block]
            .clone()
            .into_iter()
    }

    pub fn predecessors(&self, block: BlockId) -> impl Iterator<Item = (BlockId, &BasicBlock)> {
        self.predecessor_ids(block).map(|id| (id, &self[id]))
    }

    /// This doesn't impose any requirements on the CFG, except for it having a complete
    /// entry block, and for all blocks reachable from the entry block to be complete.
    pub fn dominator_tree(&self) -> DominatorTree {
        if let Some(dominator_tree) = &self.cache.borrow().dominator_tree {
            return dominator_tree.clone();
        }
        let dominator_tree = DominatorTree::build_from(self);
        self.cache
            .borrow_mut()
            .dominator_tree
            .get_or_insert(dominator_tree)
            .clone()
    }

    pub fn dominance_frontiers(&self) -> BTreeMap<BlockId, BTreeSet<BlockId>> {
        let mut dominance_frontiers: BTreeMap<BlockId, BTreeSet<BlockId>> =
            self.blocks().map(|(id, _)| (id, BTreeSet::new())).collect();

        let dominator_tree = self.dominator_tree();
        for (block_id, _) in self.blocks() {
            if self.n_predecessors(block_id) < 2 {
                continue;
            }
            let idom = dominator_tree.immediate_dominator(block_id).unwrap();
            for (pred_id, _) in self.predecessors(block_id) {
                let mut runner = pred_id;
                while runner != idom {
                    dominance_frontiers
                        .get_mut(&runner)
                        .unwrap()
                        .insert(block_id);
                    runner = dominator_tree.immediate_dominator(runner).unwrap();
                }
            }
        }

        dominance_frontiers
    }

    pub fn du_chain(&self, reg: AnyReg) -> uda::DuChain {
        if let Some(du_chains) = &self.cache.borrow().du_chains {
            return du_chains[reg].clone();
        }
        let du_chains = uda::DuChains::try_build_from(self).expect("CFG in invalid state");
        self.cache.borrow_mut().du_chains.get_or_insert(du_chains)[reg].clone()
    }

    pub fn du_chains(&self) -> uda::DuChains {
        if let Some(du_chains) = &self.cache.borrow().du_chains {
            return du_chains.clone();
        }
        let du_chains = uda::DuChains::try_build_from(self).expect("CFG in invalid state");
        self.cache
            .borrow_mut()
            .du_chains
            .get_or_insert(du_chains)
            .clone()
    }

    pub fn live_set(&self, block: BlockId) -> liveness::LiveSet {
        if let Some(live_sets) = &self.cache.borrow().live_sets {
            return live_sets[block].clone();
        }
        let live_sets = liveness::LiveSets::build_from(self);
        self.cache.borrow_mut().live_sets.get_or_insert(live_sets)[block].clone()
    }

    pub fn live_sets(&self) -> liveness::LiveSets {
        if let Some(live_sets) = &self.cache.borrow().live_sets {
            return live_sets.clone();
        }
        let live_sets = liveness::LiveSets::build_from(self);
        self.cache
            .borrow_mut()
            .live_sets
            .get_or_insert(live_sets)
            .clone()
    }

    pub fn var_generator(&self) -> VARGenerator {
        if let Some(var_generator) = &self.cache.borrow().var_generator {
            return var_generator.clone();
        }
        let var_generator = self.du_chains().var_generator();
        self.cache
            .borrow_mut()
            .var_generator
            .get_or_insert(var_generator)
            .clone()
    }

    pub fn remove_param(&mut self, block_id: BlockId, param_idx: usize) {
        self[block_id].arguments.remove(param_idx);
        for (_, block) in self.blocks_mut() {
            for bref in block.successors_mut() {
                if bref.id == block_id {
                    bref.arguments.remove(param_idx);
                }
            }
        }
    }

    /// Will try to insert the `instruction` on the edge between `pred` and `succ`. A new block that
    /// connects `pred` and `succ` will always be inserted (and returned). This block will only
    /// contain the provided instructions.
    pub fn insert_on_edge(
        &mut self,
        pred: BlockId,
        succ: BlockId,
        instructions: &[Instruction],
    ) -> BlockId {
        let mut var_generator = self.var_generator();
        let arguments: Vec<_> = self[succ]
            .arguments
            .iter()
            .map(|&reg| var_generator.next_of_type(reg))
            .collect();
        let connector_block = BasicBlock {
            arguments: arguments.clone(),
            instructions: instructions.to_vec(),
            terminator: Some(crate::term::jump(BlockRef::new(succ, arguments))),
            is_call_block: false,
        };
        let connector_id = self.insert(connector_block);
        self[pred].successor_mut(succ).unwrap().id = connector_id;
        connector_id
    }

    fn build_predecessors(&self) -> Predecessors {
        let mut predecessors =
            Predecessors::from_iter(self.blocks().map(|(id, _)| (id, Vec::new())));
        for (id, _) in self.blocks() {
            for succ in self.successor_ids(id) {
                match predecessors.get_mut(&succ) {
                    Some(preds) => preds.push(id),
                    None => {
                        let out_id = id.as_id_in_cfg_of_len(self.len());
                        panic!("cannot build predecessors: block {out_id} has successor not in the CFG")
                    }
                }
            }
        }
        predecessors
    }

    fn invalidate_cache(&self) {
        self.cache.borrow_mut().invalidate();
    }
}

impl std::ops::Index<BlockId> for Cfg {
    type Output = BasicBlock;

    fn index(&self, index: BlockId) -> &Self::Output {
        &self.blocks[index.0]
    }
}

impl std::ops::IndexMut<BlockId> for Cfg {
    fn index_mut(&mut self, index: BlockId) -> &mut Self::Output {
        self.invalidate_cache();
        &mut self.blocks[index.0]
    }
}

#[derive(Debug, Clone)]
pub struct DominatorTree {
    entry_block: BlockId,
    /// Maps each block to its immediate dominator (idom).
    immediate_dominators: BTreeMap<BlockId, Option<BlockId>>,
    /// Maps each block to the blocks it is the idom of.
    rev_immediate_dominators: BTreeMap<BlockId, BTreeSet<BlockId>>,
}

impl DominatorTree {
    fn build_from(cfg: &Cfg) -> Self {
        // TODO: use a more efficent algorith for this, s.a. the Lengauer-Tarjan one.

        // Slow quadratic algorithm from Wikipedia
        let mut dominators = BTreeMap::new();
        dominators.insert(cfg.entry_block, BTreeSet::from([cfg.entry_block]));
        for (block, _) in cfg.blocks() {
            if block == cfg.entry_block {
                continue;
            }
            dominators.insert(block, BTreeSet::from_iter(cfg.blocks().map(|(id, _)| id)));
        }
        {
            let mut changing = true;
            while changing {
                changing = false;
                for (block, _) in cfg.blocks() {
                    if block == cfg.entry_block {
                        continue;
                    }
                    let mut pred_doms = cfg.predecessor_ids(block).map(|pred| &dominators[&pred]);
                    let Some(first_pred_doms) = pred_doms.next() else { continue };
                    let mut new_dominators = BTreeSet::from_iter(first_pred_doms.iter().copied());
                    for doms in pred_doms {
                        new_dominators.retain(|r| doms.contains(r))
                    }
                    new_dominators.insert(block);
                    if dominators[&block] != new_dominators {
                        changing = true;
                    }
                    dominators.insert(block, new_dominators);
                }
            }
        }

        // Keep only the strict dominators
        for (block, doms) in &mut dominators {
            doms.remove(block);
        }

        let mut immediate_dominators = BTreeMap::from_iter(cfg.blocks().map(|(id, _)| (id, None)));
        let mut rev_immediate_dominators =
            BTreeMap::from_iter(cfg.blocks().map(|(id, _)| (id, BTreeSet::new())));

        while let Some(block) = dominators
            .iter()
            .find_map(|(b, doms)| doms.is_empty().then_some(*b))
        {
            dominators.remove(&block);
            for (&b, doms) in &mut dominators {
                let removed = doms.remove(&block);
                if removed && doms.is_empty() {
                    let existing_idom = immediate_dominators.insert(b, Some(block)).unwrap();
                    if existing_idom.is_some() {
                        panic!("every node should have at most one immediate dominator");
                    }
                    rev_immediate_dominators.get_mut(&block).unwrap().insert(b);
                }
            }
        }

        Self {
            entry_block: cfg.entry_block,
            immediate_dominators,
            rev_immediate_dominators,
        }
    }

    /// Returns the immediate dominator (idom) of the block.
    ///
    /// From Wikipedia:
    /// > The _immediate dominator_ or **idom** of a node n is the unique node that strictly
    /// > dominates n but does not strictly dominate any other node that strictly dominates n. Every
    /// > node, except the entry node, has an immediate dominator.
    pub fn immediate_dominator(&self, block: BlockId) -> Option<BlockId> {
        self.immediate_dominators[&block]
    }

    /// Returns an iterator over the block's strict dominators, starting from its immediate
    /// dominator.
    pub fn strict_dominators(&self, block: BlockId) -> impl Iterator<Item = BlockId> + '_ {
        Dominators {
            tree: self,
            current: self.immediate_dominator(block),
        }
    }

    /// Returns an iterator over the block's dominators, starting from the block itself.
    pub fn dominators(&self, block: BlockId) -> impl Iterator<Item = BlockId> + '_ {
        Dominators {
            tree: self,
            current: Some(block),
        }
    }

    /// Returns `true` if the block is reachable from the entry block.
    pub fn is_reachable(&self, block: BlockId) -> bool {
        block == self.entry_block || self.immediate_dominators[&block].is_some()
    }

    pub fn dominates(&self, dominator: BlockId, dominated: BlockId) -> bool {
        self.dominators(dominated).any(|block| block == dominator)
    }

    pub fn strictly_dominates(&self, dominator: BlockId, dominated: BlockId) -> bool {
        self.strict_dominators(dominated)
            .any(|block| block == dominator)
    }

    pub fn dfs_preorder(&self) -> DominatorTreeDfsPreorder {
        self.dfs_preorder_from(self.entry_block)
    }

    pub fn dfs_preorder_from(&self, start: BlockId) -> DominatorTreeDfsPreorder {
        DominatorTreeDfsPreorder::new(self, start)
    }

    pub fn first_common_dominator(
        &self,
        mut blocks: impl Iterator<Item = BlockId>,
    ) -> Option<BlockId> {
        let first_block = blocks.next()?;
        let mut common_dominators: BTreeSet<BlockId> =
            BTreeSet::from_iter(self.dominators(first_block));
        for block in blocks {
            let dominators: BTreeSet<BlockId> = self.dominators(block).collect();
            common_dominators.retain(|block| dominators.contains(block));
        }
        self.dominators(first_block)
            .find(|b| common_dominators.contains(b))
    }
}

#[derive(Debug)]
struct Dominators<'a> {
    tree: &'a DominatorTree,
    current: Option<BlockId>,
}

impl Iterator for Dominators<'_> {
    type Item = BlockId;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.current.take()?;
        self.current = self.tree.immediate_dominator(current);
        Some(current)
    }
}

#[derive(Debug)]
pub struct DominatorTreeDfsPreorder<'a> {
    tree: &'a DominatorTree,
    stack: Vec<BlockId>,
}

impl<'a> DominatorTreeDfsPreorder<'a> {
    fn new(tree: &'a DominatorTree, start: BlockId) -> Self {
        Self {
            tree,
            stack: vec![start],
        }
    }
}

impl Iterator for DominatorTreeDfsPreorder<'_> {
    type Item = BlockId;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.stack.pop()?;
        self.stack
            .extend(self.tree.rev_immediate_dominators[&current].iter().rev());
        Some(current)
    }
}
