//! Liveness Analysis
//!
//! See https://inria.hal.science/inria-00558509v2 for the paper on which this implementation is
//! based.
//!
//! For phi defs (formal block arguments) and phi uses (actual block arguments) the following rules
//! are used (as in the paper mentioned above):
//!
//!  - phi defs are considered live-in for the succ block
//!  - phi defs are not considered live-out for the pred block with respect to the succ block
//!    (i.e., if they are live-in in another succ, they will be live-out for the pred block)
//!  - phi uses are not considered live-in for the succ block with respect to the pred block
//!    (i.e., if they are live-out in another pred, they will be live-in for the succ block)
//!  - phi uses are considered live-out for the pred block
//!
//! Non-virtual regs are completely ignored!

use crate::{
    cfg::{BlockId, Cfg},
    AnyReg,
};
use std::collections::{BTreeMap, HashSet};

use crate::dfa::uda;

#[derive(Debug, Clone, Default)]
pub struct LiveSets(BTreeMap<BlockId, LiveSet>);

impl std::ops::Index<BlockId> for LiveSets {
    type Output = LiveSet;

    fn index(&self, index: BlockId) -> &Self::Output {
        &self.0[&index]
    }
}

impl std::ops::IndexMut<BlockId> for LiveSets {
    fn index_mut(&mut self, index: BlockId) -> &mut Self::Output {
        self.0.get_mut(&index).unwrap()
    }
}

#[derive(Debug, Clone, Default)]
pub struct LiveSet {
    pub live_ins: HashSet<AnyReg>,
    pub live_outs: HashSet<AnyReg>,
}

impl LiveSets {
    pub fn build_from(cfg: &Cfg) -> Self {
        LiveSetsBuilder::new(cfg).build()
    }
}

struct LiveSetsBuilder<'a> {
    cfg: &'a Cfg,
    du_chains: uda::DuChains,
    live_sets: LiveSets,
}

impl<'a> LiveSetsBuilder<'a> {
    fn new(cfg: &'a Cfg) -> Self {
        Self {
            cfg,
            du_chains: cfg.du_chains(),
            // Insert an empty live set for each block.
            live_sets: LiveSets(BTreeMap::from_iter(
                cfg.blocks().map(|(id, _)| (id, LiveSet::default())),
            )),
        }
    }

    fn build(mut self) -> LiveSets {
        for reg in self.du_chains.defined_regs().collect::<Vec<_>>() {
            if !reg.is_virtual() {
                continue;
            }
            let uses: Vec<_> = self.du_chains[reg]
                .uses()
                .chain(self.du_chains[reg].phi_uses())
                .map(|loc| loc.block_id)
                .collect();
            for block in uses {
                if self.du_chains[reg].phi_uses_in(block).is_some() {
                    self.live_sets[block].live_outs.insert(reg);
                }
                self.up_and_mark(block, reg);
            }
        }
        self.live_sets
    }

    // According to the paper, there's a more efficient stack-based implementation of this. However,
    // since passes that use the liveness information probably want to use sets for live_ins and
    // live_outs, it isn't that much of a benifit to use stacks for the live_ins and live_outs while
    // constructing them.
    fn up_and_mark(&mut self, block: BlockId, reg: AnyReg) {
        let du_chain = &self.du_chains[reg];
        if du_chain.def_block() == block && !du_chain.is_phi_def() {
            return; // Killed in block, stop.
        }
        if !self.live_sets[block].live_ins.insert(reg) {
            return; // Propagation already done, stop.
        }
        if du_chain.def_block() == block && du_chain.is_phi_def() {
            return; // Do not propagate phi defs (i.e. block arguments).
        }
        for pred in self.cfg.predecessor_ids(block) {
            self.live_sets[pred].live_outs.insert(reg);
            self.up_and_mark(pred, reg)
        }
    }
}
