use std::collections::{BinaryHeap, HashMap, HashSet};

use crate::{cfg::Cfg, AnyReg};

use super::coloring::{Color, Coloring};

#[derive(Debug, Default, Clone)]
pub struct ConflictGraph(pub HashMap<AnyReg, HashSet<AnyReg>>);

impl ConflictGraph {
    pub fn new() -> Self {
        Self::default()
    }

    // Panics if there is a reg in `already_live` that has not been used in prevouis call
    // as `new_live`
    pub fn new_live(&mut self, new_live: AnyReg, already_live: impl Iterator<Item = AnyReg>) {
        self.0.entry(new_live).or_default();
        for already_live_reg in already_live {
            self.0.get_mut(&new_live).unwrap().insert(already_live_reg);
            self.0.get_mut(&already_live_reg).unwrap().insert(new_live);
        }
    }

    fn filtered(&self, keep: HashSet<AnyReg>) -> ConflictGraph {
        ConflictGraph(
            keep.iter()
                .filter_map(|reg| {
                    self.0.get(reg).map(|confs| {
                        (
                            *reg,
                            confs
                                .iter()
                                .filter(|conf| keep.contains(conf))
                                .cloned()
                                .collect(),
                        )
                    })
                })
                .collect(),
        )
    }

    fn get_stable_set(&self) -> HashSet<AnyReg> {
        self.0
            .iter()
            .filter(|(_, confl)| confl.is_empty())
            .map(|(reg, _)| *reg)
            .collect()
    }
}

#[derive(Debug)]
struct OptimizationUnit<'a> {
    old_coloring: &'a Coloring,
    full_conflict_graph: &'a ConflictGraph,
    conflict_graph: ConflictGraph,
    arg: AnyReg,
}

pub fn coales(cfg: &Cfg, coloring: &Coloring, conflict_graph: &ConflictGraph) -> Coloring {
    let mut coloring = coloring.clone();

    let mut blocks: Vec<_> = cfg.blocks().collect();
    // Sort callblocks first
    blocks.sort_unstable_by_key(|(_, b)| !b.is_call_block);

    for (block_id, block) in blocks {
        for (i, &arg) in block.arguments.iter().enumerate() {
            let params: Vec<_> = cfg
                .predecessors(block_id)
                .map(|(_, pred)| pred.successor(block_id).unwrap().arguments[i])
                .collect();

            let filterd_conflict_graph = conflict_graph
                .filtered(params.iter().cloned().chain(std::iter::once(arg)).collect());
            let ou = OptimizationUnit {
                old_coloring: &coloring,
                full_conflict_graph: conflict_graph,
                conflict_graph: filterd_conflict_graph,
                arg,
            };

            if let Some(c) = ou.optimize() {
                coloring = c;
            }
        }
    }
    coloring
}

#[derive(Debug)]
struct Entry {
    color: Color,
    conflict_graph: ConflictGraph,
    stable_set: HashSet<AnyReg>,
}

impl PartialEq for Entry {
    fn eq(&self, other: &Self) -> bool {
        self.stable_set.len().eq(&other.stable_set.len())
    }
}

impl Eq for Entry {}

impl PartialOrd for Entry {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Entry {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.stable_set.len().cmp(&other.stable_set.len())
    }
}

impl OptimizationUnit<'_> {
    fn optimize(&self) -> Option<Coloring> {
        let mut heap = BinaryHeap::new();

        let color_options: Vec<AnyReg> = match self.arg {
            AnyReg::R(_) => super::TEMP_CPU_REGS
                .iter()
                .chain(super::SAVED_CPU_REGS.iter())
                .map(|&reg| reg.into())
                .collect(),
            AnyReg::F(_) => super::TEMP_FPU_D_REGS
                .iter()
                .chain(super::SAVED_FPU_D_REGS.iter())
                .map(|&reg| reg.into())
                .collect(),
        };

        for color in color_options {
            let conflict_graph = self.conflict_graph.clone();
            let stable_set = conflict_graph.get_stable_set();
            let e = Entry {
                color: Color {
                    physical_reg: color,
                },
                conflict_graph,
                stable_set,
            };
            heap.push(e);
        }

        'new_entry: while let Some(e) = heap.pop() {
            let mut virtual_coloring = self.old_coloring.clone();
            let mut pinning_candidates = HashSet::new();

            for &u in &e.stable_set {
                if virtual_coloring.coloring.get(&u).unwrap() == &e.color {
                    // Already the new color
                    pinning_candidates.insert(u);
                    continue;
                }
                let mut inner_virtual_coloring = virtual_coloring.clone();

                let mut to_recolor = vec![(u, e.color)];
                loop {
                    let mut new_to_recolor = Vec::new();

                    for (reg, color) in to_recolor.into_iter() {
                        let saved_pinned_to_unsaved =
                            inner_virtual_coloring.saved_pinned.contains(&reg)
                                && !color.physical_reg.is_saved();
                        let already_pinned = inner_virtual_coloring.pinned.contains(&reg);
                        if saved_pinned_to_unsaved || already_pinned {
                            // Register already pinned
                            let mut new_e = e;
                            new_e.conflict_graph.0.get_mut(&u).unwrap().insert(u);
                            new_e.stable_set = new_e.conflict_graph.get_stable_set();
                            heap.push(new_e);
                            continue 'new_entry;
                        }
                        if pinning_candidates.contains(&reg) {
                            let mut new_e = e;
                            if reg == self.arg {
                                new_e.conflict_graph.0.get_mut(&u).unwrap().insert(u);
                            } else {
                                new_e.conflict_graph.0.get_mut(&u).unwrap().insert(reg);
                            }
                            new_e.stable_set = new_e.conflict_graph.get_stable_set();
                            heap.push(new_e);
                            continue 'new_entry;
                        }

                        let old_color_ref = inner_virtual_coloring.coloring.get_mut(&reg).unwrap();
                        let old_color = *old_color_ref;
                        *old_color_ref = color;

                        for conflict in self.full_conflict_graph.0.get(&reg).unwrap().iter().filter(
                            |pos_confl| {
                                inner_virtual_coloring.coloring.get(pos_confl) == Some(&color)
                            },
                        ) {
                            new_to_recolor.push((*conflict, old_color));
                        }
                    }
                    if new_to_recolor.is_empty() {
                        break;
                    }
                    to_recolor = new_to_recolor;
                }
                pinning_candidates.insert(u);
                virtual_coloring = inner_virtual_coloring;
            }

            if pinning_candidates.len() >= 2 {
                virtual_coloring.pinned.extend(pinning_candidates);
                return Some(virtual_coloring);
            }
        }

        None
    }
}
