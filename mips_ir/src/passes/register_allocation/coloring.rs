use super::{
    coalescing::ConflictGraph, SAVED_CPU_REGS, SAVED_FPU_D_REGS, TEMP_CPU_REGS, TEMP_FPU_D_REGS,
};
use crate::{
    cfg::Cfg,
    dfa::liveness,
    dfa::uda::{self, Location},
    AnyReg,
};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Color {
    pub physical_reg: AnyReg,
}

impl Color {
    pub fn new(physical_reg: AnyReg) -> Self {
        Self { physical_reg }
    }
}

#[derive(Debug, Clone)]
pub struct Coloring {
    pub coloring: HashMap<AnyReg, Color>,
    pub pinned: HashSet<AnyReg>,
    // These registers need to be in a saved reg
    pub saved_pinned: HashSet<AnyReg>,
}

pub fn color(cfg: &Cfg) -> (Coloring, ConflictGraph) {
    Colorer::new(cfg).color()
}

struct Colorer<'a> {
    cfg: &'a Cfg,
    du_chains: uda::DuChains,
    live_sets: liveness::LiveSets,
    coloring: Coloring,
    assigned_colors: HashMap<Color, AnyReg>,
    suggestions: HashMap<AnyReg, Color>,
    conflict_graph: ConflictGraph,
}

impl<'a> Colorer<'a> {
    fn new(cfg: &'a Cfg) -> Self {
        Self {
            cfg,
            du_chains: cfg.du_chains(),
            live_sets: cfg.live_sets(),
            assigned_colors: HashMap::new(),
            coloring: Coloring {
                coloring: HashMap::new(),
                pinned: HashSet::new(),
                saved_pinned: HashSet::new(),
            },
            suggestions: HashMap::new(),
            conflict_graph: ConflictGraph::new(),
        }
    }

    fn color(mut self) -> (Coloring, ConflictGraph) {
        for block_id in self.cfg.dominator_tree().dfs_preorder() {
            let is_call_block = self.cfg[block_id].is_call_block;
            self.assigned_colors = self
                .coloring
                .coloring
                .iter()
                .filter(|(reg, _)| self.live_sets[block_id].live_ins.contains(reg))
                .map(|(reg, color)| (*color, *reg))
                .collect();
            for reg in &self.live_sets[block_id].live_ins.clone() {
                if reg.is_virtual() && !self.coloring.coloring.contains_key(reg) {
                    self.assign_color_to(
                        *reg,
                        is_call_block && self.live_sets[block_id].live_outs.contains(reg),
                    );
                }
            }
            let block = &self.cfg[block_id];
            for (index, instr) in block.instructions.iter().enumerate() {
                for reg in instr.uses().filter(|r| r.is_virtual()) {
                    if !self.live_sets[block_id].live_outs.contains(&reg)
                        && self.du_chains[reg].last_use_in(block_id)
                            == Some(Location(index as isize))
                    {
                        self.release_color_of(reg);
                    }
                }
                for reg in instr.defs().filter(|r| r.is_virtual()) {
                    self.assign_color_to(reg, false);
                }
            }

            for reg in block.terminator().uses().filter(|r| r.is_virtual()) {
                if !self.live_sets[block_id].live_outs.contains(&reg)
                    && self.du_chains[reg].last_use_in(block_id)
                        == Some(Location(block.instructions.len() as isize))
                {
                    self.release_color_of(reg);
                }
            }

            for succ in block.successors() {
                for (i, reg) in succ.arguments.iter().enumerate() {
                    if let Some(color) = self.coloring.coloring.get(reg) {
                        self.suggestions
                            .insert(self.cfg[succ.id].arguments[i], *color);
                    }
                }
            }
        }
        (self.coloring, self.conflict_graph)
    }

    fn release_color_of(&mut self, reg: AnyReg) {
        self.assigned_colors.remove(&self.coloring.coloring[&reg]);
    }

    /// Also returns the assigned color.
    fn assign_color_to(&mut self, reg: AnyReg, must_be_saved: bool) -> Color {
        let color = match reg {
            AnyReg::R(_) => self.get_unassigned_cpu_color(must_be_saved),
            AnyReg::F(_) => self.get_unassigned_fpu_color(must_be_saved),
        };

        self.conflict_graph
            .new_live(reg, self.assigned_colors.values().cloned());
        self.assigned_colors.insert(color, reg);
        self.coloring.coloring.insert(reg, color);
        if must_be_saved {
            self.coloring.saved_pinned.insert(reg);
        }

        color
    }

    fn get_unassigned_cpu_color(&self, must_be_saved: bool) -> Color {
        match must_be_saved {
            true => {
                self.get_unassigned_color_from(SAVED_CPU_REGS.iter().map(|&r| Color::new(r.into())))
            }
            false => self.get_unassigned_color_from(
                TEMP_CPU_REGS
                    .iter()
                    .chain(&SAVED_CPU_REGS)
                    .map(|&r| Color::new(r.into())),
            ),
        }
    }

    fn get_unassigned_fpu_color(&self, must_be_saved: bool) -> Color {
        match must_be_saved {
            true => self
                .get_unassigned_color_from(SAVED_FPU_D_REGS.iter().map(|&r| Color::new(r.into()))),
            false => self.get_unassigned_color_from(
                TEMP_FPU_D_REGS
                    .iter()
                    .chain(&SAVED_FPU_D_REGS)
                    .map(|&r| Color::new(r.into())),
            ),
        }
    }

    fn get_unassigned_color_from(
        &self,
        mut candidates: impl Iterator<Item = Color> + Clone,
    ) -> Color {
        candidates
            .find(|color| !self.assigned_colors.contains_key(color))
            .expect("not enough colors")
    }
}
