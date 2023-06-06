use super::{SAVED_CPU_REGS, SAVED_FPU_D_REGS, TEMP_CPU_REGS, TEMP_FPU_D_REGS};
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

#[derive(Debug)]
pub struct Coloring {
    pub coloring: HashMap<AnyReg, Color>,
}

pub fn color(cfg: &Cfg) -> Coloring {
    Colorer::new(cfg).color()
}

struct Colorer<'a> {
    cfg: &'a Cfg,
    du_chains: uda::DuChains,
    live_sets: liveness::LiveSets,
    coloring: Coloring,
    assigned_colors: HashSet<Color>,
    suggestions: HashMap<AnyReg, Color>,
}

impl<'a> Colorer<'a> {
    fn new(cfg: &'a Cfg) -> Self {
        Self {
            cfg,
            du_chains: cfg.du_chains(),
            live_sets: cfg.live_sets(),
            assigned_colors: HashSet::new(),
            coloring: Coloring {
                coloring: HashMap::new(),
            },
            suggestions: HashMap::new(),
        }
    }

    fn color(mut self) -> Coloring {
        for block_id in self.cfg.dominator_tree().dfs_preorder() {
            let is_call_block = self.cfg[block_id].is_call_block;
            self.assigned_colors = self.live_sets[block_id]
                .live_ins
                .iter()
                .filter_map(|reg| self.coloring.coloring.get(reg).copied())
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
        self.coloring
    }

    fn release_color_of(&mut self, reg: AnyReg) {
        self.assigned_colors.remove(&self.coloring.coloring[&reg]);
    }

    /// Also returns the assigned color.
    fn assign_color_to(&mut self, reg: AnyReg, must_be_saved: bool) -> Color {
        let suggestion = self.suggestions.get(&reg);
        let color = match reg {
            AnyReg::R(_) => self.get_unassigned_cpu_color(must_be_saved, suggestion),
            AnyReg::F(_) => self.get_unassigned_fpu_color(must_be_saved, suggestion),
        };
        self.assigned_colors.insert(color);
        self.coloring.coloring.insert(reg, color);
        color
    }

    fn get_unassigned_cpu_color(&self, must_be_saved: bool, suggestion: Option<&Color>) -> Color {
        match must_be_saved {
            true => self.get_unassigned_color_from(
                SAVED_CPU_REGS.iter().map(|&r| Color::new(r.into())),
                suggestion,
            ),
            false => self.get_unassigned_color_from(
                TEMP_CPU_REGS
                    .iter()
                    .chain(&SAVED_CPU_REGS)
                    .map(|&r| Color::new(r.into())),
                suggestion,
            ),
        }
    }

    fn get_unassigned_fpu_color(&self, must_be_saved: bool, suggestion: Option<&Color>) -> Color {
        match must_be_saved {
            true => self.get_unassigned_color_from(
                SAVED_FPU_D_REGS.iter().map(|&r| Color::new(r.into())),
                suggestion,
            ),
            false => self.get_unassigned_color_from(
                TEMP_FPU_D_REGS
                    .iter()
                    .chain(&SAVED_FPU_D_REGS)
                    .map(|&r| Color::new(r.into())),
                suggestion,
            ),
        }
    }

    fn get_unassigned_color_from(
        &self,
        candidates: impl Iterator<Item = Color> + Clone,
        suggestion: Option<&Color>,
    ) -> Color {
        let candidates: Vec<_> = candidates
            .filter(|color| !self.assigned_colors.contains(color))
            .collect();

        // If a other block has sugested this color and its available, use it.
        if let Some(suggestion) = suggestion {
            if candidates.contains(suggestion) {
                return *suggestion;
            }
        }

        // Try to not use a color that is already suggested for another reg
        if let Some(s) = candidates
            .iter()
            .find(|color| !self.suggestions.values().any(|s_color| &s_color == color))
        {
            return *s;
        }

        *candidates.first().expect("not enough colors")
    }
}
