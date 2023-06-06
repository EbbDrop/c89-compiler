use super::{util::alias_reg_from, N_CPU_REGS, N_FPU_D_REGS, N_SAVED_CPU_REGS, N_SAVED_FPU_D_REGS};
use crate::cfg::BlockId;
use crate::function::StackAddress;
use crate::{
    cfg::Cfg,
    dfa::uda::{GlobalLocation, Location},
    AnyReg, FReg, Function, Instruction, Reg,
};
use std::collections::{BTreeMap, HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct SpillingGroup {
    def_reg: AnyReg,
    load_regs: HashSet<AnyReg>,
    stack_address: StackAddress,
}

pub type SpillingResult = Vec<SpillingGroup>;

pub fn spill_belady(function: &mut Function) {
    let mut spilling_result = SpillingResult::default();
    let mut belady_info = BTreeMap::new();

    for block_id in function.cfg.blocks().map(|(id, _)| id).collect::<Vec<_>>() {
        BeladyBlock::new(&mut belady_info, &mut spilling_result, function, block_id).run();
    }

    let original_blocks = function.cfg.blocks().map(|(id, _)| id).collect::<Vec<_>>();
    for block_id in original_blocks {
        for pred_id in function.cfg.predecessor_ids(block_id).collect::<Vec<_>>() {
            let mut to_reload = HashMap::new();
            let block_in_regs = &belady_info[&block_id].in_regs;
            let pred_out_regs = &belady_info[&pred_id].out_regs;
            for reg in block_in_regs.difference(pred_out_regs) {
                let idx = function.cfg[block_id]
                    .arguments
                    .iter()
                    .position(|arg| arg == reg);
                match idx {
                    Some(idx) => {
                        let pred_reg =
                            function.cfg[pred_id].successor(block_id).unwrap().arguments[idx];
                        if pred_out_regs.contains(&pred_reg) {
                            continue;
                        }
                        to_reload.insert(pred_reg, Some(idx));
                    }
                    None => {
                        to_reload.insert(*reg, None);
                    }
                }
            }
            if to_reload.is_empty() {
                continue;
            }
            let nop_instructions: Vec<_> = std::iter::repeat(Instruction::Nop)
                .take(to_reload.len())
                .collect();
            let new_block_id = function
                .cfg
                .insert_on_edge(pred_id, block_id, &nop_instructions);
            for (instr_idx, (reg, idx)) in (0..nop_instructions.len()).rev().zip(to_reload) {
                let alias = match idx {
                    Some(idx) => {
                        // If we have an index, `reg` is actually a the reg in pred that was passed
                        // to block.
                        let alias = function
                            .cfg
                            .var_generator()
                            .next_of_type(function.cfg[new_block_id].arguments[idx]);
                        function.cfg[new_block_id]
                            .successor_mut(block_id)
                            .unwrap()
                            .arguments[idx] = alias;
                        add_spill_aliases(
                            function,
                            &mut spilling_result,
                            reg,
                            std::iter::once(alias),
                        );
                        alias
                    }
                    None => {
                        let (alias, aliases) = alias_reg_from(
                            &mut function.cfg,
                            reg,
                            GlobalLocation::new(new_block_id, Location(instr_idx as isize)),
                        );
                        for (&block_id, &other_alias) in aliases.iter() {
                            let Some(info) = belady_info.get_mut(&block_id) else { continue };
                            if info.in_regs.remove(&reg) {
                                info.in_regs.insert(other_alias);
                            }
                            if info.out_regs.remove(&reg) {
                                info.out_regs.insert(other_alias);
                            }
                        }
                        add_spill_aliases(
                            function,
                            &mut spilling_result,
                            reg,
                            std::iter::once(alias).chain(aliases.values().copied()),
                        );
                        alias
                    }
                };
                let stack_address = spilling_result
                    .iter()
                    .find(|g| g.def_reg == reg || g.load_regs.contains(&reg))
                    .unwrap()
                    .stack_address;
                function.cfg[new_block_id].instructions[instr_idx] =
                    crate::instr::virt::load_from_stack(alias, stack_address);
            }
        }
    }

    for group in &spilling_result {
        let du_chain = &function.cfg.du_chains()[group.def_reg];
        let block_id = du_chain.def_block();
        match du_chain.def_location() {
            crate::dfa::uda::DefLocation::Instruction(location) => {
                function.cfg[block_id].instructions.insert(
                    location + 1,
                    crate::instr::virt::store_to_stack(group.def_reg, group.stack_address),
                );
            }
            crate::dfa::uda::DefLocation::Argument(idx) => {
                if belady_info[&block_id].in_regs.contains(&group.def_reg) {
                    function.cfg[block_id].instructions.insert(
                        0,
                        crate::instr::virt::store_to_stack(group.def_reg, group.stack_address),
                    );
                } else {
                    let mut visited = HashSet::<(BlockId, AnyReg)>::new();
                    let mut to_visit: HashSet<(BlockId, AnyReg)> = function
                        .cfg
                        .predecessors(block_id)
                        .map(|(pred_id, pred)| {
                            (pred_id, pred.successor(block_id).unwrap().arguments[idx])
                        })
                        .collect();
                    while let Some(&(id, alias)) = to_visit.iter().next() {
                        to_visit.remove(&(id, alias));
                        visited.insert((id, alias));
                        let du_chain = &function.cfg.du_chains()[alias];
                        if du_chain.def_block() == id {
                            match du_chain.def_location() {
                                crate::dfa::uda::DefLocation::Argument(idx) => {
                                    match belady_info.get(&id) {
                                        Some(info) if info.in_regs.contains(&alias) => {
                                            function.cfg[id].instructions.insert(
                                                0,
                                                crate::instr::virt::store_to_stack(
                                                    alias,
                                                    group.stack_address,
                                                ),
                                            );
                                        }
                                        _ => {
                                            for pair in function.cfg.predecessors(id).map(
                                                |(pred_id, pred)| {
                                                    (
                                                        pred_id,
                                                        pred.successor(id).unwrap().arguments[idx],
                                                    )
                                                },
                                            ) {
                                                if !visited.contains(&pair) {
                                                    to_visit.insert(pair);
                                                }
                                            }
                                        }
                                    }
                                }
                                crate::dfa::uda::DefLocation::Instruction(i) => {
                                    function.cfg[id].instructions.insert(
                                        i + 1,
                                        crate::instr::virt::store_to_stack(
                                            alias,
                                            group.stack_address,
                                        ),
                                    );
                                }
                            }
                        } else {
                            match belady_info.get(&id) {
                                Some(info) if info.in_regs.contains(&alias) => {
                                    function.cfg[id].instructions.insert(
                                        0,
                                        crate::instr::virt::store_to_stack(
                                            alias,
                                            group.stack_address,
                                        ),
                                    );
                                }
                                Some(_) => {
                                    for pair in function
                                        .cfg
                                        .predecessor_ids(id)
                                        .map(|pred_id| (pred_id, alias))
                                    {
                                        if !visited.contains(&pair) {
                                            to_visit.insert(pair);
                                        }
                                    }
                                }
                                None => {
                                    match function.cfg[id]
                                        .arguments
                                        .iter()
                                        .position(|&arg| arg == alias)
                                    {
                                        Some(idx) => {
                                            for pair in function.cfg.predecessors(id).map(
                                                |(pred_id, pred)| {
                                                    (
                                                        pred_id,
                                                        pred.successor(id).unwrap().arguments[idx],
                                                    )
                                                },
                                            ) {
                                                if !visited.contains(&pair) {
                                                    to_visit.insert(pair);
                                                }
                                            }
                                        }
                                        None => {
                                            for pair in function
                                                .cfg
                                                .predecessor_ids(id)
                                                .map(|pred_id| (pred_id, alias))
                                            {
                                                if !visited.contains(&pair) {
                                                    to_visit.insert(pair);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Everything that is live-in, but not in `in_regs` is expected to be stored before this block.
/// Everything that is live-out, but not in `out_regs` will have been stored in this block.
#[derive(Debug)]
struct BeladyBlockProps {
    in_regs: HashSet<AnyReg>,
    out_regs: HashSet<AnyReg>,
}

pub struct BeladyBlock<'a, 'b, 'c> {
    block_id: BlockId,
    belady_info: &'c mut BTreeMap<BlockId, BeladyBlockProps>,
    spilling_result: &'b mut SpillingResult,
    function: &'a mut Function,
    in_regs: HashSet<AnyReg>,
    current_cpu_regs: HashSet<Reg>,
    current_fpu_regs: HashSet<FReg>,
    used_regs: HashSet<AnyReg>,
    location: usize,
}

impl<'a, 'b, 'c> BeladyBlock<'a, 'b, 'c> {
    fn new(
        belady_info: &'c mut BTreeMap<BlockId, BeladyBlockProps>,
        spilling_result: &'b mut SpillingResult,
        function: &'a mut Function,
        block_id: BlockId,
    ) -> Self {
        Self {
            function,
            block_id,
            belady_info,
            spilling_result,
            in_regs: HashSet::new(),
            current_cpu_regs: HashSet::new(),
            current_fpu_regs: HashSet::new(),
            used_regs: HashSet::new(),
            location: 0,
        }
    }

    fn run(mut self) {
        let is_call_block = self.function.cfg[self.block_id].is_call_block;

        let live_ins = &self.function.cfg.live_sets()[self.block_id].live_ins;
        let live_outs = &self.function.cfg.live_sets()[self.block_id].live_outs;

        // Set `self.in_regs` and `self.current_regs` to the K members of `live_ins` with the
        // nearest nextuse.
        {
            let mut candidates = live_ins.clone();
            let mut n_in_saved_cpu_regs = 0; // saved
            let mut n_in_saved_fpu_d_regs = 0; // saved
            while let Some(&candidate) = candidates.iter().min_by_key(|&&reg| {
                self.distance_to_next_use_after(self.block_id, -1, reg)
                    .unwrap_or(std::usize::MAX)
            }) {
                candidates.remove(&candidate);
                match candidate {
                    AnyReg::R(reg) if self.current_cpu_regs.len() < N_CPU_REGS => {
                        if is_call_block && live_outs.contains(&candidate) {
                            if n_in_saved_cpu_regs < N_SAVED_CPU_REGS {
                                self.current_cpu_regs.insert(reg);
                                n_in_saved_cpu_regs += 1;
                            }
                        } else {
                            self.current_cpu_regs.insert(reg);
                        }
                    }
                    AnyReg::F(freg) if self.current_fpu_regs.len() < N_FPU_D_REGS => {
                        if is_call_block && live_outs.contains(&candidate) {
                            if n_in_saved_fpu_d_regs < N_SAVED_FPU_D_REGS {
                                self.current_fpu_regs.insert(freg);
                                n_in_saved_fpu_d_regs += 1;
                            }
                        } else {
                            self.current_fpu_regs.insert(freg);
                        }
                    }
                    _ => (),
                }
                if self.current_cpu_regs.len() == N_CPU_REGS
                    && self.current_fpu_regs.len() == N_FPU_D_REGS
                {
                    break;
                }
            }
        }
        self.in_regs = HashSet::from_iter(
            self.current_cpu_regs
                .iter()
                .map(|&r| r.into())
                .chain(self.current_fpu_regs.iter().map(|&r| r.into())),
        );

        while self.location < self.function.cfg[self.block_id].instructions.len() {
            let instr = &self.function.cfg[self.block_id].instructions[self.location];
            let uses: Vec<AnyReg> = instr.uses().filter(|r| r.is_virtual()).collect();
            let defs = instr.defs().filter(|r| r.is_virtual());
            let (displaced, added) = self.displace(uses.iter().copied());
            for reg in displaced {
                if !self.used_regs.contains(&reg) {
                    self.in_regs.remove(&reg);
                }
            }
            for reg in added {
                self.insert_load(reg);
            }
            self.used_regs.extend(uses);
            // Location must be incremented before calling self.displace(defs)!
            self.location += 1;
            self.displace(defs);
        }

        if is_call_block {
            // After the call is added, only saved registers can be preserved.
            while self.current_cpu_regs.len() > N_SAVED_CPU_REGS {
                let to_remove = *self
                    .current_cpu_regs
                    .iter()
                    .max_by_key(|&&reg| {
                        self.distance_to_next_use_after(
                            self.block_id,
                            self.location as isize - 1,
                            reg.into(),
                        )
                        .unwrap_or(std::usize::MAX)
                    })
                    .unwrap();
                self.current_cpu_regs.remove(&to_remove);
            }
            while self.current_fpu_regs.len() > N_SAVED_FPU_D_REGS {
                let to_remove = *self
                    .current_fpu_regs
                    .iter()
                    .max_by_key(|&&reg| {
                        self.distance_to_next_use_after(
                            self.block_id,
                            self.location as isize - 1,
                            reg.into(),
                        )
                        .unwrap_or(std::usize::MAX)
                    })
                    .unwrap();
                self.current_fpu_regs.remove(&to_remove);
            }
        }

        {
            let block = &self.function.cfg[self.block_id];
            let term_uses = block.terminator().uses().filter(|r| r.is_virtual());
            let term_uses: Vec<_> = term_uses.collect();
            let (displaced, added) = self.displace(term_uses.iter().copied());
            for reg in displaced {
                if !self.used_regs.contains(&reg) {
                    self.in_regs.remove(&reg);
                }
            }
            for reg in added {
                self.insert_load(reg);
            }
            self.used_regs.extend(term_uses);
        }

        let belady_block_props = BeladyBlockProps {
            in_regs: self.in_regs,
            out_regs: HashSet::from_iter(
                self.current_cpu_regs
                    .iter()
                    .map(|&r| r.into())
                    .chain(self.current_fpu_regs.iter().map(|&r| r.into())),
            ),
        };
        self.belady_info.insert(self.block_id, belady_block_props);
    }

    /// Returns a (displaced, added) pair.
    fn displace<I>(&mut self, needed_regs: I) -> (HashSet<AnyReg>, HashSet<AnyReg>)
    where
        I: Iterator<Item = AnyReg> + Clone,
    {
        let mut n_missing_cpu_regs = 0;
        let mut n_missing_fpu_regs = 0;
        for reg in needed_regs.clone() {
            match reg {
                AnyReg::R(r) if !self.current_cpu_regs.contains(&r) => n_missing_cpu_regs += 1,
                AnyReg::F(r) if !self.current_fpu_regs.contains(&r) => n_missing_fpu_regs += 1,
                _ => {}
            }
        }

        let n_cpu_regs_to_displace =
            usize::saturating_sub(n_missing_cpu_regs + self.current_cpu_regs.len(), N_CPU_REGS);
        let n_fpu_regs_to_displace = usize::saturating_sub(
            n_missing_fpu_regs + self.current_fpu_regs.len(),
            N_FPU_D_REGS,
        );

        let mut displaced = HashSet::new();

        let location = self.location as isize - 1;

        for _ in 0..n_cpu_regs_to_displace {
            let regs = self.current_cpu_regs.iter().copied();
            let reg_to_displace = regs
                .max_by_key(|&r| {
                    self.distance_to_next_use_after(self.block_id, location, r.into())
                        .unwrap_or(std::usize::MAX)
                })
                .unwrap();
            displaced.insert(reg_to_displace.into());
            self.current_cpu_regs.remove(&reg_to_displace);
        }

        for _ in 0..n_fpu_regs_to_displace {
            let regs = self.current_fpu_regs.iter().copied();
            let reg_to_displace = regs
                .max_by_key(|&r| {
                    self.distance_to_next_use_after(self.block_id, location, r.into())
                        .unwrap_or(std::usize::MAX)
                })
                .unwrap();
            displaced.insert(reg_to_displace.into());
            self.current_fpu_regs.remove(&reg_to_displace);
        }

        let mut added = HashSet::new();
        for reg in needed_regs {
            let inserted = match reg {
                AnyReg::R(r) => self.current_cpu_regs.insert(r),
                AnyReg::F(r) => self.current_fpu_regs.insert(r),
            };
            if inserted {
                added.insert(reg);
            }
        }

        (displaced, added)
    }

    fn insert_load(&mut self, reg: AnyReg) {
        let (alias, aliases) = alias_reg_from(
            &mut self.function.cfg,
            reg,
            GlobalLocation::new(self.block_id, Location(self.location as isize)),
        );
        if let Some(&start_alias) = aliases.get(&self.block_id) {
            if self.in_regs.remove(&reg) {
                self.in_regs.insert(start_alias);
            }
            if self.used_regs.remove(&reg) {
                self.used_regs.insert(start_alias);
            }
        }
        match reg {
            AnyReg::R(reg) => {
                if self.current_cpu_regs.remove(&reg) {
                    self.current_cpu_regs.insert(alias.try_into().unwrap());
                }
            }
            AnyReg::F(freg) => {
                if self.current_fpu_regs.remove(&freg) {
                    self.current_fpu_regs.insert(alias.try_into().unwrap());
                }
            }
        }
        for (&block_id, &other_alias) in &aliases {
            let Some(info) = self.belady_info.get_mut(&block_id) else { continue };
            if info.in_regs.remove(&reg) {
                info.in_regs.insert(other_alias);
            }
            if info.out_regs.remove(&reg) {
                info.out_regs.insert(other_alias);
            }
        }
        add_spill_aliases(
            self.function,
            self.spilling_result,
            reg,
            std::iter::once(alias).chain(aliases.values().copied()),
        );
        let stack_address = self
            .spilling_result
            .iter()
            .find(|g| g.def_reg == reg || g.load_regs.contains(&reg))
            .unwrap()
            .stack_address;
        let reload_instr = crate::instr::virt::load_from_stack(alias, stack_address);
        let block = &mut self.function.cfg[self.block_id];
        block.instructions.insert(self.location, reload_instr);
        self.location += 1;
    }

    /// Returns `None` if there's no next use. Use -1 to get distance to next use from block start.
    fn distance_to_next_use_after(
        &self,
        block_id: BlockId,
        location: isize,
        reg: AnyReg,
    ) -> Option<usize> {
        fn rec(
            cfg: &Cfg,
            block_id: BlockId,
            location: isize,
            reg: AnyReg,
            visited_stack: &mut Vec<(BlockId, AnyReg)>,
        ) -> Option<usize> {
            if visited_stack.contains(&(block_id, reg)) {
                return None;
            }
            let du_chain = &cfg.du_chains()[reg];
            if du_chain.def_block() == block_id
                && location <= du_chain.def_location().as_location().0
            {
                // If the CFG is proper SSA, there can be no uses before (or at) the def.
                return None;
            }
            if !cfg.live_sets()[block_id].live_outs.contains(&reg) {
                // Last use is in this block.
                return du_chain
                    .uses_in(block_id)
                    .filter(|&loc| location < loc.0)
                    .min()
                    .map(|loc| (loc.0 - location + 1) as usize);
            }
            // `reg` is live-out and `location` comes after (or on) the def location (or it is not
            // defined in this block, but is live-in (since it's live-out)).
            let block = &cfg[block_id];
            visited_stack.push((block_id, reg));
            let distance_to_nearest_next_use = block
                .successors()
                .filter_map(|succ_bref| {
                    rec(cfg, succ_bref.id, -1, reg, visited_stack)
                        .into_iter()
                        .chain(
                            succ_bref
                                .arguments
                                .iter()
                                .zip(&cfg[succ_bref.id].arguments)
                                .filter(|(&arg, _)| arg == reg)
                                .flat_map(|(_, &param)| {
                                    rec(cfg, block_id, -1, param, visited_stack)
                                }),
                        )
                        .min()
                })
                .min()
                .map(|d| block.instructions.len() - (location + 1) as usize + 1 + d);
            visited_stack.pop();
            distance_to_nearest_next_use
        }
        let mut visited_stack = Vec::new();
        rec(
            &self.function.cfg,
            block_id,
            location,
            reg,
            &mut visited_stack,
        )
    }
}

fn add_spill_aliases(
    function: &mut Function,
    spilling_result: &mut SpillingResult,
    reg: AnyReg,
    aliases: impl Iterator<Item = AnyReg>,
) {
    for group in spilling_result.iter_mut() {
        if group.def_reg == reg || group.load_regs.contains(&reg) {
            for alias in aliases {
                if group.load_regs.insert(alias) {
                    function
                        .reg_to_stack_address
                        .insert(alias, group.stack_address);
                }
            }
            return;
        }
    }
    let stack_address = function.create_stack_address(reg.stack_info());
    function.reg_to_stack_address.insert(reg, stack_address);
    spilling_result.push(SpillingGroup {
        def_reg: reg,
        load_regs: aliases.collect(),
        stack_address,
    });
}
