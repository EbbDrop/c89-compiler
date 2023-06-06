use crate::{
    cfg::{BlockId, Cfg},
    AnyReg, FReg, Reg, VARGenerator,
};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefLocation {
    Argument(usize),
    Instruction(usize),
}

impl DefLocation {
    #[inline]
    pub fn as_location(self) -> Location {
        match self {
            Self::Argument(_) => Location(-1),
            Self::Instruction(n) => Location(n as isize),
        }
    }

    #[inline]
    pub fn as_next_location(self) -> Location {
        Location(self.as_location().0 + 1)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Location(pub isize);

impl std::ops::Sub for Location {
    type Output = isize;

    fn sub(self, rhs: Self) -> Self::Output {
        rhs.0 - self.0
    }
}

// impl std::ops::Sub<isize> for Location {
//     type Output = Location;

//     /// Will panic if `self.0` becomes less than `-1`.
//     fn sub(self, rhs: isize) -> Self::Output {
//         if self.0 - rhs < -1 {
//             panic!("cannot subtract beyond -1 location");
//         }
//         Self(self.0 - rhs)
//     }
// }

// impl std::ops::Add<isize> for Location {
//     type Output = Location;

//     fn add(self, rhs: isize) -> Self::Output {
//         Self(self.0 + rhs)
//     }
// }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalDefLocation {
    pub block: BlockId,
    pub local: DefLocation,
}

impl GlobalDefLocation {
    pub fn new(block: BlockId, local: DefLocation) -> Self {
        Self { block, local }
    }
}

impl GlobalDefLocation {
    #[inline]
    pub fn as_location(self) -> GlobalLocation {
        GlobalLocation::new(self.block, self.local.as_location())
    }

    #[inline]
    pub fn as_next_location(self) -> GlobalLocation {
        GlobalLocation::new(self.block, self.local.as_next_location())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct GlobalLocation {
    pub block_id: BlockId,
    pub local: Location,
}

impl GlobalLocation {
    pub fn new(block_id: BlockId, local: Location) -> Self {
        Self { block_id, local }
    }
}

#[derive(Debug, Clone)]
pub struct DuChain {
    def: GlobalDefLocation,
    /// All uses not including the propagation as arguments (see `phi_uses`).
    uses: BTreeMap<BlockId, Vec<Location>>,
    /// If it is passed as a block argument to another block. These uses are not included in
    /// `uses`.
    phi_uses: BTreeMap<BlockId, Location>,
}

impl DuChain {
    pub fn new(def: GlobalDefLocation) -> Self {
        Self {
            def,
            uses: BTreeMap::new(),
            phi_uses: BTreeMap::new(),
        }
    }

    pub fn add_use(&mut self, use_location: GlobalLocation) {
        self.uses
            .entry(use_location.block_id)
            .or_default()
            .push(use_location.local)
    }

    pub fn add_phi_use(&mut self, use_location: GlobalLocation) {
        self.phi_uses
            .insert(use_location.block_id, use_location.local);
    }

    pub fn extend_uses<I>(&mut self, use_locations: I)
    where
        I: IntoIterator<Item = GlobalLocation>,
    {
        use_locations
            .into_iter()
            .for_each(|location| self.add_use(location))
    }

    pub fn extend_phi_uses<I>(&mut self, use_locations: I)
    where
        I: IntoIterator<Item = GlobalLocation>,
    {
        use_locations
            .into_iter()
            .for_each(|location| self.add_phi_use(location))
    }

    pub fn def(&self) -> GlobalDefLocation {
        self.def
    }

    pub fn def_block(&self) -> BlockId {
        self.def.block
    }

    pub fn def_location(&self) -> DefLocation {
        self.def.local
    }

    pub fn is_phi_def(&self) -> bool {
        matches!(self.def.local, DefLocation::Argument(_))
    }

    pub fn uses(&self) -> impl Iterator<Item = GlobalLocation> + '_ {
        self.uses.iter().flat_map(|(&block_id, locations)| {
            locations
                .iter()
                .map(move |&local_location| GlobalLocation::new(block_id, local_location))
        })
    }

    pub fn uses_in(&self, block: BlockId) -> impl Iterator<Item = Location> + '_ {
        self.uses.get(&block).into_iter().flatten().copied()
    }

    pub fn last_use_in(&self, block: BlockId) -> Option<Location> {
        self.uses_in(block).max()
    }

    pub fn phi_uses(&self) -> impl Iterator<Item = GlobalLocation> + '_ {
        self.phi_uses
            .iter()
            .map(|(&block, &location)| GlobalLocation::new(block, location))
    }

    /// Returns the terminator's location if the reg of this DU-chain has a phi-use in the given
    /// block.
    pub fn phi_uses_in(&self, block: BlockId) -> Option<Location> {
        self.phi_uses.get(&block).copied()
    }
}

#[derive(Debug, Clone)]
pub struct DuChains(HashMap<AnyReg, DuChain>);

impl std::ops::Index<AnyReg> for DuChains {
    type Output = DuChain;

    fn index(&self, index: AnyReg) -> &Self::Output {
        &self.0[&index]
    }
}

impl std::ops::IndexMut<AnyReg> for DuChains {
    fn index_mut(&mut self, index: AnyReg) -> &mut Self::Output {
        self.0.get_mut(&index).unwrap()
    }
}

#[derive(Debug, Clone)]
pub enum DefUseError {
    UndefUses(HashMap<AnyReg, BTreeSet<GlobalLocation>>),
    MultipleDefs(HashMap<AnyReg, HashSet<GlobalDefLocation>>),
}

impl DuChains {
    /// Returns an error if the CFG contains uses that are not defined, or if it contains multiple
    /// defs of the same reg. Does *not* check for use-before def. Multiple arguments are considered
    /// multiple defs and as such result in an error. Non-virtual registers are completely ignored.
    /// Undef phi uses are considered undef uses.
    pub fn try_build_from(cfg: &Cfg) -> Result<Self, DefUseError> {
        // Allow multiple def locations to be set, so we can collect error info later.
        let mut defs: HashMap<AnyReg, HashSet<GlobalDefLocation>> = HashMap::new();
        let mut uses: HashMap<AnyReg, BTreeSet<GlobalLocation>> = HashMap::new();
        let mut phi_uses: HashMap<AnyReg, BTreeSet<GlobalLocation>> = HashMap::new();
        for (id, block) in cfg.blocks() {
            let mut add_def = |reg: AnyReg, location| {
                if reg.is_virtual() {
                    defs.entry(reg)
                        .or_default()
                        .insert(GlobalDefLocation::new(id, location));
                }
            };
            let mut add_use = |reg: AnyReg, location| {
                if reg.is_virtual() {
                    uses.entry(reg)
                        .or_default()
                        .insert(GlobalLocation::new(id, Location(location)));
                }
            };
            for (idx, &reg) in block.arguments.iter().enumerate() {
                add_def(reg, DefLocation::Argument(idx));
            }
            for (idx, instr) in block.instructions.iter().enumerate() {
                for reg in instr.uses() {
                    add_use(reg, idx as isize);
                }
                for reg in instr.defs() {
                    add_def(reg, DefLocation::Instruction(idx));
                }
            }
            for reg in block.terminator().uses() {
                add_use(reg, block.instructions.len() as isize);
            }
            for &reg in block.successors().flat_map(|bref| &bref.arguments) {
                if reg.is_virtual() {
                    phi_uses.entry(reg).or_default().insert(GlobalLocation::new(
                        id,
                        Location(block.instructions.len() as isize),
                    ));
                }
            }
        }
        let mut multiple_defs = HashMap::new();
        let mut du_chains = HashMap::new();
        for (reg, def_locations) in defs {
            if def_locations.len() > 1 {
                multiple_defs.insert(reg, def_locations);
            } else if let Some(def_location) = def_locations.into_iter().next() {
                du_chains.insert(reg, DuChain::new(def_location));
            }
        }
        if !multiple_defs.is_empty() {
            return Err(DefUseError::MultipleDefs(multiple_defs));
        }
        let mut undef_uses = HashMap::new();
        for (reg, use_locations) in uses {
            match du_chains.get_mut(&reg) {
                Some(du_chain) => {
                    du_chain.extend_uses(use_locations);
                }
                None => {
                    undef_uses.insert(reg, use_locations);
                }
            }
        }
        for (reg, use_locations) in phi_uses {
            match du_chains.get_mut(&reg) {
                Some(du_chain) => {
                    du_chain.extend_phi_uses(use_locations);
                }
                None => {
                    undef_uses.entry(reg).or_default().extend(use_locations);
                }
            }
        }
        if !undef_uses.is_empty() {
            return Err(DefUseError::UndefUses(undef_uses));
        }
        Ok(Self(du_chains))
    }

    pub fn defined_regs(&self) -> impl Iterator<Item = AnyReg> + '_ {
        self.0.keys().copied()
    }

    pub fn max_virtual_reg(&self) -> u32 {
        self.0
            .keys()
            .map(|reg| match *reg {
                AnyReg::R(Reg::Virtual(n)) => n,
                _ => 0,
            })
            .max()
            .unwrap_or(0)
    }

    pub fn max_virtual_single_freg(&self) -> u32 {
        self.0
            .keys()
            .map(|reg| match *reg {
                AnyReg::F(FReg::VirtualSingle(n)) => n,
                _ => 0,
            })
            .max()
            .unwrap_or(0)
    }

    pub fn max_virtual_double_freg(&self) -> u32 {
        self.0
            .keys()
            .map(|reg| match *reg {
                AnyReg::F(FReg::VirtualDouble(n)) => n,
                _ => 0,
            })
            .max()
            .unwrap_or(0)
    }

    pub fn var_generator(&self) -> VARGenerator {
        VARGenerator::new(
            self.max_virtual_reg() + 1,
            self.max_virtual_single_freg() + 1,
            self.max_virtual_double_freg() + 1,
        )
    }
}
