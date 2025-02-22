//! Use-Def Analyzer

mod du_chains;
mod instruction;
mod terminator;

pub use du_chains::{
    DefLocation, DefUseError, DuChain, DuChains, GlobalLocation, Location,
};

use crate::AnyReg;

#[derive(Debug, Clone)]
pub struct Defs(Option<AnyReg>);

#[derive(Debug, Clone)]
pub struct Uses(<Vec<AnyReg> as IntoIterator>::IntoIter);
