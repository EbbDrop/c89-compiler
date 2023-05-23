//! In this module, the following abbreviations are heavily used:
//!
//! | abbreviation              | meaning                                |
//! | ------------------------- | -------------------------------------- |
//! | `succ`, `succs`           | successor (direct successor)           |
//! | `pred`, `preds`           | predecessor (direct predecessor)       |
//! | `dsucc`, `dsuccs`         | default successor                      |
//! | `dpred`, `dpreds`         | default predecessor                    |
//! | `ndsucc`, `ndsuccs`       | non-default successor                  |
//! | `ndpred`, `ndpreds`       | non-default predecessor                |
//! | `bref`, `brefs`           | [`BlockRef`]                           |
//! | `succidx`, `succidxs`     | [`SuccIdx`] of a successor             |
//! | `dsuccidx`, `dsuccidxs`   | [`SuccIdx`] of a default successor     |
//! | `ndsuccidx`, `ndsuccidxs` | [`SuccIdx`] of a non-default successor |
//! | BB, block                 | [`Basic Block`]                        |
//!

use crate::{FunctionCall, VirtualTerminator};

use super::*;

impl Function {
    /// Returns `true` if the specified global label is referenced anywhere in this function.
    pub fn references_label(&self, label: &Label) -> bool {
        self.blocks().any(|b| b.references_label(label))
    }

    /// Returns all the global labels that are referenced anywhere in this function.
    pub fn referenced_labels(&self) -> impl Iterator<Item = &Label> {
        self.blocks().flat_map(|b| b.referenced_labels())
    }

    pub fn calls_function(&self, label: &Label) -> bool {
        self.blocks().any(|b| b.calls_function(label))
    }

    pub fn function_calls(&self) -> impl Iterator<Item = &FunctionCall> {
        self.blocks().flat_map(|b| b.function_calls())
    }
}

/// Index to identify the successors of a [`Basic Block`] within that block. Note that no guarantees
/// are made about what such index looks like, nor the order of the indices.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SuccIdx(usize);

impl BasicBlock {
    pub fn succ_id(&self, succ_idx: SuccIdx) -> BlockId {
        self.succ_bref(succ_idx).label.id
    }

    pub fn succ_bref(&self, succ_idx: SuccIdx) -> &BlockRef {
        match &self.terminator {
            Terminator::BranchIf(_, _, _, true_target, false_target)
            | Terminator::BranchIfZ(_, _, true_target, false_target)
            | Terminator::BranchIfFCond(_, true_target, false_target) => match succ_idx.0 {
                0 => false_target,
                1 => true_target,
                _ => panic!("invalid successor index"),
            },
            Terminator::BranchIfZAndLink(_, _, _, false_target) => {
                assert!(succ_idx.0 == 0);
                false_target
            }
            Terminator::Jump(target) => {
                assert!(succ_idx.0 == 1);
                target
            }
            Terminator::ReturnToRa => panic!("invalid successor index"),
            Terminator::JumpAndLink(_, next_block)
            | Terminator::JumpAndLinkRa(_, next_block)
            | Terminator::JumpAndLinkReg(_, _, next_block) => {
                assert!(succ_idx.0 == 0);
                next_block
            }
            Terminator::Virtual(VirtualTerminator::Return(_)) => panic!("invalid successor index"),
        }
    }

    pub fn succ_bref_mut(&mut self, succ_idx: SuccIdx) -> &mut BlockRef {
        match &mut self.terminator {
            Terminator::BranchIf(_, _, _, true_target, false_target)
            | Terminator::BranchIfZ(_, _, true_target, false_target)
            | Terminator::BranchIfFCond(_, true_target, false_target) => match succ_idx.0 {
                0 => false_target,
                1 => true_target,
                _ => panic!("invalid successor index"),
            },
            Terminator::BranchIfZAndLink(_, _, _, false_target) => {
                assert!(succ_idx.0 == 0);
                false_target
            }
            Terminator::Jump(target) => {
                assert!(succ_idx.0 == 1);
                target
            }
            Terminator::ReturnToRa => panic!("invalid successor index"),
            Terminator::JumpAndLink(_, next_block)
            | Terminator::JumpAndLinkRa(_, next_block)
            | Terminator::JumpAndLinkReg(_, _, next_block) => {
                assert!(succ_idx.0 == 0);
                next_block
            }
            Terminator::Virtual(VirtualTerminator::Return(_)) => panic!("invalid successor index"),
        }
    }

    /// Returns `true` if this block has any direct successors.
    pub fn has_succs(&self) -> bool {
        self.succidxs().next().is_some()
    }

    pub fn succidxs(&self) -> impl Iterator<Item = SuccIdx> {
        // Make the index of the default successor always 0, number the others from left to right.
        match &self.terminator {
            Terminator::BranchIf(_, _, _, _, _)
            | Terminator::BranchIfZ(_, _, _, _)
            | Terminator::BranchIfFCond(_, _, _) => vec![SuccIdx(0), SuccIdx(1)].into_iter(),
            Terminator::BranchIfZAndLink(_, _, _, _) => vec![SuccIdx(0)].into_iter(),
            Terminator::Jump(_) => vec![SuccIdx(1)].into_iter(),
            Terminator::JumpAndLink(_, _)
            | Terminator::JumpAndLinkRa(_, _)
            | Terminator::JumpAndLinkReg(_, _, _) => vec![SuccIdx(0)].into_iter(),
            Terminator::ReturnToRa | Terminator::Virtual(VirtualTerminator::Return(_)) => {
                vec![].into_iter()
            }
        }
    }

    /// Returns `true` if this block has a default successor.
    pub fn has_dsucc(&self) -> bool {
        self.dsuccidx().is_some()
    }

    pub fn dsuccidx(&self) -> Option<SuccIdx> {
        match &self.terminator {
            Terminator::BranchIf(_, _, _, _, _)
            | Terminator::BranchIfZ(_, _, _, _)
            | Terminator::BranchIfZAndLink(_, _, _, _)
            | Terminator::BranchIfFCond(_, _, _)
            | Terminator::JumpAndLink(_, _)
            | Terminator::JumpAndLinkRa(_, _)
            | Terminator::JumpAndLinkReg(_, _, _) => Some(SuccIdx(0)),
            Terminator::Jump(_)
            | Terminator::ReturnToRa
            | Terminator::Virtual(VirtualTerminator::Return(_)) => None,
        }
    }

    pub fn has_ndsucc(&self) -> bool {
        self.ndsuccidxs().next().is_some()
    }

    pub fn ndsuccidxs(&self) -> impl Iterator<Item = SuccIdx> {
        match &self.terminator {
            Terminator::BranchIf(_, _, _, _, _)
            | Terminator::BranchIfZ(_, _, _, _)
            | Terminator::BranchIfFCond(_, _, _)
            | Terminator::Jump(_) => Some(SuccIdx(1)),
            Terminator::BranchIfZAndLink(_, _, _, _)
            | Terminator::JumpAndLink(_, _)
            | Terminator::ReturnToRa
            | Terminator::JumpAndLinkRa(_, _)
            | Terminator::JumpAndLinkReg(_, _, _)
            | Terminator::Virtual(VirtualTerminator::Return(_)) => None,
        }
        .into_iter()
    }

    /// Tries to swap the given successors of this block. Returns `Ok(_)` if the swap succeeded, or
    /// `Err(_)` if the swap is not possible. Panics if the `succ1` or `succ2` are not valid
    /// successors of this block. It is ok if `succ1` and `succ2` are the same successor, this will
    /// then be a no-op.
    ///
    /// If swap succeeds, an `Ok(_)` with the succidxs swapped will be returned. Otherwise, if the
    /// swap is not possible, the returned `Err(_)` will contain the succidxs in the original order.
    pub fn try_swap_succs(
        &mut self,
        succ1: SuccIdx,
        succ2: SuccIdx,
    ) -> Result<(SuccIdx, SuccIdx), (SuccIdx, SuccIdx)> {
        if succ1 == succ2 {
            return Ok((succ2, succ1));
        }
        match &mut self.terminator {
            Terminator::BranchIf(cond, _, _, true_target, false_target) => {
                assert!(succ1.0 == 0 && succ2.0 == 1 || succ1.0 == 1 && succ2.0 == 0);
                *cond = match cond {
                    BCond::Eq => BCond::Ne,
                    BCond::Ne => BCond::Eq,
                };
                std::mem::swap(true_target, false_target);
                Ok((succ2, succ1))
            }
            Terminator::BranchIfZ(cond, _, true_target, false_target) => {
                assert!(succ1.0 == 0 && succ2.0 == 1 || succ1.0 == 1 && succ2.0 == 0);
                *cond = match cond {
                    BZCond::GeZ => BZCond::LtZ,
                    BZCond::GtZ => BZCond::LeZ,
                    BZCond::LeZ => BZCond::GtZ,
                    BZCond::LtZ => BZCond::GeZ,
                };
                std::mem::swap(true_target, false_target);
                Ok((succ2, succ1))
            }

            Terminator::BranchIfFCond(cond, true_target, false_target) => {
                assert!(succ1.0 == 0 && succ2.0 == 1 || succ1.0 == 1 && succ2.0 == 0);
                *cond = !*cond;
                std::mem::swap(true_target, false_target);
                Ok((succ2, succ1))
            }
            Terminator::Jump(_)
            | Terminator::ReturnToRa
            | Terminator::Virtual(VirtualTerminator::Return(_)) => {
                // These terminators have no successors.
                panic!("invalid arguments supplied to try_swap_succs")
            }
            Terminator::BranchIfZAndLink(_, _, _, _)
            | Terminator::JumpAndLink(_, _)
            | Terminator::JumpAndLinkRa(_, _)
            | Terminator::JumpAndLinkReg(_, _, _) => {
                // These terminators have only one successor. This can only be valid if
                // `succ1 == succ2`, which is already handled above.
                panic!("invalid arguments supplied to try_swap_succs")
            }
        }
    }

    // ---------------------------------------------------------------------------------------------

    pub fn has_preds_in(&self, function: &Function) -> bool {
        function.blocks().any(|b| {
            b.succidxs()
                .map(|idx| b.succ_id(idx))
                .any(|id| id == self.id())
        })
    }

    pub fn has_succs_in(&self, function: &Function) -> bool {
        self.succidxs()
            .map(|idx| self.succ_id(idx))
            .any(|id| function.has_block(id))
    }

    pub fn preds_in<'f>(&self, function: &'f Function) -> impl Iterator<Item = &'f BasicBlock> {
        let id = self.id();
        function
            .blocks()
            .filter(move |b| b.succidxs().map(|idx| b.succ_id(idx)).any(|s| s == id))
    }

    pub fn succs_in<'a, 'f: 'a>(
        &'a self,
        function: &'f Function,
    ) -> impl Iterator<Item = &'f BasicBlock> + 'a {
        self.succidxs()
            .map(|idx| self.succ_id(idx))
            .filter_map(|id| function.get_block(id))
    }

    pub fn has_dpreds_in(&self, function: &Function) -> bool {
        function
            .blocks()
            .any(|b| b.dsuccidx().map(|idx| b.succ_id(idx)) == Some(self.id()))
    }

    pub fn has_dsucc_in(&self, function: &Function) -> bool {
        self.dsuccidx()
            .map(|idx| self.succ_id(idx))
            .map(|id| function.has_block(id))
            .unwrap_or(false)
    }

    pub fn dpreds_in<'f>(&self, function: &'f Function) -> impl Iterator<Item = &'f BasicBlock> {
        let id = self.id();
        function
            .blocks()
            .filter(move |b| b.dsuccidx().map(|idx| b.succ_id(idx)) == Some(id))
    }

    pub fn dsuccidx_in(&self, function: &Function) -> Option<SuccIdx> {
        self.dsuccidx()
            .filter(|&idx| function.has_block(self.succ_id(idx)))
    }

    pub fn dsucc_in<'f>(&self, function: &'f Function) -> Option<&'f BasicBlock> {
        self.dsuccidx()
            .and_then(|idx| function.get_block(self.succ_id(idx)))
    }

    pub fn has_ndpreds_in(&self, function: &Function) -> bool {
        function.blocks().any(|b| {
            b.ndsuccidxs()
                .map(|idx| b.succ_id(idx))
                .any(|id| id == self.id())
        })
    }

    pub fn has_ndsuccs_in(&self, function: &Function) -> bool {
        self.ndsuccidxs()
            .map(|idx| self.succ_id(idx))
            .any(|id| function.has_block(id))
    }

    pub fn ndpreds_in<'f>(&self, function: &'f Function) -> impl Iterator<Item = &'f BasicBlock> {
        let id = self.id();
        function
            .blocks()
            .filter(move |b| b.ndsuccidxs().map(|idx| b.succ_id(idx)).any(|s| s == id))
    }

    pub fn ndsuccidxs_in<'a, 'f: 'a>(
        &'a self,
        function: &'f Function,
    ) -> impl Iterator<Item = SuccIdx> + 'a {
        self.ndsuccidxs()
            .filter(|&idx| function.has_block(self.succ_id(idx)))
    }

    pub fn ndsuccs_in<'a, 'f: 'a>(
        &'a self,
        function: &'f Function,
    ) -> impl Iterator<Item = &'f BasicBlock> + 'a {
        self.ndsuccidxs()
            .filter_map(|idx| function.get_block(self.succ_id(idx)))
    }
}
