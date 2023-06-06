use super::{BlockId, BlockRef};
use crate::{BCond, BZCond, Terminator, VirtualTerminator};
use arrayvec::ArrayVec;

impl Terminator {
    pub fn has_targets(&self) -> bool {
        self.targets().next().is_some()
    }

    pub fn targets(&self) -> impl Iterator<Item = &BlockRef> {
        let mut arr = ArrayVec::<&BlockRef, 2>::new();
        match self {
            Terminator::BranchIf(_, _, _, tt, ft)
            | Terminator::BranchIfZ(_, _, tt, ft)
            | Terminator::BranchIfFCond(_, tt, ft) => {
                arr.push(tt);
                arr.push(ft);
            }
            Terminator::BranchIfZAndLink(_, _, _, ft) => arr.push(ft),
            Terminator::Jump(t) => arr.push(t),
            Terminator::JumpAndLinkRa(_, nb) | Terminator::JumpAndLinkReg(_, _, nb) => arr.push(nb),
            Terminator::Syscall(Some(nb)) => arr.push(nb),
            Terminator::Syscall(None)
            | Terminator::ReturnToRa
            | Terminator::Virtual(VirtualTerminator::Return(_)) => {}
        }
        arr.into_iter()
    }

    pub fn targets_mut(&mut self) -> impl Iterator<Item = &mut BlockRef> {
        let mut arr = ArrayVec::<&mut BlockRef, 2>::new();
        match self {
            Terminator::BranchIf(_, _, _, tt, ft)
            | Terminator::BranchIfZ(_, _, tt, ft)
            | Terminator::BranchIfFCond(_, tt, ft) => {
                arr.push(tt);
                arr.push(ft);
            }
            Terminator::BranchIfZAndLink(_, _, _, ft) => arr.push(ft),
            Terminator::Jump(t) => arr.push(t),
            Terminator::JumpAndLinkRa(_, nb) | Terminator::JumpAndLinkReg(_, _, nb) => arr.push(nb),
            Terminator::Syscall(Some(nb)) => arr.push(nb),
            Terminator::Syscall(None)
            | Terminator::ReturnToRa
            | Terminator::Virtual(VirtualTerminator::Return(_)) => {}
        }
        arr.into_iter()
    }

    pub fn target(&self, id: BlockId) -> Option<&BlockRef> {
        self.targets().find(|t| t.id == id)
    }

    pub fn target_mut(&mut self, id: BlockId) -> Option<&mut BlockRef> {
        self.targets_mut().find(|t| t.id == id)
    }

    pub fn default_target(&self) -> Option<&BlockRef> {
        match self {
            Terminator::BranchIf(_, _, _, _, t)
            | Terminator::BranchIfZ(_, _, _, t)
            | Terminator::BranchIfZAndLink(_, _, _, t)
            | Terminator::BranchIfFCond(_, _, t)
            | Terminator::JumpAndLinkRa(_, t)
            | Terminator::JumpAndLinkReg(_, _, t)
            | Terminator::Syscall(Some(t)) => Some(t),
            Terminator::Jump(_)
            | Terminator::ReturnToRa
            | Terminator::Syscall(None)
            | Terminator::Virtual(VirtualTerminator::Return(_)) => None,
        }
    }

    pub fn default_target_mut(&mut self) -> Option<&mut BlockRef> {
        match self {
            Terminator::BranchIf(_, _, _, _, t)
            | Terminator::BranchIfZ(_, _, _, t)
            | Terminator::BranchIfZAndLink(_, _, _, t)
            | Terminator::BranchIfFCond(_, _, t)
            | Terminator::JumpAndLinkRa(_, t)
            | Terminator::JumpAndLinkReg(_, _, t)
            | Terminator::Syscall(Some(t)) => Some(t),
            Terminator::Jump(_)
            | Terminator::ReturnToRa
            | Terminator::Syscall(None)
            | Terminator::Virtual(VirtualTerminator::Return(_)) => None,
        }
    }

    pub fn try_swap_targets(
        &mut self,
        succ1: BlockId,
        succ2: BlockId,
    ) -> Result<(BlockId, BlockId), (BlockId, BlockId)> {
        match self {
            Terminator::BranchIf(cond, _, _, tt, ft) => {
                assert!(tt.id == succ1 && ft.id == succ2 || tt.id == succ2 && ft.id == succ1);
                *cond = match cond {
                    BCond::Eq => BCond::Ne,
                    BCond::Ne => BCond::Eq,
                };
                std::mem::swap(tt, ft);
                Ok((succ2, succ1))
            }
            Terminator::BranchIfZ(cond, _, tt, ft) => {
                assert!(tt.id == succ1 && ft.id == succ2 || tt.id == succ2 && ft.id == succ1);
                *cond = match cond {
                    BZCond::GeZ => BZCond::LtZ,
                    BZCond::GtZ => BZCond::LeZ,
                    BZCond::LeZ => BZCond::GtZ,
                    BZCond::LtZ => BZCond::GeZ,
                };
                std::mem::swap(tt, ft);
                Ok((succ2, succ1))
            }

            Terminator::BranchIfFCond(cond, tt, ft) => {
                assert!(tt.id == succ1 && ft.id == succ2 || tt.id == succ2 && ft.id == succ1);
                *cond = !*cond;
                std::mem::swap(tt, ft);
                Ok((succ2, succ1))
            }
            Terminator::Jump(_)
            | Terminator::ReturnToRa
            | Terminator::Syscall(None)
            | Terminator::Virtual(VirtualTerminator::Return(_)) => {
                // These terminators have no successors.
                panic!("invalid arguments supplied to try_swap_succs")
            }
            Terminator::BranchIfZAndLink(_, _, _, _)
            | Terminator::JumpAndLinkRa(_, _)
            | Terminator::JumpAndLinkReg(_, _, _)
            | Terminator::Syscall(Some(_)) => {
                // These terminators have only one successor.
                panic!("invalid arguments supplied to try_swap_succs")
            }
        }
    }
}
