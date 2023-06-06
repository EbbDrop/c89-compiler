use crate::{AnyReg, Reg, Terminator, VirtualTerminator};

use super::Uses;

impl Terminator {
    pub fn uses(&self) -> Uses {
        Uses(match *self {
            Terminator::BranchIf(_, rs, rt, _, _) => vec![rs.into(), rt.into()].into_iter(),
            Terminator::BranchIfZ(_, rs, _, _) => vec![rs.into()].into_iter(),
            Terminator::BranchIfZAndLink(_, rs, _, _) => vec![rs.into()].into_iter(),
            Terminator::BranchIfFCond(_, _, _) => Vec::new().into_iter(),
            Terminator::Jump(_) => Vec::new().into_iter(),
            Terminator::ReturnToRa => vec![Reg::RA.into()].into_iter(),
            Terminator::Syscall(_) => vec![Reg::V0.into()].into_iter(),
            Terminator::JumpAndLinkRa(_, _) => Vec::new().into_iter(),
            Terminator::JumpAndLinkReg(rd, rs, _) => vec![rd.into(), rs.into()].into_iter(),
            Terminator::Virtual(ref virt) => return virt.uses(),
        })
    }

    pub fn map_uses(&mut self, mut f: impl FnMut(AnyReg) -> AnyReg) {
        match self {
            Terminator::BranchIf(_, rs, rt, _, _) => {
                *rs = f((*rs).into()).try_into().unwrap();
                *rt = f((*rt).into()).try_into().unwrap()
            }
            Terminator::BranchIfZ(_, rs, _, _) => *rs = f((*rs).into()).try_into().unwrap(),
            Terminator::BranchIfZAndLink(_, rs, _, _) => *rs = f((*rs).into()).try_into().unwrap(),
            Terminator::BranchIfFCond(_, _, _) => (),
            Terminator::Jump(_) => (),
            Terminator::ReturnToRa => (),
            Terminator::Syscall(_) => (),
            Terminator::JumpAndLinkRa(_, _) => (),
            Terminator::JumpAndLinkReg(rd, rs, _) => {
                *rd = f((*rd).into()).try_into().unwrap();
                *rs = f((*rs).into()).try_into().unwrap();
            }
            Terminator::Virtual(virt) => virt.map_uses(f),
        }
    }

    pub fn phi_uses(&self) -> impl Iterator<Item = AnyReg> + '_ {
        self.targets().flat_map(|bref| &bref.arguments).copied()
    }

    pub fn map_phi_uses(&mut self, mut f: impl FnMut(AnyReg) -> AnyReg) {
        for bref in self.targets_mut() {
            for arg in &mut bref.arguments {
                *arg = f(*arg);
            }
        }
    }

    pub fn all_uses(&self) -> impl Iterator<Item = AnyReg> + '_ {
        self.uses().chain(self.phi_uses())
    }

    pub fn map_all_uses(&mut self, mut f: impl FnMut(AnyReg) -> AnyReg) {
        self.map_uses(&mut f);
        self.map_phi_uses(f);
    }
}

impl VirtualTerminator {
    pub fn uses(&self) -> Uses {
        Uses(match self {
            VirtualTerminator::Return(r) => Vec::from_iter(*r).into_iter(),
        })
    }

    pub fn map_uses(&mut self, mut f: impl FnMut(AnyReg) -> AnyReg) {
        match self {
            VirtualTerminator::Return(r) => {
                if let Some(r) = r.as_mut() {
                    *r = f(*r);
                }
            }
        }
    }
}
