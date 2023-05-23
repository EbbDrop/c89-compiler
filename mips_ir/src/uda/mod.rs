//! Use-Def Analyzer

use crate::{
    AnyReg, FImmOp, FRegOp2, FRegOp3, FunctionCall, ImmOp1, ImmOp2, Instruction, PseudoInstruction,
    Reg, RegOp1, RegOp2, RegOp3, Terminator, VirtualInstruction, VirtualTerminator,
};

/// Registers a single instruction/terminator uses and defines. Assume `uses` will always happen
/// 'before' the `defs`. Note that the usedefs may be incomplete if non-virtual registers are
/// involved, e.g. HI and LO registers are ignored, a use of an even fpu register as double fmt
/// won't add a use for the corresponding odd fpu register, etc.
#[derive(Debug, Clone, Default)]
pub struct RegUseDefs {
    pub uses: Vec<AnyReg>,
    pub defs: Vec<AnyReg>,
}

impl RegUseDefs {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_uses(uses: Vec<AnyReg>) -> Self {
        Self {
            uses,
            ..Default::default()
        }
    }

    pub fn with_use(use_: AnyReg) -> Self {
        Self {
            uses: vec![use_],
            ..Default::default()
        }
    }

    pub fn with_defs(defs: Vec<AnyReg>) -> Self {
        Self {
            defs,
            ..Default::default()
        }
    }

    pub fn with_def(def: AnyReg) -> Self {
        Self {
            defs: vec![def],
            ..Default::default()
        }
    }

    pub fn add_uses(&mut self, uses: &[AnyReg]) -> &mut Self {
        self.uses.extend_from_slice(uses);
        self
    }

    pub fn add_use(&mut self, use_: AnyReg) -> &mut Self {
        self.uses.push(use_);
        self
    }

    pub fn add_defs(&mut self, defs: &[AnyReg]) -> &mut Self {
        self.defs.extend_from_slice(defs);
        self
    }

    pub fn add_def(&mut self, def: AnyReg) -> &mut Self {
        self.defs.push(def);
        self
    }
}

impl Instruction {
    pub fn reg_usedefs(&self) -> RegUseDefs {
        match *self {
            Instruction::Nop => RegUseDefs::new(),
            Instruction::Reg3(op, rd, rs, rt) => match op {
                RegOp3::AddS
                | RegOp3::AddU
                | RegOp3::SubS
                | RegOp3::SubU
                | RegOp3::And
                | RegOp3::Or
                | RegOp3::Nor
                | RegOp3::Xor
                | RegOp3::ShiftLeftLogical
                | RegOp3::ShiftRightLogical
                | RegOp3::ShiftRightArithmetic
                | RegOp3::SetLtS
                | RegOp3::SetLtU => RegUseDefs {
                    uses: vec![rs.into(), rt.into()],
                    defs: vec![rd.into()],
                },
            },
            Instruction::Reg2(op, rd, rs) => match op {
                // These instructions write their output to the LO and HI registers, but these are
                // not taken into account for Use-Def analysis.
                RegOp2::DivS | RegOp2::DivU | RegOp2::MultS | RegOp2::MultU => {
                    RegUseDefs::with_uses(vec![rd.into(), rs.into()])
                }
                RegOp2::TrapIf(_) => RegUseDefs::with_uses(vec![rd.into(), rs.into()]),
            },
            Instruction::Reg1(op, rd) => match op {
                RegOp1::MoveFromHi | RegOp1::MoveFromLo => RegUseDefs::with_def(rd.into()),
                RegOp1::MoveToHi | RegOp1::MoveToLo => RegUseDefs::with_use(rd.into()),
            },
            Instruction::Imm2(op, rt, rs, _) => match op {
                ImmOp2::AddS
                | ImmOp2::AddU
                | ImmOp2::And
                | ImmOp2::Or
                | ImmOp2::Xor
                | ImmOp2::ShiftLeftLogical
                | ImmOp2::ShiftRightLogical
                | ImmOp2::ShiftRightArithmetic
                | ImmOp2::SetLtS
                | ImmOp2::SetLtU => RegUseDefs {
                    uses: vec![rs.into()],
                    defs: vec![rt.into()],
                },
                ImmOp2::LoadByteS
                | ImmOp2::LoadByteU
                | ImmOp2::LoadHalfS
                | ImmOp2::LoadHalfU
                | ImmOp2::LoadWord
                | ImmOp2::LoadWordLeft
                | ImmOp2::LoadWordRight => RegUseDefs {
                    uses: vec![rs.into()],
                    defs: vec![rt.into()],
                },
                ImmOp2::StoreByte
                | ImmOp2::StoreHalf
                | ImmOp2::StoreWord
                | ImmOp2::StoreWordLeft
                | ImmOp2::StoreWordRight => RegUseDefs::with_uses(vec![rt.into(), rs.into()]),
                ImmOp2::LoadLinkedWord => RegUseDefs {
                    uses: vec![rs.into()],
                    defs: vec![rt.into()],
                },
                ImmOp2::StoreConditionalWord => RegUseDefs::with_uses(vec![rt.into(), rs.into()]),
            },
            Instruction::Imm1(op, rt, _) => match op {
                ImmOp1::LoadUpper => RegUseDefs::with_def(rt.into()),
                ImmOp1::TrapIf(_) => RegUseDefs::with_use(rt.into()),
            },
            Instruction::FReg3(op, fd, fs, ft) => match op {
                FRegOp3::Add(_) | FRegOp3::Sub(_) | FRegOp3::Div(_) | FRegOp3::Mul(_) => {
                    RegUseDefs {
                        uses: vec![fs.into(), ft.into()],
                        defs: vec![fd.into()],
                    }
                }
            },
            Instruction::FReg2(op, fd, fs) => match op {
                FRegOp2::Abs(_) | FRegOp2::Neg(_) | FRegOp2::Sqrt(_) => RegUseDefs {
                    uses: vec![fs.into()],
                    defs: vec![fd.into()],
                },
                // This sets the fpu cc depending on the result. This is not taken into account for
                // Use-Def analysis.
                FRegOp2::Cmp(_) => RegUseDefs::with_uses(vec![fs.into(), fd.into()]),
                FRegOp2::Convert(_, _)
                | FRegOp2::ConvertToWord(_)
                | FRegOp2::ConvertFromWord(_)
                | FRegOp2::Move(_) => RegUseDefs {
                    uses: vec![fs.into()],
                    defs: vec![fd.into()],
                },
            },
            Instruction::FImm(op, ft, base, _) => match op {
                FImmOp::LoadWordToFpu | FImmOp::LoadDoublewordToFpu => RegUseDefs {
                    uses: vec![base.into()],
                    defs: vec![ft.into()],
                },
                FImmOp::StoreWordFromFpu | FImmOp::StoreDoublewordFromFpu => {
                    RegUseDefs::with_uses(vec![ft.into(), base.into()])
                }
            },
            Instruction::MoveFromFpu(rt, fs) => RegUseDefs {
                uses: vec![fs.into()],
                defs: vec![rt.into()],
            },
            Instruction::MoveToFpu(rt, fs) => RegUseDefs {
                uses: vec![rt.into()],
                defs: vec![fs.into()],
            },
            Instruction::Syscall => RegUseDefs::new(),
            Instruction::Break => RegUseDefs::new(),
            Instruction::Pseudo(ref pseudo) => pseudo.reg_usedefs(),
            Instruction::Virtual(ref virt) => virt.reg_usedefs(),
        }
    }
}

impl PseudoInstruction {
    pub fn reg_usedefs(&self) -> RegUseDefs {
        match *self {
            PseudoInstruction::LoadAddress(rt, _) => RegUseDefs::with_def(rt.into()),
        }
    }
}

impl VirtualInstruction {
    pub fn reg_usedefs(&self) -> RegUseDefs {
        match self {
            VirtualInstruction::FunctionCall(FunctionCall {
                label: _,
                return_reg,
                arguments,
            }) => RegUseDefs {
                uses: arguments.iter().map(|(r, _)| *r).collect(),
                defs: (*return_reg).into_iter().collect(),
            },
        }
    }
}

impl Terminator {
    pub fn reg_usedefs(&self) -> RegUseDefs {
        match *self {
            Terminator::BranchIf(_, rs, rt, _, _) => {
                RegUseDefs::with_uses(vec![rs.into(), rt.into()])
            }
            Terminator::BranchIfZ(_, rs, _, _) => RegUseDefs::with_use(rs.into()),
            Terminator::BranchIfZAndLink(_, rs, _, _) => RegUseDefs::with_use(rs.into()),
            Terminator::BranchIfFCond(_, _, _) => RegUseDefs::new(),
            Terminator::Jump(_) => RegUseDefs::new(),
            Terminator::JumpAndLink(_, _) => RegUseDefs::new(),
            Terminator::ReturnToRa => RegUseDefs::with_use(Reg::RA.into()),
            Terminator::JumpAndLinkRa(_, _) => RegUseDefs::new(),
            Terminator::JumpAndLinkReg(rd, rs, _) => {
                RegUseDefs::with_uses(vec![rd.into(), rs.into()])
            }
            Terminator::Virtual(ref virt) => virt.reg_usedefs(),
        }
    }
}

impl VirtualTerminator {
    pub fn reg_usedefs(&self) -> RegUseDefs {
        match self {
            VirtualTerminator::Return(r) => RegUseDefs::with_uses((*r).into_iter().collect()),
        }
    }
}
