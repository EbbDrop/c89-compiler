//! Register Usage and Definition Info of instructions.

use crate::{
    instruction::MemOp, AnyReg, FImmOp, FRegOp2, FRegOp3, FunctionCall, ImmOp1, ImmOp2,
    Instruction, PseudoInstruction, RegOp1, RegOp2, RegOp3, VirtualInstruction,
};

use super::{Defs, Uses};

impl Iterator for Defs {
    type Item = AnyReg;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.take()
    }
}

impl Iterator for Uses {
    type Item = AnyReg;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl Instruction {
    pub fn defs(&self) -> Defs {
        Defs(match *self {
            Instruction::Nop => None,
            Instruction::Reg3(op, rd, _rs, _rt) => match op {
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
                | RegOp3::SetLtU => Some(rd.into()),
            },
            Instruction::Reg2(op, _rd, _rs) => match op {
                // These instructions write their output to the LO and HI registers, but these are
                // not taken into account for Use-Def analysis.
                RegOp2::DivS | RegOp2::DivU | RegOp2::MultS | RegOp2::MultU => None,
                RegOp2::TrapIf(_) => None,
            },
            Instruction::Reg1(op, rd) => match op {
                RegOp1::MoveFromHi | RegOp1::MoveFromLo => Some(rd.into()),
                RegOp1::MoveToHi | RegOp1::MoveToLo => None,
            },
            Instruction::Imm2(op, rt, _rs, _imm) => match op {
                ImmOp2::AddS
                | ImmOp2::AddU
                | ImmOp2::And
                | ImmOp2::Or
                | ImmOp2::Xor
                | ImmOp2::ShiftLeftLogical
                | ImmOp2::ShiftRightLogical
                | ImmOp2::ShiftRightArithmetic
                | ImmOp2::SetLtS
                | ImmOp2::SetLtU => Some(rt.into()),
            },
            Instruction::Mem(op, rt, _base, _offset) => match op {
                MemOp::LoadByteS
                | MemOp::LoadByteU
                | MemOp::LoadHalfS
                | MemOp::LoadHalfU
                | MemOp::LoadWord
                | MemOp::LoadWordLeft
                | MemOp::LoadWordRight
                | MemOp::LoadLinkedWord => Some(rt.into()),
                MemOp::StoreByte
                | MemOp::StoreHalf
                | MemOp::StoreWord
                | MemOp::StoreWordLeft
                | MemOp::StoreWordRight
                | MemOp::StoreConditionalWord => None,
            },
            Instruction::Imm1(op, rt, _) => match op {
                ImmOp1::LoadUpper => Some(rt.into()),
                ImmOp1::TrapIf(_) => None,
            },
            Instruction::FReg3(op, fd, _fs, _ft) => match op {
                FRegOp3::Add(_) | FRegOp3::Sub(_) | FRegOp3::Div(_) | FRegOp3::Mul(_) => {
                    Some(fd.into())
                }
            },
            Instruction::FReg2(op, fd, _fs) => match op {
                FRegOp2::Abs(_) | FRegOp2::Neg(_) | FRegOp2::Sqrt(_) => Some(fd.into()),
                // This sets the fpu cc depending on the result. This is not taken into account for
                // Use-Def analysis.
                FRegOp2::Cmp(_) => None,
                FRegOp2::Convert(_, _)
                | FRegOp2::ConvertToWord(_)
                | FRegOp2::ConvertFromWord(_)
                | FRegOp2::Move(_) => Some(fd.into()),
            },
            Instruction::FImm(op, ft, _base, _offset) => match op {
                FImmOp::LoadWordToFpu | FImmOp::LoadDoublewordToFpu => Some(ft.into()),
                FImmOp::StoreWordFromFpu | FImmOp::StoreDoublewordFromFpu => None,
            },
            Instruction::MoveFromFpu(rt, _fs) => Some(rt.into()),
            Instruction::MoveToFpu(_rt, fs) => Some(fs.into()),
            Instruction::Break | Instruction::Call(_) => None,
            Instruction::Pseudo(ref pseudo) => return pseudo.defs(),
            Instruction::Virtual(ref virt) => return virt.defs(),
            Instruction::Hidden(_) => None,
            Instruction::Comment(_) => None,
        })
    }

    pub fn map_defs(&mut self, mut f: impl FnMut(AnyReg) -> AnyReg) {
        match self {
            Instruction::Nop => (),
            Instruction::Reg3(op, rd, _rs, _rt) => match op {
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
                | RegOp3::SetLtU => *rd = f((*rd).into()).try_into().unwrap(),
            },
            Instruction::Reg2(op, _rd, _rs) => match op {
                // These instructions write their output to the LO and HI registers, but these are
                // not taken into account for Use-Def analysis.
                RegOp2::DivS | RegOp2::DivU | RegOp2::MultS | RegOp2::MultU => (),
                RegOp2::TrapIf(_) => (),
            },
            Instruction::Reg1(op, rd) => match op {
                RegOp1::MoveFromHi | RegOp1::MoveFromLo => {
                    *rd = f((*rd).into()).try_into().unwrap()
                }
                RegOp1::MoveToHi | RegOp1::MoveToLo => (),
            },
            Instruction::Imm2(op, rt, _rs, _imm) => match op {
                ImmOp2::AddS
                | ImmOp2::AddU
                | ImmOp2::And
                | ImmOp2::Or
                | ImmOp2::Xor
                | ImmOp2::ShiftLeftLogical
                | ImmOp2::ShiftRightLogical
                | ImmOp2::ShiftRightArithmetic
                | ImmOp2::SetLtS
                | ImmOp2::SetLtU => *rt = f((*rt).into()).try_into().unwrap(),
            },
            Instruction::Mem(op, rt, _base, _offset) => match op {
                MemOp::LoadByteS
                | MemOp::LoadByteU
                | MemOp::LoadHalfS
                | MemOp::LoadHalfU
                | MemOp::LoadWord
                | MemOp::LoadWordLeft
                | MemOp::LoadWordRight
                | MemOp::LoadLinkedWord => *rt = f((*rt).into()).try_into().unwrap(),
                MemOp::StoreByte
                | MemOp::StoreHalf
                | MemOp::StoreWord
                | MemOp::StoreWordLeft
                | MemOp::StoreWordRight
                | MemOp::StoreConditionalWord => (),
            },
            Instruction::Imm1(op, rt, _) => match op {
                ImmOp1::LoadUpper => *rt = f((*rt).into()).try_into().unwrap(),
                ImmOp1::TrapIf(_) => (),
            },
            Instruction::FReg3(op, fd, _fs, _ft) => match op {
                FRegOp3::Add(_) | FRegOp3::Sub(_) | FRegOp3::Div(_) | FRegOp3::Mul(_) => {
                    *fd = f((*fd).into()).try_into().unwrap()
                }
            },
            Instruction::FReg2(op, fd, _fs) => match op {
                FRegOp2::Abs(_) | FRegOp2::Neg(_) | FRegOp2::Sqrt(_) => {
                    *fd = f((*fd).into()).try_into().unwrap()
                }
                // This sets the fpu cc depending on the result. This is not taken into account for
                // Use-Def analysis.
                FRegOp2::Cmp(_) => (),
                FRegOp2::Convert(_, _)
                | FRegOp2::ConvertToWord(_)
                | FRegOp2::ConvertFromWord(_)
                | FRegOp2::Move(_) => *fd = f((*fd).into()).try_into().unwrap(),
            },
            Instruction::FImm(op, ft, _base, _offset) => match op {
                FImmOp::LoadWordToFpu | FImmOp::LoadDoublewordToFpu => {
                    *ft = f((*ft).into()).try_into().unwrap()
                }
                FImmOp::StoreWordFromFpu | FImmOp::StoreDoublewordFromFpu => (),
            },
            Instruction::MoveFromFpu(rt, _fs) => *rt = f((*rt).into()).try_into().unwrap(),
            Instruction::MoveToFpu(_rt, fs) => *fs = f((*fs).into()).try_into().unwrap(),
            Instruction::Break | Instruction::Call(_) => (),
            Instruction::Pseudo(pseudo) => pseudo.map_defs(f),
            Instruction::Virtual(virt) => virt.map_defs(f),
            Instruction::Hidden(_) => (),
            Instruction::Comment(_) => (),
        }
    }

    pub fn uses(&self) -> Uses {
        Uses(
            match *self {
                Instruction::Nop => Vec::new(),
                Instruction::Reg3(op, _rd, rs, rt) => match op {
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
                    | RegOp3::SetLtU => vec![rs.into(), rt.into()],
                },
                Instruction::Reg2(op, rd, rs) => match op {
                    RegOp2::DivS
                    | RegOp2::DivU
                    | RegOp2::MultS
                    | RegOp2::MultU
                    | RegOp2::TrapIf(_) => {
                        vec![rd.into(), rs.into()]
                    }
                },
                Instruction::Reg1(op, rd) => match op {
                    RegOp1::MoveFromHi | RegOp1::MoveFromLo => Vec::new(),
                    RegOp1::MoveToHi | RegOp1::MoveToLo => vec![rd.into()],
                },
                Instruction::Imm2(op, _rt, rs, _imm) => match op {
                    ImmOp2::AddS
                    | ImmOp2::AddU
                    | ImmOp2::And
                    | ImmOp2::Or
                    | ImmOp2::Xor
                    | ImmOp2::ShiftLeftLogical
                    | ImmOp2::ShiftRightLogical
                    | ImmOp2::ShiftRightArithmetic
                    | ImmOp2::SetLtS
                    | ImmOp2::SetLtU => vec![rs.into()],
                },
                Instruction::Mem(op, rt, base, _offset) => match op {
                    MemOp::LoadByteS
                    | MemOp::LoadByteU
                    | MemOp::LoadHalfS
                    | MemOp::LoadHalfU
                    | MemOp::LoadWord
                    | MemOp::LoadWordLeft
                    | MemOp::LoadWordRight
                    | MemOp::LoadLinkedWord => vec![base.into()],
                    MemOp::StoreByte
                    | MemOp::StoreHalf
                    | MemOp::StoreWord
                    | MemOp::StoreWordLeft
                    | MemOp::StoreWordRight
                    | MemOp::StoreConditionalWord => vec![rt.into(), base.into()],
                },
                Instruction::Imm1(op, rt, _imm) => match op {
                    ImmOp1::LoadUpper => Vec::new(),
                    ImmOp1::TrapIf(_) => vec![rt.into()],
                },
                Instruction::FReg3(op, _fd, fs, ft) => match op {
                    FRegOp3::Add(_) | FRegOp3::Sub(_) | FRegOp3::Div(_) | FRegOp3::Mul(_) => {
                        vec![fs.into(), ft.into()]
                    }
                },
                Instruction::FReg2(op, fd, fs) => match op {
                    FRegOp2::Abs(_) | FRegOp2::Neg(_) | FRegOp2::Sqrt(_) => {
                        vec![fs.into()]
                    }
                    // This sets the fpu cc depending on the result. This is not taken into account for
                    // Use-Def analysis.
                    FRegOp2::Cmp(_) => vec![fs.into(), fd.into()],
                    FRegOp2::Convert(_, _)
                    | FRegOp2::ConvertToWord(_)
                    | FRegOp2::ConvertFromWord(_)
                    | FRegOp2::Move(_) => {
                        vec![fs.into()]
                    }
                },
                Instruction::FImm(op, ft, base, _imm) => match op {
                    FImmOp::LoadWordToFpu | FImmOp::LoadDoublewordToFpu => {
                        vec![base.into()]
                    }
                    FImmOp::StoreWordFromFpu | FImmOp::StoreDoublewordFromFpu => {
                        vec![ft.into(), base.into()]
                    }
                },
                Instruction::MoveFromFpu(_rt, fs) => {
                    vec![fs.into()]
                }
                Instruction::MoveToFpu(rt, _fs) => {
                    vec![rt.into()]
                }
                Instruction::Break | Instruction::Call(_) => Vec::new(),
                Instruction::Pseudo(ref pseudo) => return pseudo.uses(),
                Instruction::Virtual(ref virt) => return virt.uses(),
                Instruction::Hidden(_) => Vec::new(),
                Instruction::Comment(_) => Vec::new(),
            }
            .into_iter(),
        )
    }

    pub fn map_uses(&mut self, mut f: impl FnMut(AnyReg) -> AnyReg) {
        match self {
            Instruction::Nop => (),
            Instruction::Reg3(op, _rd, rs, rt) => match op {
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
                | RegOp3::SetLtU => {
                    *rs = f((*rs).into()).try_into().unwrap();
                    *rt = f((*rt).into()).try_into().unwrap();
                }
            },
            Instruction::Reg2(op, rd, rs) => match op {
                RegOp2::DivS | RegOp2::DivU | RegOp2::MultS | RegOp2::MultU | RegOp2::TrapIf(_) => {
                    *rd = f((*rd).into()).try_into().unwrap();
                    *rs = f((*rs).into()).try_into().unwrap();
                }
            },
            Instruction::Reg1(op, rd) => match op {
                RegOp1::MoveFromHi | RegOp1::MoveFromLo => (),
                RegOp1::MoveToHi | RegOp1::MoveToLo => *rd = f((*rd).into()).try_into().unwrap(),
            },
            Instruction::Imm2(op, _rt, rs, _imm) => match op {
                ImmOp2::AddS
                | ImmOp2::AddU
                | ImmOp2::And
                | ImmOp2::Or
                | ImmOp2::Xor
                | ImmOp2::ShiftLeftLogical
                | ImmOp2::ShiftRightLogical
                | ImmOp2::ShiftRightArithmetic
                | ImmOp2::SetLtS
                | ImmOp2::SetLtU => *rs = f((*rs).into()).try_into().unwrap(),
            },
            Instruction::Mem(op, rt, base, _offset) => match op {
                MemOp::LoadByteS
                | MemOp::LoadByteU
                | MemOp::LoadHalfS
                | MemOp::LoadHalfU
                | MemOp::LoadWord
                | MemOp::LoadWordLeft
                | MemOp::LoadWordRight
                | MemOp::LoadLinkedWord => *base = f((*base).into()).try_into().unwrap(),
                MemOp::StoreByte
                | MemOp::StoreHalf
                | MemOp::StoreWord
                | MemOp::StoreWordLeft
                | MemOp::StoreWordRight
                | MemOp::StoreConditionalWord => {
                    *rt = f((*rt).into()).try_into().unwrap();
                    *base = f((*base).into()).try_into().unwrap();
                }
            },
            Instruction::Imm1(op, rt, _imm) => match op {
                ImmOp1::LoadUpper => (),
                ImmOp1::TrapIf(_) => *rt = f((*rt).into()).try_into().unwrap(),
            },
            Instruction::FReg3(op, _fd, fs, ft) => match op {
                FRegOp3::Add(_) | FRegOp3::Sub(_) | FRegOp3::Div(_) | FRegOp3::Mul(_) => {
                    *fs = f((*fs).into()).try_into().unwrap();
                    *ft = f((*ft).into()).try_into().unwrap();
                }
            },
            Instruction::FReg2(op, fd, fs) => match op {
                FRegOp2::Abs(_) | FRegOp2::Neg(_) | FRegOp2::Sqrt(_) => {
                    *fs = f((*fs).into()).try_into().unwrap()
                }
                // This sets the fpu cc depending on the result. This is not taken into account for
                // Use-Def analysis.
                FRegOp2::Cmp(_) => {
                    *fs = f((*fs).into()).try_into().unwrap();
                    *fd = f((*fd).into()).try_into().unwrap();
                }
                FRegOp2::Convert(_, _)
                | FRegOp2::ConvertToWord(_)
                | FRegOp2::ConvertFromWord(_)
                | FRegOp2::Move(_) => *fs = f((*fs).into()).try_into().unwrap(),
            },
            Instruction::FImm(op, ft, base, _imm) => match op {
                FImmOp::LoadWordToFpu | FImmOp::LoadDoublewordToFpu => {
                    *base = f((*base).into()).try_into().unwrap()
                }
                FImmOp::StoreWordFromFpu | FImmOp::StoreDoublewordFromFpu => {
                    *ft = f((*ft).into()).try_into().unwrap();
                    *base = f((*base).into()).try_into().unwrap();
                }
            },
            Instruction::MoveFromFpu(_rt, fs) => *fs = f((*fs).into()).try_into().unwrap(),
            Instruction::MoveToFpu(rt, _fs) => *rt = f((*rt).into()).try_into().unwrap(),
            Instruction::Break | Instruction::Call(_) => (),
            Instruction::Pseudo(pseudo) => pseudo.map_uses(f),
            Instruction::Virtual(virt) => virt.map_uses(f),
            Instruction::Hidden(_) => (),
            Instruction::Comment(_) => (),
        }
    }

    pub fn has_side_effects(&self) -> bool {
        match self {
            Instruction::Nop => false,
            Instruction::Reg3(op, _, _, _) => match op {
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
                | RegOp3::SetLtU => false,
            },
            Instruction::Reg2(op, _, _) => match op {
                // Store the result in the HI and LO registers.
                RegOp2::DivS | RegOp2::DivU | RegOp2::MultS | RegOp2::MultU => true,
                RegOp2::TrapIf(_) => true,
            },
            Instruction::Reg1(op, _) => match op {
                RegOp1::MoveFromHi | RegOp1::MoveFromLo => false,
                RegOp1::MoveToHi | RegOp1::MoveToLo => true,
            },
            Instruction::Imm2(op, _, _, _) => match op {
                ImmOp2::AddS
                | ImmOp2::AddU
                | ImmOp2::And
                | ImmOp2::Or
                | ImmOp2::Xor
                | ImmOp2::ShiftLeftLogical
                | ImmOp2::ShiftRightLogical
                | ImmOp2::ShiftRightArithmetic
                | ImmOp2::SetLtS
                | ImmOp2::SetLtU => false,
            },
            Instruction::Mem(op, _rt, _base, _offset) => match op {
                MemOp::LoadByteS
                | MemOp::LoadByteU
                | MemOp::LoadHalfS
                | MemOp::LoadHalfU
                | MemOp::LoadWord
                | MemOp::LoadWordLeft
                | MemOp::LoadWordRight => false,
                MemOp::StoreByte
                | MemOp::StoreHalf
                | MemOp::StoreWord
                | MemOp::StoreWordLeft
                | MemOp::StoreWordRight => true,
                MemOp::LoadLinkedWord => true,
                MemOp::StoreConditionalWord => true,
            },
            Instruction::Imm1(op, _, _) => match op {
                ImmOp1::LoadUpper => false,
                ImmOp1::TrapIf(_) => true,
            },
            Instruction::FReg3(op, _, _, _) => match op {
                FRegOp3::Add(_) | FRegOp3::Sub(_) | FRegOp3::Div(_) | FRegOp3::Mul(_) => false,
            },
            Instruction::FReg2(op, _, _) => match op {
                FRegOp2::Abs(_) | FRegOp2::Neg(_) | FRegOp2::Sqrt(_) => false,
                // Sets the FPU CC depending on the result.
                FRegOp2::Cmp(_) => true,
                FRegOp2::Convert(_, _)
                | FRegOp2::ConvertToWord(_)
                | FRegOp2::ConvertFromWord(_)
                | FRegOp2::Move(_) => false,
            },
            Instruction::FImm(op, _, _, _) => match op {
                FImmOp::LoadWordToFpu | FImmOp::LoadDoublewordToFpu => false,
                FImmOp::StoreWordFromFpu | FImmOp::StoreDoublewordFromFpu => true,
            },
            Instruction::MoveFromFpu(_, _) => false,
            Instruction::MoveToFpu(_, _) => false,
            Instruction::Break => true,
            Instruction::Call(_) => true,
            Instruction::Pseudo(ref pseudo) => pseudo.has_side_effects(),
            Instruction::Virtual(ref virt) => virt.has_side_effects(),
            Instruction::Hidden(_) => true,
            Instruction::Comment(_) => true,
        }
    }
}

impl PseudoInstruction {
    pub fn defs(&self) -> Defs {
        Defs(match *self {
            PseudoInstruction::LoadAddress(rt, _) => Some(rt.into()),
        })
    }

    pub fn map_defs(&mut self, mut f: impl FnMut(AnyReg) -> AnyReg) {
        match self {
            PseudoInstruction::LoadAddress(rt, _) => *rt = f((*rt).into()).try_into().unwrap(),
        }
    }

    pub fn uses(&self) -> Uses {
        match self {
            PseudoInstruction::LoadAddress(_, _) => Uses(Vec::new().into_iter()),
        }
    }

    pub fn map_uses(&mut self, mut _f: impl FnMut(AnyReg) -> AnyReg) {
        match self {
            PseudoInstruction::LoadAddress(_, _) => (),
        }
    }

    pub fn has_side_effects(&self) -> bool {
        match self {
            PseudoInstruction::LoadAddress(_, _) => false,
        }
    }
}

impl VirtualInstruction {
    pub fn defs(&self) -> Defs {
        Defs(match self {
            VirtualInstruction::FunctionCall(FunctionCall { return_reg, .. }) => *return_reg,
            &VirtualInstruction::Declare(reg) => Some(reg),
            &VirtualInstruction::Move { dst, .. } => Some(dst),
            &VirtualInstruction::LoadStackAddress { reg, .. } => Some(reg.into()),
            &VirtualInstruction::LoadFromStack { reg, .. } => Some(reg),
            VirtualInstruction::StoreToStack { .. } => None,
        })
    }

    pub fn map_defs(&mut self, mut f: impl FnMut(AnyReg) -> AnyReg) {
        match self {
            VirtualInstruction::FunctionCall(FunctionCall { return_reg, .. }) => {
                if let Some(r) = return_reg.as_mut() {
                    *r = f(*r)
                }
            }
            VirtualInstruction::Declare(reg) => *reg = f(*reg),
            VirtualInstruction::Move { dst, .. } => *dst = f(*dst),
            VirtualInstruction::LoadStackAddress { reg, .. } => {
                *reg = f((*reg).into()).try_into().unwrap()
            }
            VirtualInstruction::LoadFromStack { reg, .. } => *reg = f(*reg),
            VirtualInstruction::StoreToStack { .. } => (),
        }
    }

    pub fn uses(&self) -> Uses {
        Uses(match self {
            VirtualInstruction::FunctionCall(FunctionCall { arguments, .. }) => arguments
                .iter()
                .map(|(r, _)| *r)
                .collect::<Vec<_>>()
                .into_iter(),
            &VirtualInstruction::Declare(_) => Vec::new().into_iter(),
            &VirtualInstruction::Move { src, .. } => vec![src].into_iter(),
            VirtualInstruction::LoadStackAddress { .. } => Vec::new().into_iter(),
            VirtualInstruction::LoadFromStack { .. } => Vec::new().into_iter(),
            &VirtualInstruction::StoreToStack { reg, .. } => vec![reg].into_iter(),
        })
    }

    pub fn map_uses(&mut self, mut f: impl FnMut(AnyReg) -> AnyReg) {
        match self {
            VirtualInstruction::FunctionCall(FunctionCall { arguments, .. }) => {
                for (arg, _) in arguments {
                    *arg = f(*arg)
                }
            }
            VirtualInstruction::Declare(_) => (),
            VirtualInstruction::Move { src, .. } => *src = f(*src),
            VirtualInstruction::LoadStackAddress { .. } => (),
            VirtualInstruction::LoadFromStack { .. } => (),
            VirtualInstruction::StoreToStack { reg, .. } => *reg = f(*reg),
        }
    }

    pub fn has_side_effects(&self) -> bool {
        match self {
            VirtualInstruction::FunctionCall(_) => true,
            VirtualInstruction::Declare(_) => false,
            VirtualInstruction::Move { .. } => false,
            VirtualInstruction::LoadStackAddress { .. } => false,
            VirtualInstruction::LoadFromStack { .. } => false,
            VirtualInstruction::StoreToStack { .. } => true,
        }
    }
}
