use mips_ir as mir;

use crate::ir::table::ItemId;

#[derive(Debug)]
pub enum MipsValue {
    Imm(u16),
    Reg(mir::Reg),
    FReg(mir::FReg),
}

impl From<mir::Reg> for MipsValue {
    fn from(reg: mir::Reg) -> Self {
        Self::Reg(reg)
    }
}

impl From<mir::FReg> for MipsValue {
    fn from(freg: mir::FReg) -> Self {
        Self::FReg(freg)
    }
}

impl From<mir::AnyReg> for MipsValue {
    fn from(reg: mir::AnyReg) -> Self {
        match reg {
            mir::AnyReg::R(reg) => reg.into(),
            mir::AnyReg::F(reg) => reg.into(),
        }
    }
}

#[derive(Debug)]
pub enum MipsLvalue {
    // The register contains a pointer to the value
    Address(mir::Reg),
    // The value is in only some register specifed by the ItemId
    Reg(ItemId),
}
