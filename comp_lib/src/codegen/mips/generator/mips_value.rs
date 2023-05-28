use mips_ir as mir;

use crate::ir::table::ItemId;

#[derive(Debug)]
pub enum MipsCondOrValue {
    FloatCond { inverse: bool },
    Value(MipsValue),
}

impl From<mir::Reg> for MipsCondOrValue {
    fn from(reg: mir::Reg) -> Self {
        Self::Value(reg.into())
    }
}

impl From<mir::FReg> for MipsCondOrValue {
    fn from(freg: mir::FReg) -> Self {
        Self::Value(freg.into())
    }
}

impl From<mir::AnyReg> for MipsCondOrValue {
    fn from(reg: mir::AnyReg) -> Self {
        Self::Value(reg.into())
    }
}

impl From<MipsValue> for MipsCondOrValue {
    fn from(value: MipsValue) -> Self {
        Self::Value(value)
    }
}

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
pub enum StackedMipsValue {
    /// The value is a imidiat
    Imm(u16),
    /// The value is in the builders stack, take care of the order
    InStack,
}

#[derive(Debug)]
pub enum MipsLvalue {
    // The register contains a pointer to the value
    Address(mir::Reg),
    // The value is in only some register specifed by the ItemId
    Reg(ItemId),
}
