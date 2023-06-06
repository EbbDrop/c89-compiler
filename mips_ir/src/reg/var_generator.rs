use crate::{AnyReg, FReg, Reg};

/// Generates new virtual AnyReg's.
#[derive(Debug, Clone)]
pub struct VARGenerator {
    next_reg: u32,
    next_freg_single: u32,
    next_freg_double: u32,
}

impl VARGenerator {
    pub(crate) fn new(next_reg: u32, next_freg_single: u32, next_freg_double: u32) -> Self {
        Self {
            next_reg,
            next_freg_single,
            next_freg_double,
        }
    }

    pub fn next_of_type(&mut self, reg: AnyReg) -> AnyReg {
        match reg {
            AnyReg::R(_) => self.next_reg().into(),
            AnyReg::F(freg) if freg.is_double() => self.next_freg_double().into(),
            AnyReg::F(_) => self.next_freg_single().into(),
        }
    }

    pub fn next_reg(&mut self) -> Reg {
        let n = self.next_reg;
        self.next_reg += 1;
        Reg::Virtual(n)
    }

    pub fn next_freg_single(&mut self) -> FReg {
        let n = self.next_freg_single;
        self.next_freg_single += 1;
        FReg::VirtualSingle(n)
    }

    pub fn next_freg_double(&mut self) -> FReg {
        let n = self.next_freg_double;
        self.next_freg_double += 1;
        FReg::VirtualDouble(n)
    }
}
