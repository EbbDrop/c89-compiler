use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AddressSpace(pub u32);

impl AddressSpace {
    pub const fn default() -> Self {
        Self(0)
    }
}

impl Default for AddressSpace {
    fn default() -> Self {
        Self::default()
    }
}

impl crate::FmtAsLlvmAsm for AddressSpace {
    fn fmt_as_llvm_asm(&self, f: &mut fmt::Formatter, _opts: &crate::FmtOpts) -> fmt::Result {
        write!(f, "addrspace({})", self.0)
    }
}
