use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Alignment(pub u32);

impl Alignment {
    pub const fn default() -> Self {
        Self(0)
    }
}

impl Default for Alignment {
    fn default() -> Self {
        Self::default()
    }
}

impl crate::FmtAsLlvmAsm for Alignment {
    fn fmt_as_llvm_asm(&self, f: &mut fmt::Formatter, _opts: &crate::FmtOpts) -> fmt::Result {
        write!(f, "align {}", self.0)
    }
}
