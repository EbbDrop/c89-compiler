use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Default,
    Hidden,
    Protected,
}

impl Visibility {
    pub const fn default() -> Self {
        Self::Default
    }
}

impl Default for Visibility {
    fn default() -> Self {
        Self::default()
    }
}

impl crate::FmtAsLlvmAsm for Visibility {
    fn fmt_as_llvm_asm(&self, f: &mut fmt::Formatter, _opts: &crate::FmtOpts) -> fmt::Result {
        f.write_str(match self {
            Self::Default => "default",
            Self::Hidden => "hidden",
            Self::Protected => "protected",
        })
    }
}
