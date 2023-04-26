use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Linkage {
    Private,
    Internal,
    AvailableExternally,
    Linkonce,
    Weak,
    Common,
    Appending,
    ExternWeak,
    LinkonceOdr,
    WeakOdr,
    External,
}

impl Linkage {
    pub const fn default() -> Self {
        Self::External
    }
}

impl Default for Linkage {
    fn default() -> Self {
        Self::default()
    }
}

impl crate::FmtAsLlvmAsm for Linkage {
    fn fmt_as_llvm_asm(&self, f: &mut fmt::Formatter, _opts: &crate::FmtOpts) -> fmt::Result {
        f.write_str(match self {
            Self::Private => "private",
            Self::Internal => "internal",
            Self::AvailableExternally => "available_externally",
            Self::Linkonce => "linkonce",
            Self::Weak => "weak",
            Self::Common => "common",
            Self::Appending => "appending",
            Self::ExternWeak => "extern_weak",
            Self::LinkonceOdr => "linkonce_odr",
            Self::WeakOdr => "weak_odr",
            Self::External => "external",
        })
    }
}
