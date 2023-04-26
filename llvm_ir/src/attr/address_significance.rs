use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddressSignificance {
    /// Address is not significant with the module.
    LocalUnnamed,
    /// Address is not significant.
    Unnamed,
}

impl crate::FmtAsLlvmAsm for AddressSignificance {
    fn fmt_as_llvm_asm(&self, f: &mut fmt::Formatter, _opts: &crate::FmtOpts) -> fmt::Result {
        f.write_str(match self {
            Self::LocalUnnamed => "local_unnamed_addr",
            Self::Unnamed => "unnamed_addr",
        })
    }
}
