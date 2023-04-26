use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CallingConv(pub u32);

impl CallingConv {
    pub const fn default() -> Self {
        Self(cc::C)
    }
}

impl Default for CallingConv {
    fn default() -> Self {
        Self::default()
    }
}

pub mod cc {
    pub const C: u32 = 0;
    pub const FAST: u32 = 8;
    pub const COLD: u32 = 9;
    pub const GHC: u32 = 10;
    pub const HIPE: u32 = 11;
    pub const WEBKIT_JS: u32 = 12;
    pub const ANY_REG: u32 = 13;
    pub const PRESERVE_MOST: u32 = 14;
    pub const PRESERVE_ALL: u32 = 15;
    pub const SWIFT: u32 = 16;
    pub const CXX_FAST_TLS: u32 = 17;
    pub const TAIL: u32 = 18;
    pub const CFGUARD_CHECK: u32 = 19;
    pub const SWIFT_TAIL: u32 = 20;
    pub const X86_STDCALL: u32 = 64;
    pub const X86_FASTCALL: u32 = 65;
    pub const ARM_APCS: u32 = 66;
    pub const ARM_AAPCS: u32 = 67;
    pub const ARM_APPCS_VFP: u32 = 68;
}

impl crate::FmtAsLlvmAsm for CallingConv {
    fn fmt_as_llvm_asm(&self, f: &mut fmt::Formatter, _opts: &crate::FmtOpts) -> fmt::Result {
        match self.0 {
            cc::C => f.write_str("ccc"),
            cc::FAST => f.write_str("fastcc"),
            cc::COLD => f.write_str("coldcc"),
            cc::WEBKIT_JS => f.write_str("webkit_jscc"),
            cc::ANY_REG => f.write_str("anyregcc"),
            cc::PRESERVE_MOST => f.write_str("preserve_mostcc"),
            cc::PRESERVE_ALL => f.write_str("preserve_allcc"),
            cc::SWIFT => f.write_str("swiftcc"),
            cc::CXX_FAST_TLS => f.write_str("cxx_fast_tlscc"),
            cc::TAIL => f.write_str("tailcc"),
            cc::CFGUARD_CHECK => f.write_str("cfguard_checkcc"),
            cc::SWIFT_TAIL => f.write_str("swifttailcc"),
            n => write!(f, "cc {n}"),
        }
    }
}
