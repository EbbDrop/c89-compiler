use std::fmt::{self, Write};

#[derive(Debug, Clone)]
pub struct FmtOpts {
    pub display_unnamed_ids: bool,
}

impl Default for FmtOpts {
    fn default() -> Self {
        Self {
            display_unnamed_ids: true,
        }
    }
}

pub trait FmtAsLlvmAsm {
    fn fmt_as_llvm_asm(&self, f: &mut fmt::Formatter, opts: &FmtOpts) -> fmt::Result;
}

pub trait FmtAsLlvmAsmMC {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &FmtOpts,
        module: &crate::Module,
    ) -> fmt::Result;
}

pub trait FmtAsLlvmAsmFC {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &FmtOpts,
        module: &crate::Module,
        function: &crate::FunctionDeclaration,
    ) -> fmt::Result;
}

impl<T: FmtAsLlvmAsm> FmtAsLlvmAsmMC for T {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &FmtOpts,
        _: &crate::Module,
    ) -> fmt::Result {
        self.fmt_as_llvm_asm(f, opts)
    }
}

impl<T: FmtAsLlvmAsmMC> FmtAsLlvmAsmFC for T {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &FmtOpts,
        module: &crate::Module,
        _: &crate::FunctionDeclaration,
    ) -> fmt::Result {
        self.fmt_as_llvm_asm(f, opts, module)
    }
}

pub fn fmt_as_llvm_quoted_string(
    f: &mut fmt::Formatter,
    bytes: impl IntoIterator<Item = u8>,
) -> fmt::Result {
    f.write_char('"')?;
    for byte in bytes {
        fmt_char_as_llvm(f, byte)?;
    }
    f.write_char('"')
}

/// Displays the character (byte) to the formatter, escaping it if necessary.
///
/// Escapes all non-graphical ascii characters (excluding space), and the double quote (`"`) and
/// backslash (`\`) character.
pub fn fmt_char_as_llvm(f: &mut impl fmt::Write, byte: u8) -> fmt::Result {
    if byte == b' ' || (byte.is_ascii_graphic() && byte != b'"' && byte != b'\\') {
        f.write_char(byte.into())
    } else {
        fmt_as_llvm_escaped_char(f, byte)
    }
}

pub fn fmt_as_llvm_escaped_char(f: &mut impl fmt::Write, byte: u8) -> fmt::Result {
    write!(f, "\\{byte:02X}")
}
