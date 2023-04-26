use crate::Name;
use std::fmt::{self, Display, Write};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Global(pub Id);

impl From<Id> for Global {
    fn from(value: Id) -> Self {
        Self(value)
    }
}

impl AsRef<Id> for Global {
    fn as_ref(&self) -> &Id {
        &self.0
    }
}

impl AsMut<Id> for Global {
    fn as_mut(&mut self) -> &mut Id {
        &mut self.0
    }
}

impl crate::FmtAsLlvmAsm for Global {
    fn fmt_as_llvm_asm(&self, f: &mut fmt::Formatter, opts: &crate::FmtOpts) -> fmt::Result {
        f.write_char('@')?;
        self.0.fmt_as_llvm_asm(f, opts)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Local(pub Id);

impl From<Id> for Local {
    fn from(value: Id) -> Self {
        Self(value)
    }
}

impl AsRef<Id> for Local {
    fn as_ref(&self) -> &Id {
        &self.0
    }
}

impl AsMut<Id> for Local {
    fn as_mut(&mut self) -> &mut Id {
        &mut self.0
    }
}

impl crate::FmtAsLlvmAsm for Local {
    fn fmt_as_llvm_asm(&self, f: &mut fmt::Formatter, opts: &crate::FmtOpts) -> fmt::Result {
        f.write_char('%')?;
        self.0.fmt_as_llvm_asm(f, opts)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Id {
    Named(Named),
    Unnamed(Unnamed),
}

impl Id {
    pub fn is_named(&self) -> bool {
        matches!(self, Self::Named(_))
    }
}

impl From<Named> for Id {
    fn from(value: Named) -> Self {
        Self::Named(value)
    }
}

impl From<Unnamed> for Id {
    fn from(value: Unnamed) -> Self {
        Self::Unnamed(value)
    }
}

impl crate::FmtAsLlvmAsm for Id {
    fn fmt_as_llvm_asm(&self, f: &mut fmt::Formatter, opts: &crate::FmtOpts) -> fmt::Result {
        match self {
            Id::Named(named) => named.fmt_as_llvm_asm(f, opts),
            Id::Unnamed(unnamed) => unnamed.fmt_as_llvm_asm(f, opts),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Named(pub(crate) std::rc::Rc<Name>);

impl crate::FmtAsLlvmAsm for Named {
    fn fmt_as_llvm_asm(&self, f: &mut fmt::Formatter, opts: &crate::FmtOpts) -> fmt::Result {
        self.0.fmt_as_llvm_asm(f, opts)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Unnamed(pub(crate) usize);

impl crate::FmtAsLlvmAsm for Unnamed {
    fn fmt_as_llvm_asm(&self, f: &mut fmt::Formatter, _opts: &crate::FmtOpts) -> fmt::Result {
        self.0.fmt(f)
    }
}
