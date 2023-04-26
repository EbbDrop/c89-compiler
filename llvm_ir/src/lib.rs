#[macro_use]
mod macros;

mod attr;
mod fmt_as_llvm_asm;
mod function;
mod global_var;
mod id;
mod id_store;
mod module;

pub mod instruction;
pub mod ty;
pub mod value;

pub use attr::*;
pub use fmt_as_llvm_asm::FmtOpts;
pub use function::*;
pub use global_var::*;
pub use module::*;
pub use value::constant;

use fmt_as_llvm_asm::*;
use std::fmt;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Name(String);

impl std::fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            f.debug_tuple("Name").field(&self.0).finish()
        } else {
            self.0.fmt(f)
        }
    }
}

impl<T: Into<String>> From<T> for Name {
    fn from(value: T) -> Self {
        // Names are stored as-is, without double-quotes or espaced characters. These things are
        // performed in the fmt_as_llvm_asm function.
        Self(value.into())
    }
}

impl crate::FmtAsLlvmAsm for Name {
    fn fmt_as_llvm_asm(&self, f: &mut fmt::Formatter, _opts: &FmtOpts) -> fmt::Result {
        if !self.0.is_empty()
            && !self.0.as_bytes()[0].is_ascii_digit()
            && self
                .0
                .bytes()
                .all(|b| b.is_ascii_alphanumeric() || b == b'.')
        {
            f.write_str(&self.0)
        } else {
            crate::fmt_as_llvm_quoted_string(f, self.0.bytes())
        }
    }
}

pub mod convert {
    pub trait From_<T>
    where
        Self: Sized,
    {
        fn from_(value: T) -> Self;
    }

    pub trait Into_<T>
    where
        Self: Sized,
    {
        fn into_(self) -> T;
    }

    impl<T, U: From_<T>> Into_<U> for T {
        fn into_(self) -> U {
            U::from_(self)
        }
    }

    pub trait TryFrom_<T>
    where
        Self: Sized,
    {
        type Error;

        fn try_from_(value: T) -> Result<Self, Self::Error>;
    }

    pub trait TryInto_<T>
    where
        Self: Sized,
    {
        type Error;

        fn try_into_(self) -> Result<T, Self::Error>;
    }

    impl<T, U: TryFrom_<T>> TryInto_<U> for T {
        type Error = U::Error;

        fn try_into_(self) -> Result<U, Self::Error> {
            U::try_from_(self)
        }
    }
}
