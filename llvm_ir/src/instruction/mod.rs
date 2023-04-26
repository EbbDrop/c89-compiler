macro_rules! impl_conversions {
    ({ $($from_ty:ty),* $(,)? } => $between_ty:ty => $to_ty:ty) => {
        $(
            impl From<$from_ty> for $to_ty {
                fn from(value: $from_ty) -> Self {
                    <$between_ty>::from(value).into()
                }
            }
        )*
    };
}

macro_rules! declare_type_union_validated {
    (
        $(@into($transitive_into_target:ty);)?
        $(@yielding($yield_ty:ty);)?
        $(@maybe_yielding($maybe_yield_ty:ty);)?
        #[$meta:meta] $vis:vis enum $enum_name:ident {
            $( $variant:ident$(($raw_variant:ty))? ),* $(,)?
        }
    ) => {
        declare_type_union! { #[$meta] $vis enum $enum_name { $($variant),* } }

        when! { ($($transitive_into_target)?) {
            impl_conversions! { { $($variant),* } => $enum_name => $($transitive_into_target)? }
        }}

        when! { ($($yield_ty)?) {
            impl crate::instruction::Yielding for $enum_name {
                type YieldTy = $($yield_ty)?;

                fn yield_ty(&self) -> Self::YieldTy {
                    match self {
                        $(Self::$variant(instr) => instr.yield_ty().into(),)*
                    }
                }
            }
        }}

        impl crate::FmtAsLlvmAsmFC for $enum_name {
            fn fmt_as_llvm_asm(
                &self,
                f: &mut std::fmt::Formatter,
                opts: &crate::FmtOpts,
                module: &crate::Module,
                function: &crate::FunctionDeclaration,
            ) -> std::fmt::Result {
                match self {
                    $(Self::$variant(v) => v.fmt_as_llvm_asm(f, opts, module, function),)*
                }
            }
        }

        $($(
            #[$meta] pub struct $variant($raw_variant);

            impl crate::FmtAsLlvmAsmFC for $variant {
                fn fmt_as_llvm_asm(
                    &self,
                    f: &mut std::fmt::Formatter,
                    opts: &crate::FmtOpts,
                    module: &crate::Module,
                    function: &crate::FunctionDeclaration,
                ) -> std::fmt::Result {
                    self.0.fmt_as_llvm_asm(f, opts, module, function)
                }
            }
        )?)*

        when! { ($($yield_ty)?) {
            $($(
                impl crate::instruction::Yielding for $variant {
                    type YieldTy = <$raw_variant as crate::instruction::Yielding>::YieldTy;
                    fn yield_ty(&self) -> Self::YieldTy { self.0.yield_ty() }
                }
            )?)*
        }}

        when! { ($($maybe_yield_ty)?) {
            $($(
                impl crate::instruction::MaybeYielding for $variant {
                    type YieldTy = <$raw_variant as crate::instruction::MaybeYielding>::YieldTy;
                    fn is_yielding(&self) -> bool { self.0.is_yielding() }
                    fn yield_ty(&self) -> Option<Self::YieldTy> { self.0.yield_ty() }
                }
            )?)*
        }}
    };
}

mod maybe_yielding_instruction;
mod terminator;
mod void_instruction;
mod yielding_instruction;

pub use maybe_yielding_instruction::*;
pub use terminator::*;
pub use void_instruction::*;
pub use yielding_instruction::*;

use crate::{convert::TryFrom_, ty, value};
use std::fmt;

#[derive(Debug)]
pub struct ValidationError {
    instruction_name: String,
    reason: String,
}

impl ValidationError {
    fn new(instruction_name: impl Into<String>, reason: impl Into<String>) -> Self {
        Self {
            instruction_name: instruction_name.into(),
            reason: reason.into(),
        }
    }
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "invalid {} instruction: {}",
            self.instruction_name, self.reason
        )
    }
}

impl std::error::Error for ValidationError {}

impl From<ValidationError> for String {
    fn from(value: ValidationError) -> Self {
        value.to_string()
    }
}

pub trait IntoValidated<T> {
    type ValidatedType: Into<T>;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError>;
}

impl<U: IntoValidated<YieldingInstruction>> TryFrom_<U> for YieldingInstruction {
    type Error = ValidationError;

    fn try_from_(value: U) -> Result<Self, Self::Error> {
        value.into_validated().map(Into::into)
    }
}

impl<U: IntoValidated<MaybeYieldingInstruction>> TryFrom_<U> for MaybeYieldingInstruction {
    type Error = ValidationError;

    fn try_from_(value: U) -> Result<Self, Self::Error> {
        value.into_validated().map(Into::into)
    }
}

impl<U: IntoValidated<VoidInstruction>> TryFrom_<U> for VoidInstruction {
    type Error = ValidationError;

    fn try_from_(value: U) -> Result<Self, Self::Error> {
        value.into_validated().map(Into::into)
    }
}

impl<U: IntoValidated<Terminator>> TryFrom_<U> for Terminator {
    type Error = ValidationError;

    fn try_from_(value: U) -> Result<Self, Self::Error> {
        value.into_validated().map(Into::into)
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Yielding(value::Register<ty::FirstClass>, YieldingInstruction),
    MaybeYielding(
        Option<value::Register<ty::FirstClass>>,
        MaybeYieldingInstruction,
    ),
    Void(VoidInstruction),
}

impl crate::FmtAsLlvmAsmFC for Instruction {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
        function: &crate::FunctionDeclaration,
    ) -> fmt::Result {
        match self {
            Instruction::Yielding(result, instruction) => {
                if opts.display_unnamed_ids || result.is_named(function) {
                    result.fmt_as_llvm_asm(f, opts, module, function)?;
                    f.write_str(" = ")?;
                }
                instruction.fmt_as_llvm_asm(f, opts, module, function)
            }
            Instruction::MaybeYielding(Some(result), instruction) => {
                if opts.display_unnamed_ids || result.is_named(function) {
                    result.fmt_as_llvm_asm(f, opts, module, function)?;
                    f.write_str(" = ")?;
                }
                instruction.fmt_as_llvm_asm(f, opts, module, function)
            }
            Instruction::MaybeYielding(None, instruction) => {
                instruction.fmt_as_llvm_asm(f, opts, module, function)
            }
            Instruction::Void(instruction) => {
                instruction.fmt_as_llvm_asm(f, opts, module, function)
            }
        }
    }
}
