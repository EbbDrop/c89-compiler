use super::{IntoValidated, ValidationError};
use crate::ty::{self, Type};
use crate::value::{self, Value};
use crate::{AddressSpace, CallingConv};
use std::fmt::{self, Write};

pub trait MaybeYielding {
    type YieldTy: ty::FirstClassType;

    /// May panic if the instruction is not valid.
    fn is_yielding(&self) -> bool;

    /// May panic if the instruction is not valid.
    fn yield_ty(&self) -> Option<Self::YieldTy>;
}

impl IntoValidated<MaybeYieldingInstruction> for MaybeYieldingInstruction {
    type ValidatedType = MaybeYieldingInstruction;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        Ok(self)
    }
}

declare_type_union_validated! {
    @maybe_yielding(ty::Element);
    #[derive(Debug, Clone)]
    pub enum MaybeYieldingInstruction {
        ValidatedCall(Call),
    }
}

#[derive(Debug, Clone)]
pub struct Call {
    pub calling_conv: CallingConv,
    pub fn_ty: ty::Function,
    pub fn_pointer: value::Pointer,
    pub fn_args: Vec<value::FirstClass>,
}

impl MaybeYielding for Call {
    type YieldTy = ty::Element;

    fn is_yielding(&self) -> bool {
        !matches!(self.fn_ty.return_type(), crate::ReturnType::Void)
    }

    fn yield_ty(&self) -> Option<Self::YieldTy> {
        match self.fn_ty.return_type() {
            crate::ReturnType::Void => None,
            crate::ReturnType::Element(ty) => Some(ty.clone()),
        }
    }
}

impl IntoValidated<MaybeYieldingInstruction> for Call {
    type ValidatedType = ValidatedCall;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        let mut param_types = self.fn_ty.param_types().iter();
        let mut arg_values = self.fn_args.iter();
        loop {
            match (param_types.next(), arg_values.next()) {
                (Some(ty), Some(value)) => {
                    if !value.ty().equiv_to(ty) {
                        return Err(ValidationError::new(
                            "call",
                            "type of passed argument doesn't match function parameter type",
                        ));
                    }
                }
                (None, Some(_)) if self.fn_ty.is_vararg() => break,
                (None, Some(_)) => {
                    return Err(ValidationError::new(
                        "call",
                        "more arguments passed than expected by function type",
                    ))
                }
                (Some(_), None) => {
                    return Err(ValidationError::new(
                        "call",
                        "less arguments passed than expected by function type",
                    ))
                }
                (None, None) => break,
            }
        }
        Ok(ValidatedCall(self))
    }
}

impl crate::FmtAsLlvmAsmFC for Call {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
        function: &crate::FunctionDeclaration,
    ) -> fmt::Result {
        f.write_str("call ")?;

        if self.calling_conv != CallingConv::default() {
            self.calling_conv
                .fmt_as_llvm_asm(f, opts, module, function)?;
            f.write_char(' ')?;
        }

        let address_space = self.fn_pointer.ty().address_space();
        if address_space != AddressSpace::default() {
            address_space.fmt_as_llvm_asm(f, opts, module, function)?;
            f.write_char(' ')?;
        }

        if self.fn_ty.is_vararg() {
            self.fn_ty.fmt_as_llvm_asm(f, opts, module, function)?;
        } else {
            self.fn_ty
                .return_type()
                .fmt_as_llvm_asm(f, opts, module, function)?;
        }
        f.write_char(' ')?;

        self.fn_pointer.fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_char('(')?;

        if let [head, tail @ ..] = self.fn_args.as_slice() {
            head.ty().fmt_as_llvm_asm(f, opts, module, function)?;
            f.write_char(' ')?;
            head.fmt_as_llvm_asm(f, opts, module, function)?;
            for arg in tail {
                f.write_str(", ")?;
                arg.ty().fmt_as_llvm_asm(f, opts, module, function)?;
                f.write_char(' ')?;
                arg.fmt_as_llvm_asm(f, opts, module, function)?;
            }
        }

        f.write_char(')')
    }
}
