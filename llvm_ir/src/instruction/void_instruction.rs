use super::{IntoValidated, ValidationError};
use crate::ty::{self, Type};
use crate::value::{self, Value};
use std::fmt::{self, Write};

impl IntoValidated<VoidInstruction> for VoidInstruction {
    type ValidatedType = VoidInstruction;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        Ok(self)
    }
}

declare_type_union_validated! {
    #[derive(Debug, Clone)]
    pub enum VoidInstruction {
        ValidatedStore(Store),
    }
}

#[derive(Debug, Clone)]
pub struct Store<V: value::ElementValue = value::Element, P: value::PointerValue = value::Pointer> {
    pub value: V,
    pub pointer: P,
}

impl<V, P> IntoValidated<VoidInstruction> for Store<V, P>
where
    V: value::ElementValue,
    P: value::PointerValue,
{
    type ValidatedType = ValidatedStore;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        let ty = self.value.ty();

        if ty.has_opaque_struct() {
            return Err(ValidationError::new("store", "size of value must be known"));
        }

        let value: value::Element = self.value.into();

        match value.ty() {
            // NOTE: Not sure if storing a scalable vector is allowed, since it crashes LLVM.
            // Loading a scalable vector (directly) isn't a problem though, so this might just be a
            // bug in LLVM.
            ty::Element::Single(_) => {}
            ty::Element::Aggregate(aggregate) => {
                if aggregate.has_scalable_vec() {
                    return Err(ValidationError::new("store", "size of value must be known"));
                }
            }
        }

        Ok(ValidatedStore(Store {
            value,
            pointer: self.pointer.into(),
        }))
    }
}

impl<V: value::ElementValue, P: value::PointerValue> crate::FmtAsLlvmAsmFC for Store<V, P> {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
        function: &crate::FunctionDeclaration,
    ) -> fmt::Result {
        f.write_str("store ")?;
        self.value.ty().fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_char(' ')?;
        self.value.fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_str(", ")?;
        self.pointer
            .ty()
            .fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_char(' ')?;
        self.pointer.fmt_as_llvm_asm(f, opts, module, function)
    }
}
