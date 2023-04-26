pub mod binary_op;
pub mod cast;
pub mod compare;
pub mod unary_op;

pub use binary_op::{BinaryFpOp, BinaryIntOp, ValidatedBinaryOp};
pub use cast::ValidatedCastOp;
pub use compare::{FcmpCond, IcmpCond, ValidatedCompareOp};
pub use unary_op::{UnaryFpOp, ValidatedUnaryOp};

use super::{IntoValidated, ValidationError};
use crate::ty::{self, Type};
use crate::value::{self, Value};
use std::fmt::{self, Write};

pub trait Yielding {
    type YieldTy: ty::FirstClassType;

    /// May panic if the instruction is not valid.
    fn yield_ty(&self) -> Self::YieldTy;
}

impl IntoValidated<YieldingInstruction> for YieldingInstruction {
    type ValidatedType = YieldingInstruction;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        Ok(self)
    }
}

declare_type_union_validated! {
    @yielding(ty::FirstClass);
    #[derive(Debug, Clone)]
    pub enum YieldingInstruction {
        ValidatedUnaryOp,
        ValidatedBinaryOp,
        ValidatedCastOp,
        ValidatedCompareOp,
        ValidatedExtractValue(ExtractValue<value::Aggregate>),
        ValidatedGetElementPtr(GetElementPtr<ty::Element>),
        ValidatedPhi(Phi<value::FirstClass>),
        ValidatedAlloca(Alloca<ty::Element>),
        ValidatedLoad(Load<ty::Element>),
    }
}

#[derive(Debug, Clone)]
pub struct ExtractValue<V: value::AggregateValue> {
    pub value: V,
    pub index: value::constant::Integer,
    pub indices: Vec<value::constant::Integer>,
}

impl<V: value::AggregateValue> ExtractValue<V> {
    fn indexed_type(&self) -> Result<ty::Element, ()> {
        // NOTE: Extractvalue can index into aggregates that contain opaque structures or scalable
        // vectors, as long as those are not indexed.
        fn rec<'a>(
            ty: &ty::Element,
            mut remaining_indices: impl Iterator<Item = &'a value::constant::Integer>,
        ) -> Result<ty::Element, ()> {
            let Some(index) = remaining_indices.next() else { return Ok(ty.clone()) };
            match ty {
                // NOTE: extractvalue cannot index into vectors, while GEP can
                ty::Element::Single(_) => Err(()),
                ty::Element::Aggregate(ty::Aggregate::Structure(structure)) => {
                    match (index.value(), structure.body_types()) {
                        (Some(n), Some(body_types))
                            if n >= 0 && (n as usize) < body_types.len() =>
                        {
                            rec(&body_types[n as usize], remaining_indices)
                        }
                        _ => Err(()),
                    }
                }
                ty::Element::Aggregate(ty::Aggregate::Array(array)) => match index.value() {
                    Some(n) if n >= 0 && (n as usize) < array.size() => {
                        rec(array.element_type(), remaining_indices)
                    }
                    _ => Err(()),
                },
            }
        }
        let value: value::Aggregate = self.value.clone().into();
        rec(
            &value.ty().into(),
            std::iter::once(&self.index).chain(&self.indices),
        )
    }
}

impl<V: value::AggregateValue> Yielding for ExtractValue<V> {
    type YieldTy = ty::Element;

    fn yield_ty(&self) -> Self::YieldTy {
        self.indexed_type().unwrap()
    }
}

impl<V: value::AggregateValue> IntoValidated<YieldingInstruction> for ExtractValue<V> {
    type ValidatedType = ValidatedExtractValue;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        if self.indexed_type().is_err() {
            return Err(ValidationError::new(
                "extractvalue",
                "invalid indices for this type",
            ));
        }
        Ok(ValidatedExtractValue(ExtractValue {
            value: self.value.into(),
            index: self.index,
            indices: self.indices,
        }))
    }
}

impl<V: value::AggregateValue> crate::FmtAsLlvmAsmFC for ExtractValue<V> {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
        function: &crate::FunctionDeclaration,
    ) -> fmt::Result {
        f.write_str("extractvalue ")?;
        self.value.ty().fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_char(' ')?;
        self.value.fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_str(", ")?;
        self.index.fmt_as_llvm_asm(f, opts, module, function)?;
        for index in &self.indices {
            f.write_str(", ")?;
            index.fmt_as_llvm_asm(f, opts, module, function)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct GetElementPtr<T: ty::ElementType> {
    pub ty: T,
    pub pointer: value::Pointer,
    pub indices: Vec<value::Integer>,
}

impl<T: ty::ElementType> Yielding for GetElementPtr<T> {
    type YieldTy = ty::Pointer;

    fn yield_ty(&self) -> Self::YieldTy {
        self.pointer.ty()
    }
}

impl<T: ty::ElementType> IntoValidated<YieldingInstruction> for GetElementPtr<T> {
    type ValidatedType = ValidatedGetElementPtr;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        if self.ty.has_opaque_struct() || self.ty.has_scalable_vec() {
            return Err(ValidationError::new("getelementptr", "type must be sized"));
        }
        fn rec<'a>(
            ty: &ty::Element,
            mut remaining_indices: impl Iterator<Item = &'a value::Integer>,
        ) -> bool {
            let Some(index) = remaining_indices.next() else { return true };
            match ty {
                ty::Element::Single(ty::Single::Primitive(_)) => false,
                // NOTE: GEP can index into vectors (while extractvalue cannot)
                // TODO: FIXME: this is probably not an index into the vector, but rather a vector
                //  of indices into the element type of the vector.
                ty::Element::Single(ty::Single::Vector(vector)) => {
                    rec(&vector.element_type().clone().into(), remaining_indices)
                }
                ty::Element::Aggregate(ty::Aggregate::Structure(structure)) => match index {
                    value::Integer::Constant(constant) => {
                        match (constant.value(), structure.body_types()) {
                            (Some(n), Some(body_types))
                                if n >= 0 && (n as usize) < body_types.len() =>
                            {
                                rec(&body_types[n as usize], remaining_indices)
                            }
                            _ => false,
                        }
                    }
                    value::Integer::Register(_) => false,
                },
                ty::Element::Aggregate(ty::Aggregate::Array(array)) => {
                    rec(&array.element_type().clone(), remaining_indices)
                }
            }
        }
        let ty = self.ty.into();
        rec(&ty, self.indices.iter().skip(1))
            .then_some(ValidatedGetElementPtr(GetElementPtr {
                ty,
                pointer: self.pointer,
                indices: self.indices,
            }))
            .ok_or(ValidationError::new(
                "getelementptr",
                "invalid indices for type",
            ))
    }
}

impl<T: ty::ElementType> crate::FmtAsLlvmAsmFC for GetElementPtr<T> {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
        function: &crate::FunctionDeclaration,
    ) -> fmt::Result {
        f.write_str("getelementptr ")?;
        self.ty.fmt_as_llvm_asm(f, opts, module)?;
        f.write_str(", ")?;
        self.pointer
            .ty()
            .fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_char(' ')?;
        self.pointer.fmt_as_llvm_asm(f, opts, module, function)?;
        for index in &self.indices {
            f.write_str(", ")?;
            index.ty().fmt_as_llvm_asm(f, opts, module, function)?;
            f.write_char(' ')?;
            index.fmt_as_llvm_asm(f, opts, module, function)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Phi<V: value::FirstClassValue> {
    pub head: (V, value::constant::Label),
    pub tail: Vec<(V, value::constant::Label)>,
}

impl<T: value::FirstClassValue> Yielding for Phi<T> {
    type YieldTy = T::Type;

    fn yield_ty(&self) -> Self::YieldTy {
        self.head.0.ty()
    }
}

impl<V: value::FirstClassValue> IntoValidated<YieldingInstruction> for Phi<V> {
    type ValidatedType = ValidatedPhi;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        let ty = self.head.0.ty();
        if self.tail.iter().all(|v| v.0.ty().equiv_to(&ty)) {
            Ok(ValidatedPhi(Phi {
                head: (self.head.0.into(), self.head.1),
                tail: self
                    .tail
                    .into_iter()
                    .map(|(t, label)| (t.into(), label))
                    .collect(),
            }))
        } else {
            Err(ValidationError::new(
                "phi",
                "types of arguments do not match",
            ))
        }
    }
}

impl<V: value::FirstClassValue> crate::FmtAsLlvmAsmFC for Phi<V> {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
        function: &crate::FunctionDeclaration,
    ) -> fmt::Result {
        f.write_str("phi ")?;
        self.yield_ty().fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_str(" [ ")?;
        self.head.0.fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_str(", ")?;
        self.head.1.fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_str(" ]")?;
        for (value, label) in &self.tail {
            f.write_str(", [ ")?;
            value.fmt_as_llvm_asm(f, opts, module, function)?;
            f.write_str(", ")?;
            label.fmt_as_llvm_asm(f, opts, module, function)?;
            f.write_str(" ]")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Alloca<T: ty::ElementType> {
    pub ty: T,
    pub amount: Option<value::Integer>,
}

impl<T: ty::ElementType> Yielding for Alloca<T> {
    type YieldTy = ty::Pointer;

    fn yield_ty(&self) -> Self::YieldTy {
        ty::Pointer::default()
    }
}

impl<T: ty::ElementType> IntoValidated<YieldingInstruction> for Alloca<T> {
    type ValidatedType = ValidatedAlloca;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        if self.ty.has_opaque_struct() {
            Err(ValidationError::new("alloca", "size of type must be known"))
        } else {
            Ok(ValidatedAlloca(Alloca {
                ty: self.ty.into(),
                amount: self.amount,
            }))
        }
    }
}

impl<T: ty::ElementType> crate::FmtAsLlvmAsmFC for Alloca<T> {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
        function: &crate::FunctionDeclaration,
    ) -> fmt::Result {
        f.write_str("alloca ")?;
        self.ty.fmt_as_llvm_asm(f, opts, module)?;
        if let Some(amount) = &self.amount {
            f.write_str(", ")?;
            amount.ty().fmt_as_llvm_asm(f, opts, module, function)?;
            f.write_char(' ')?;
            amount.fmt_as_llvm_asm(f, opts, module, function)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Load<T: ty::ElementType> {
    pub ty: T,
    pub pointer: value::Pointer,
}

impl<T: ty::ElementType> Yielding for Load<T> {
    type YieldTy = T;

    fn yield_ty(&self) -> Self::YieldTy {
        self.ty.clone()
    }
}

impl<T: ty::ElementType> IntoValidated<YieldingInstruction> for Load<T> {
    type ValidatedType = ValidatedLoad;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        if self.ty.has_opaque_struct() {
            return Err(ValidationError::new("load", "size of type must be known"));
        }

        let ty = self.ty.into();

        match &ty {
            ty::Element::Single(_) => {}
            ty::Element::Aggregate(aggregate) => {
                if aggregate.has_scalable_vec() {
                    return Err(ValidationError::new("load", "size of type must be known"));
                }
            }
        }

        Ok(ValidatedLoad(Load {
            ty,
            pointer: self.pointer,
        }))
    }
}

impl<T: ty::ElementType> crate::FmtAsLlvmAsmFC for Load<T> {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
        function: &crate::FunctionDeclaration,
    ) -> fmt::Result {
        f.write_str("load ")?;
        self.ty.fmt_as_llvm_asm(f, opts, module)?;
        f.write_str(", ")?;
        self.pointer
            .ty()
            .fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_char(' ')?;
        self.pointer.fmt_as_llvm_asm(f, opts, module, function)
    }
}
