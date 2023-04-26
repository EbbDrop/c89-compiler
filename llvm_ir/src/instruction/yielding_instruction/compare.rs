use super::{IntoValidated, ValidationError, Yielding, YieldingInstruction};
use crate::ty::{self, Type};
use crate::value::{self, Value};
use std::fmt::{self, Write};

declare_type_union_validated! {
    @into(YieldingInstruction);
    @yielding(ty::Single);
    #[derive(Debug, Clone)]
    pub enum ValidatedCompareOp {
        ValidatedInt(Int),
        ValidatedIntVec(IntVec),
        ValidatedPtr(Ptr),
        ValidatedPtrVec(PtrVec),
        ValidatedFp(Fp),
        ValidatedFpVec(FpVec),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IcmpCond {
    /// Equal.
    ///
    /// Yields `true` if the operands are equal, `false` otherwise.
    /// No sign interpretation is necessary or performed.
    Eq,
    /// Not equal.
    ///
    /// Yields `true` if the operands are unequal, `false` otherwise.
    /// No sign interpretation is necessary or performed.
    Ne,
    /// Unsigned greater than.
    ///
    /// Interprets the operands as unsigned values and yields `true` if `op1` is greater than `op2`.
    Ugt,
    /// Unsigned greater or equal.
    ///
    /// Interprets the operands as unsigned values and yields `true` if `op1` is greater than or
    /// equal to `op2`.
    Uge,
    /// Unsigned less than.
    ///
    /// Interprets the operands as unsigned values and yields `true` if `op1` is less than `op2`.
    Ult,
    /// Unsigned less or equal.
    ///
    /// Interprets the operands as unsigned values and yields `true` if `op1` is less than or equal
    /// to `op2`.
    Ule,
    /// Signed greater than.
    ///
    /// Interprets the operands as signed values and yields `true` if `op1` is greater than `op2`.
    Sgt,
    /// Signed greater or equal.
    ///
    /// Interprets the operands as signed values and yields `true` if `op1` is greater than or
    /// equal to `op2`.
    Sge,
    /// Signed less than.
    ///
    /// Interprets the operands as signed values and yields `true` if `op1` is less than `op2`.
    Slt,
    /// Signed less or equal.
    ///
    /// Interprets the operands as signed values and yields `true` if `op1` is less than or equal to
    /// `op2`.
    Sle,
}

impl fmt::Display for IcmpCond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Eq => "eq",
            Self::Ne => "ne",
            Self::Ugt => "ugt",
            Self::Uge => "uge",
            Self::Ult => "ult",
            Self::Ule => "ule",
            Self::Sgt => "sgt",
            Self::Sge => "sge",
            Self::Slt => "slt",
            Self::Sle => "sle",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FcmpCond {
    /// No comparison, always returns `false`.
    False,
    /// Ordered and equal.
    Oeq,
    /// Ordered and greater than.
    Ogt,
    /// Ordered and greater than or equal.
    Oge,
    /// Ordered and less than.
    Olt,
    /// Ordered and less than or equal.
    Ole,
    /// Ordered and not equal.
    One,
    /// Ordered (no `NaN`s).
    Ord,
    /// Unordered or equal.
    Ueq,
    /// Unordered or greater than.
    Ugt,
    /// Unordered or greater than or equal.
    Uge,
    /// Unordered or less than.
    Ult,
    /// Unordered or less than or equal.
    Ule,
    /// Unordered or not equal.
    Une,
    /// Unordered (either `NaN`s).
    Uno,
    /// No comparison, always returns `true`.
    True,
}

impl fmt::Display for FcmpCond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::False => "false",
            Self::Oeq => "oeq",
            Self::Ogt => "ogt",
            Self::Oge => "oge",
            Self::Olt => "olt",
            Self::Ole => "ole",
            Self::One => "one",
            Self::Ord => "ord",
            Self::Ueq => "ueq",
            Self::Ugt => "ugt",
            Self::Uge => "uge",
            Self::Ult => "ult",
            Self::Ule => "ule",
            Self::Une => "une",
            Self::Uno => "uno",
            Self::True => "true",
        })
    }
}

macro_rules! fmt_compare_op {
    ($cmp:literal $operation:ident<$gen:ident>) => {
        impl<V: value::$gen> crate::FmtAsLlvmAsmFC for $operation<V> {
            fn fmt_as_llvm_asm(
                &self,
                f: &mut fmt::Formatter,
                opts: &crate::FmtOpts,
                module: &crate::Module,
                function: &crate::FunctionDeclaration,
            ) -> fmt::Result {
                write!(f, concat!($cmp, " {} "), self.operator)?;
                self.operand1
                    .ty()
                    .fmt_as_llvm_asm(f, opts, module, function)?;
                f.write_char(' ')?;
                self.operand1.fmt_as_llvm_asm(f, opts, module, function)?;
                f.write_str(", ")?;
                self.operand2.fmt_as_llvm_asm(f, opts, module, function)
            }
        }
    };
}

#[derive(Debug, Clone)]
pub struct Int<V: value::IntegerValue = value::Integer> {
    pub operator: IcmpCond,
    pub operand1: V,
    pub operand2: V,
}

fmt_compare_op! { "icmp" Int<IntegerValue> }

impl<V: value::IntegerValue> Yielding for Int<V> {
    type YieldTy = ty::Int<1>;

    fn yield_ty(&self) -> Self::YieldTy {
        ty::Int::<1>::Literal
    }
}

impl<V: value::IntegerValue> IntoValidated<YieldingInstruction> for Int<V> {
    type ValidatedType = ValidatedInt;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        if self.operand1.ty().equiv_to(&self.operand2.ty()) {
            Ok(ValidatedInt(Int {
                operator: self.operator,
                operand1: self.operand1.into(),
                operand2: self.operand2.into(),
            }))
        } else {
            Err(ValidationError::new(
                format!("icmp {}", self.operator),
                "cannot compare integer values with different types",
            ))
        }
    }
}

#[derive(Debug, Clone)]
pub struct IntVec<V: value::VectorValue = value::Vector> {
    pub operator: IcmpCond,
    pub operand1: V,
    pub operand2: V,
}

fmt_compare_op! { "icmp" IntVec<VectorValue> }

impl<V: value::VectorValue> Yielding for IntVec<V> {
    type YieldTy = ty::Int<1>;

    fn yield_ty(&self) -> Self::YieldTy {
        ty::Int::<1>::Literal
    }
}

impl<V: value::VectorValue> IntoValidated<YieldingInstruction> for IntVec<V> {
    type ValidatedType = ValidatedIntVec;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        let vec1: value::Vector = self.operand1.into();
        let vec2: value::Vector = self.operand2.into();
        let (ty1, ty2) = (vec1.ty(), vec2.ty());
        if ty1.equiv_to(&ty2) && matches!(ty1.element_type(), ty::Primitive::Integer(_)) {
            Ok(ValidatedIntVec(IntVec {
                operator: self.operator,
                operand1: vec1,
                operand2: vec2,
            }))
        } else {
            Err(ValidationError::new(
                format!("icmp {}", self.operator),
                "invalid vector element type",
            ))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Ptr<V: value::PointerValue = value::Pointer> {
    pub operator: IcmpCond,
    pub operand1: V,
    pub operand2: V,
}

fmt_compare_op! { "icmp" Ptr<PointerValue> }

impl<V: value::PointerValue> Yielding for Ptr<V> {
    type YieldTy = ty::Int<1>;

    fn yield_ty(&self) -> Self::YieldTy {
        ty::Int::<1>::Literal
    }
}

impl<V: value::PointerValue> IntoValidated<YieldingInstruction> for Ptr<V> {
    type ValidatedType = ValidatedPtr;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        if self.operand1.ty().equiv_to(&self.operand2.ty()) {
            Ok(ValidatedPtr(Ptr {
                operator: self.operator,
                operand1: self.operand1.into(),
                operand2: self.operand2.into(),
            }))
        } else {
            Err(ValidationError::new(
                format!("icmp {}", self.operator),
                "cannot compare pointer values with different types",
            ))
        }
    }
}

#[derive(Debug, Clone)]
pub struct PtrVec<V: value::VectorValue = value::Vector> {
    pub operator: IcmpCond,
    pub operand1: V,
    pub operand2: V,
}

fmt_compare_op! { "icmp" PtrVec<VectorValue> }

impl<V: value::VectorValue> Yielding for PtrVec<V> {
    type YieldTy = ty::Int<1>;

    fn yield_ty(&self) -> Self::YieldTy {
        ty::Int::<1>::Literal
    }
}

impl<V: value::VectorValue> IntoValidated<YieldingInstruction> for PtrVec<V> {
    type ValidatedType = ValidatedPtrVec;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        let vec1: value::Vector = self.operand1.into();
        let vec2: value::Vector = self.operand2.into();
        let (ty1, ty2) = (vec1.ty(), vec2.ty());
        if ty1.equiv_to(&ty2) && matches!(ty1.element_type(), ty::Primitive::Pointer(_)) {
            Ok(ValidatedPtrVec(PtrVec {
                operator: self.operator,
                operand1: vec1,
                operand2: vec2,
            }))
        } else {
            Err(ValidationError::new(
                format!("icmp {}", self.operator),
                "invalid vector element type",
            ))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Fp<V: value::FloatingPointValue = value::FloatingPoint> {
    pub operator: FcmpCond,
    pub operand1: V,
    pub operand2: V,
}

fmt_compare_op! { "fcmp" Fp<FloatingPointValue> }

impl<V: value::FloatingPointValue> IntoValidated<YieldingInstruction> for Fp<V> {
    type ValidatedType = ValidatedFp;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        if self.operand1.ty().equiv_to(&self.operand2.ty()) {
            Ok(ValidatedFp(Fp {
                operator: self.operator,
                operand1: self.operand1.into(),
                operand2: self.operand2.into(),
            }))
        } else {
            Err(ValidationError::new(
                format!("fcmp {}", self.operator),
                "cannot compare floating-point values with different types",
            ))
        }
    }
}

impl<V: value::FloatingPointValue> Yielding for Fp<V> {
    type YieldTy = ty::Int<1>;

    fn yield_ty(&self) -> Self::YieldTy {
        ty::Int::<1>::Literal
    }
}

#[derive(Debug, Clone)]
pub struct FpVec<V: value::VectorValue = value::Vector> {
    pub operator: FcmpCond,
    pub operand1: V,
    pub operand2: V,
}

fmt_compare_op! { "fcmp" FpVec<VectorValue> }

impl<V: value::VectorValue> Yielding for FpVec<V> {
    type YieldTy = ty::Int<1>;

    fn yield_ty(&self) -> Self::YieldTy {
        ty::Int::<1>::Literal
    }
}

impl<V: value::VectorValue> IntoValidated<YieldingInstruction> for FpVec<V> {
    type ValidatedType = ValidatedFpVec;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        let vec1: value::Vector = self.operand1.into();
        let vec2: value::Vector = self.operand2.into();
        let (ty1, ty2) = (vec1.ty(), vec2.ty());
        if ty1.equiv_to(&ty2) && matches!(ty1.element_type(), ty::Primitive::FloatingPoint(_)) {
            Ok(ValidatedFpVec(FpVec {
                operator: self.operator,
                operand1: vec1,
                operand2: vec2,
            }))
        } else {
            Err(ValidationError::new(
                format!("fcmp {}", self.operator),
                "invalid vector element type",
            ))
        }
    }
}
