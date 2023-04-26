use super::{IntoValidated, ValidationError, Yielding, YieldingInstruction};
use crate::value::{self, Value};
use crate::{ty, ty::Type};
use std::fmt::{self, Write};

declare_type_union_validated! {
    @into(YieldingInstruction);
    @yielding(ty::Single);
    #[derive(Debug, Clone)]
    pub enum ValidatedBinaryOp {
        ValidatedInt(Int),
        ValidatedIntVec(IntVec),
        ValidatedFp(Fp),
        ValidatedFpVec(FpVec),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryIntOp {
    Add,
    Sub,
    Mul,
    Udiv,
    Sdiv,
    Urem,
    Srem,
    And,
    Or,
    Xor,
    Shl,
    Lshr,
    Ashr,
}

impl fmt::Display for BinaryIntOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Add => "add",
            Self::Sub => "sub",
            Self::Mul => "mul",
            Self::Udiv => "udiv",
            Self::Sdiv => "sdiv",
            Self::Urem => "urem",
            Self::Srem => "srem",
            Self::And => "and",
            Self::Or => "or",
            Self::Xor => "xor",
            Self::Shl => "shl",
            Self::Lshr => "lshr",
            Self::Ashr => "ashr",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryFpOp {
    Fadd,
    Fsub,
    Fmul,
    Fdiv,
    Frem,
}

impl fmt::Display for BinaryFpOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Fadd => "fadd",
            Self::Fsub => "fsub",
            Self::Fmul => "fmul",
            Self::Fdiv => "fdiv",
            Self::Frem => "frem",
        })
    }
}

macro_rules! declare_binary_op {
    ($operation:ident<$gen:ident = $gen_def:ident>($operator:ident)) => {
        #[derive(Debug, Clone)]
        pub struct $operation<V: value::$gen = value::$gen_def> {
            pub operator: $operator,
            pub operand1: V,
            pub operand2: V,
        }

        impl<V: value::$gen> Yielding for $operation<V> {
            type YieldTy = V::Type;

            fn yield_ty(&self) -> Self::YieldTy {
                self.operand1.ty()
            }
        }

        impl<V: value::$gen> crate::FmtAsLlvmAsmFC for $operation<V> {
            fn fmt_as_llvm_asm(
                &self,
                f: &mut fmt::Formatter,
                opts: &crate::FmtOpts,
                module: &crate::Module,
                function: &crate::FunctionDeclaration,
            ) -> fmt::Result {
                write!(f, "{} ", self.operator)?;
                self.yield_ty().fmt_as_llvm_asm(f, opts, module, function)?;
                f.write_char(' ')?;
                self.operand1.fmt_as_llvm_asm(f, opts, module, function)?;
                f.write_str(", ")?;
                self.operand2.fmt_as_llvm_asm(f, opts, module, function)
            }
        }
    };
}

declare_binary_op! { Int<IntegerValue = Integer>(BinaryIntOp) }

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
                format!("{}", self.operator),
                "cannot perform binary op on different types",
            ))
        }
    }
}

declare_binary_op! { IntVec<VectorValue = Vector>(BinaryIntOp) }

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
                format!("{}", self.operator),
                "invalid vector element type",
            ))
        }
    }
}

declare_binary_op! { Fp<FloatingPointValue = FloatingPoint>(BinaryFpOp) }

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
                format!("{}", self.operator),
                "cannot perform binary op on different types",
            ))
        }
    }
}

declare_binary_op! { FpVec<VectorValue = Vector>(BinaryFpOp) }

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
                format!("{}", self.operator),
                "invalid vector element type",
            ))
        }
    }
}
