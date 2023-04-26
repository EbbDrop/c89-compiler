use fmt::Write;

use super::{IntoValidated, ValidationError, Yielding, YieldingInstruction};
use crate::ty;
use crate::value::{self, Value};
use std::fmt;

declare_type_union_validated! {
    @into(YieldingInstruction);
    @yielding(ty::Single);
    #[derive(Debug, Clone)]
    pub enum ValidatedUnaryOp {
        ValidatedFp(Fp),
        ValidatedFpVec(FpVec),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryFpOp {
    Fneg,
}

impl fmt::Display for UnaryFpOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Fneg => "fneg",
        })
    }
}

macro_rules! declare_unary_op {
    ($operation:ident<$gen:ident = $gen_def:ident>) => {
        #[derive(Debug, Clone)]
        pub struct $operation<V: value::$gen = value::$gen_def> {
            pub operator: UnaryFpOp,
            pub operand: V,
        }

        impl<V: value::$gen> Yielding for $operation<V> {
            type YieldTy = V::Type;

            fn yield_ty(&self) -> Self::YieldTy {
                self.operand.ty()
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
                self.operand.fmt_as_llvm_asm(f, opts, module, function)
            }
        }
    };
}

declare_unary_op! { Fp<FloatingPointValue = FloatingPoint> }

impl<V: value::FloatingPointValue> IntoValidated<YieldingInstruction> for Fp<V> {
    type ValidatedType = ValidatedFp;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        Ok(ValidatedFp(Fp {
            operator: self.operator,
            operand: self.operand.into(),
        }))
    }
}

declare_unary_op! { FpVec<VectorValue = Vector> }

impl<V: value::VectorValue> IntoValidated<YieldingInstruction> for FpVec<V> {
    type ValidatedType = ValidatedFpVec;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        let vector: value::Vector = self.operand.into();
        match vector.ty().element_type() {
            ty::Primitive::FloatingPoint(_) => Ok(ValidatedFpVec(FpVec {
                operator: self.operator,
                operand: vector,
            })),
            _ => Err(ValidationError::new(
                format!("{}", self.operator),
                "invalid vector element type",
            )),
        }
    }
}
