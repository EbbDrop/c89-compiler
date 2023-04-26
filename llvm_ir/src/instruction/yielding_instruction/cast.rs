use super::{IntoValidated, ValidationError, Yielding, YieldingInstruction};
use crate::ty::{self, BitSize};
use crate::value::{self, Value};
use std::fmt::{self, Write};

declare_type_union_validated! {
    @into(YieldingInstruction);
    @yielding(ty::Single);
    #[derive(Debug, Clone)]
    pub enum ValidatedCastOp {
        ValidatedTruncInt(TruncInt),
        ValidatedTruncIntVec(TruncIntVec),
        ValidatedZextInt(ZextInt),
        ValidatedZextIntVec(ZextIntVec),
        ValidatedSextInt(SextInt),
        ValidatedSextIntVec(SextIntVec),
        ValidatedFpTrunc(FpTrunc),
        ValidatedFpVecTrunc(FpVecTrunc),
        ValidatedFpExt(FpExt),
        ValidatedFpVecExt(FpVecExt),
        ValidatedFpToUi(FpToUi),
        ValidatedFpVecToUiVec(FpVecToUiVec),
        ValidatedFpToSi(FpToSi),
        ValidatedFpVecToSiVec(FpVecToSiVec),
        ValidatedUiToFp(UiToFp),
        ValidatedUiVecToFpVec(UiVecToFpVec),
        ValidatedSiToFp(SiToFp),
        ValidatedSiVecToFpVec(SiVecToFpVec),
        ValidatedPtrToInt(PtrToInt),
        ValidatedPtrVecToIntVec(PtrVecToIntVec),
        ValidatedIntToPtr(IntToPtr),
        ValidatedIntVecToPtrVec(IntVecToPtrVec),
        ValidatedBitCast(BitCast),
    }
}

macro_rules! declare_cast_ops {
    ($($(
        #[$meta:meta])* $name:ident<
            $value:ident = $value_def:ident,
            $ty:ident = $ty_def:ident
        > => $opname:literal
    ),+ $(,)?) => {
        $(
            #[derive(Debug, Clone)]
            $(#[$meta])*
            pub struct $name<V: value::$value = value::$value_def, T: ty::$ty = ty::$ty_def> {
                pub value: V,
                pub to_ty: T,
            }

            impl<V: value::$value, T: ty::$ty> Yielding for $name<V, T> {
                type YieldTy = T;
                fn yield_ty(&self) -> Self::YieldTy {
                    self.to_ty.clone()
                }
            }

            impl<V: value::$value, T: ty::$ty> crate::FmtAsLlvmAsmFC for $name<V, T> {
                fn fmt_as_llvm_asm(
                    &self,
                    f: &mut fmt::Formatter,
                    opts: &crate::FmtOpts,
                    module: &crate::Module,
                    function: &crate::FunctionDeclaration,
                ) -> fmt::Result {
                    write!(f, concat!($opname, " "))?;
                    self.value.ty().fmt_as_llvm_asm(f, opts, module, function)?;
                    f.write_char(' ')?;
                    self.value.fmt_as_llvm_asm(f, opts, module, function)?;
                    f.write_str(" to ")?;
                    self.to_ty.fmt_as_llvm_asm(f, opts, module)
                }
            }
        )+
    };
}

declare_cast_ops! {
    TruncInt<IntegerValue = Integer, IntegerType = Integer> => "trunc",
    TruncIntVec<VectorValue = Vector, VectorType = Vector> => "trunc",
    ZextInt<IntegerValue = Integer, IntegerType = Integer> => "zext",
    ZextIntVec<VectorValue = Vector, VectorType = Vector> => "zext",
    SextInt<IntegerValue = Integer, IntegerType = Integer> => "sext",
    SextIntVec<VectorValue = Vector, VectorType = Vector> => "sext",
    FpTrunc<FloatingPointValue = FloatingPoint, FloatingPointType = FloatingPoint> => "fptrunc",
    FpVecTrunc<VectorValue = Vector, VectorType = Vector> => "fptrunc",
    FpExt<FloatingPointValue = FloatingPoint, FloatingPointType = FloatingPoint> => "fpext",
    FpVecExt<VectorValue = Vector, VectorType = Vector> => "fpext",
    FpToUi<FloatingPointValue = FloatingPoint, IntegerType = Integer> => "fptoui",
    FpVecToUiVec<VectorValue = Vector, VectorType = Vector> => "fptoui",
    FpToSi<FloatingPointValue = FloatingPoint, IntegerType = Integer> => "fptosi",
    FpVecToSiVec<VectorValue = Vector, VectorType = Vector> => "fptosi",
    UiToFp<IntegerValue = Integer, FloatingPointType = FloatingPoint> => "uitofp",
    UiVecToFpVec<VectorValue = Vector, VectorType = Vector> => "uitofp",
    SiToFp<IntegerValue = Integer, FloatingPointType = FloatingPoint> => "sitofp",
    SiVecToFpVec<VectorValue = Vector, VectorType = Vector> => "sitofp",
    PtrToInt<PointerValue = Pointer, IntegerType = Integer> => "ptrtoint",
    PtrVecToIntVec<VectorValue = Vector, VectorType = Vector> => "ptrtoint",
    IntToPtr<IntegerValue = Integer, PointerType = Pointer> => "inttoptr",
    IntVecToPtrVec<VectorValue = Vector, VectorType = Vector> => "inttoptr",
    /// Bitcast.
    ///
    /// Converts `value` to `to_ty` without changing any bits.
    BitCast<SingleValue = Single, SingleType = Single> => "bitcast",
}

macro_rules! impl_intovalidated_for_resize_primitive_casts {
    (
        $($ValidatedCastType:ident($CastType:ident<$Value:ident, $Type:ident>)
            => |$from:ident, $to:ident| $cmp:expr, $op_name:literal: $err_reason:expr;
        )+
    ) => {
        $(
            impl<V, T> IntoValidated<YieldingInstruction> for $CastType<V, T>
            where
                V: value::$Value,
                T: ty::$Type,
            {
                type ValidatedType = $ValidatedCastType;

                fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
                    let validated = $ValidatedCastType($CastType {
                        value: self.value.into(),
                        to_ty: self.to_ty.into(),
                    });
                    let $from = &validated.0.value.ty();
                    let $to = &validated.0.to_ty;
                    ($cmp).then_some(validated).ok_or(ValidationError::new($op_name, $err_reason))
                }
            }
        )+
    };
}

macro_rules! impl_intovalidated_for_resize_vector_casts {
    (
        $($ValidatedCastType:ident($CastType:ident($ElementType:ident))
            => |$from:ident, $to:ident| $cmp:expr, $op_name:literal;
        )+
    ) => {
        $(
            impl<V, T> IntoValidated<YieldingInstruction> for $CastType<V, T>
            where
                V: value::VectorValue,
                T: ty::VectorType,
            {
                type ValidatedType = $ValidatedCastType;

                fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
                    use ty::Primitive as P;
                    let value: value::Vector = self.value.into();
                    let to_ty: ty::Vector = self.to_ty.into();
                    let from_ty = value.ty();
                    if from_ty.size() != to_ty.size() {
                        return Err(ValidationError::new(
                            $op_name,
                            "source and target vector types have different sizes"
                        ));
                    }
                    match (from_ty.element_type(), to_ty.element_type()) {
                        (P::$ElementType($from), P::$ElementType($to)) if ($cmp) => {
                            Ok($ValidatedCastType($CastType { value, to_ty }))
                        }
                        _ => Err(ValidationError::new($op_name, "invalid vector element type")),
                    }
                }
            }
        )+
    };
}

macro_rules! impl_intovalidated_for_always_valid_casts {
    ($($ValidatedCastType:ident($CastType:ident<$Value:ident, $Type:ident>)),+ $(,)?) => {
        $(
            impl<V, T> IntoValidated<YieldingInstruction> for $CastType<V, T>
            where
                V: value::$Value,
                T: ty::$Type,
            {
                type ValidatedType = $ValidatedCastType;

                fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
                    Ok($ValidatedCastType($CastType {
                        value: self.value.into(),
                        to_ty: self.to_ty.into(),
                    }))
                }
            }
        )+
    };
}

macro_rules! impl_intovalidated_for_always_valid_vector_casts {
    ($(
        $ValidatedCastType:ident($CastType:ident($FromElem:ident -> $ToElem:ident)),
        $op_name:literal;
    )+) => {
        $(
            impl<V, T> IntoValidated<YieldingInstruction> for $CastType<V, T>
            where
                V: value::VectorValue,
                T: ty::VectorType,
            {
                type ValidatedType = $ValidatedCastType;

                fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
                    let value: value::Vector = self.value.into();
                    let to_ty: ty::Vector = self.to_ty.into();
                    let from_ty = value.ty();
                    if from_ty.size() == to_ty.size()
                        && matches!(from_ty.element_type(), ty::Primitive::$FromElem(_))
                        && matches!(to_ty.element_type(), ty::Primitive::$ToElem(_))
                    {
                        Ok($ValidatedCastType($CastType { value, to_ty }))
                    } else {
                        Err(ValidationError::new($op_name, "invalid vector element type"))
                    }
                }
            }
        )+
    };
}

impl_intovalidated_for_resize_primitive_casts! {
    ValidatedTruncInt(TruncInt<IntegerValue, IntegerType>) => |f, t| f.bit_size() > t.bit_size(),
        "trunc": "bit size of target type must be less than source type";
    ValidatedZextInt(ZextInt<IntegerValue, IntegerType>) => |f, t| f.bit_size() < t.bit_size(),
        "zext": "bit size of target type must be greater than source type";
    ValidatedSextInt(SextInt<IntegerValue, IntegerType>) => |f, t| f.bit_size() < t.bit_size(),
        "sext": "bit size of target type must be greater than source type";
    ValidatedFpTrunc(FpTrunc<FloatingPointValue, FloatingPointType>) => |f, t| {
        f.bit_size() > t.bit_size()
    },
        "fptrunc": "bit size of target type must be less than source type";
    ValidatedFpExt(FpExt<FloatingPointValue, FloatingPointType>) => |f, t| {
        f.bit_size() < t.bit_size()
    },
        "fpext": "bit size of target type must be greater than source type";
}

impl_intovalidated_for_resize_vector_casts! {
    ValidatedTruncIntVec(TruncIntVec(Integer)) => |f, t| f.bit_size() > t.bit_size(), "trunc";
    ValidatedZextIntVec(ZextIntVec(Integer)) => |f, t| f.bit_size() < t.bit_size(), "zext";
    ValidatedSextIntVec(SextIntVec(Integer)) => |f, t| f.bit_size() < t.bit_size(), "sext";
    ValidatedFpVecTrunc(FpVecTrunc(FloatingPoint)) => |f, t| f.bit_size() > t.bit_size(), "fptrunc";
    ValidatedFpVecExt(FpVecExt(FloatingPoint)) => |f, t| f.bit_size() < t.bit_size(), "fpext";
}

impl_intovalidated_for_always_valid_casts! {
    ValidatedFpToUi(FpToUi<FloatingPointValue, IntegerType>),
    ValidatedFpToSi(FpToSi<FloatingPointValue, IntegerType>),
    ValidatedUiToFp(UiToFp<IntegerValue, FloatingPointType>),
    ValidatedSiToFp(SiToFp<IntegerValue, FloatingPointType>),
    ValidatedPtrToInt(PtrToInt<PointerValue, IntegerType>),
    ValidatedIntToPtr(IntToPtr<IntegerValue, PointerType>),
}

impl_intovalidated_for_always_valid_vector_casts! {
    ValidatedFpVecToUiVec(FpVecToUiVec(FloatingPoint -> Integer)), "fptoui";
    ValidatedFpVecToSiVec(FpVecToSiVec(FloatingPoint -> Integer)), "fptosi";
    ValidatedUiVecToFpVec(UiVecToFpVec(Integer -> FloatingPoint)), "uitofp";
    ValidatedSiVecToFpVec(SiVecToFpVec(Integer -> FloatingPoint)), "sitofp";
    ValidatedPtrVecToIntVec(PtrVecToIntVec(Pointer -> Integer)), "ptrtoint";
    ValidatedIntVecToPtrVec(IntVecToPtrVec(Integer -> Pointer)), "inttoptr";
}

impl<V, T> IntoValidated<YieldingInstruction> for BitCast<V, T>
where
    V: value::SingleValue,
    T: ty::SingleType,
{
    type ValidatedType = ValidatedBitCast;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        let value: value::Single = self.value.into();
        let to_ty = self.to_ty.into();

        let valid = match (&value.ty(), &to_ty) {
            (ty::Single::Primitive(from), ty::Single::Primitive(to)) => match (from, to) {
                (ty::Primitive::Integer(from), ty::Primitive::Integer(to)) => {
                    from.bit_size() == to.bit_size()
                }
                (ty::Primitive::Integer(from), ty::Primitive::FloatingPoint(to)) => {
                    from.bit_size() == to.bit_size()
                }
                (ty::Primitive::FloatingPoint(from), ty::Primitive::Integer(to)) => {
                    from.bit_size() == to.bit_size()
                }
                (ty::Primitive::FloatingPoint(from), ty::Primitive::FloatingPoint(to)) => {
                    from.bit_size() == to.bit_size()
                }
                (ty::Primitive::Pointer(from), ty::Primitive::Pointer(to)) => {
                    from.address_space() == to.address_space()
                }
                (ty::Primitive::Pointer(_), _) | (_, ty::Primitive::Pointer(_)) => false,
            },
            (ty::Single::Primitive(p), ty::Single::Vector(v))
            | (ty::Single::Vector(v), ty::Single::Primitive(p)) => {
                !v.is_scalable() && {
                    match (v.element_type(), p) {
                        (ty::Primitive::Integer(e), ty::Primitive::Integer(p)) => {
                            (v.size() as u32) * e.bit_size() == p.bit_size()
                        }
                        (ty::Primitive::Integer(e), ty::Primitive::FloatingPoint(p)) => {
                            (v.size() as u32) * e.bit_size() == p.bit_size()
                        }
                        (ty::Primitive::FloatingPoint(e), ty::Primitive::Integer(p)) => {
                            (v.size() as u32) * e.bit_size() == p.bit_size()
                        }
                        (ty::Primitive::FloatingPoint(e), ty::Primitive::FloatingPoint(p)) => {
                            (v.size() as u32) * e.bit_size() == p.bit_size()
                        }
                        (ty::Primitive::Pointer(_), _) | (_, ty::Primitive::Pointer(_)) => false,
                    }
                }
            }
            (ty::Single::Vector(v1), ty::Single::Vector(v2)) => {
                v1.is_scalable() == v2.is_scalable() && {
                    match (v1.element_type(), v2.element_type()) {
                        (ty::Primitive::Integer(e1), ty::Primitive::Integer(e2)) => {
                            (v1.size() as u32) * e1.bit_size() == (v2.size() as u32) * e2.bit_size()
                        }
                        (ty::Primitive::Integer(e1), ty::Primitive::FloatingPoint(e2)) => {
                            (v1.size() as u32) * e1.bit_size() == (v2.size() as u32) * e2.bit_size()
                        }
                        (ty::Primitive::FloatingPoint(e1), ty::Primitive::Integer(e2)) => {
                            (v1.size() as u32) * e1.bit_size() == (v2.size() as u32) * e2.bit_size()
                        }
                        (ty::Primitive::FloatingPoint(e1), ty::Primitive::FloatingPoint(e2)) => {
                            (v1.size() as u32) * e1.bit_size() == (v2.size() as u32) * e2.bit_size()
                        }
                        (ty::Primitive::Pointer(e1), ty::Primitive::Pointer(e2)) => {
                            v1.size() == v2.size() && e1.address_space() == e2.address_space()
                        }
                        (ty::Primitive::Pointer(_), _) | (_, ty::Primitive::Pointer(_)) => false,
                    }
                }
            }
        };

        valid
            .then_some(ValidatedBitCast(BitCast { value, to_ty }))
            .ok_or(ValidationError::new("bitcast", "invalid types"))
    }
}
