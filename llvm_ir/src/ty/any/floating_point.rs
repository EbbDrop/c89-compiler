use crate::ty::{Any, BitSize, Element, FirstClass, Primitive, PrimitiveType, Single, Type};

macro_rules! declare_floating_point_type {
    (
        $($Type:ident : $TypeCat:ident => ($bit_size:literal, $fmt_str:literal, $into_ty:ident)),+
        $(,)?
    ) => {
        declare_type_cat! {
            into_floating_point;
            try_from_floating_point;
            (FloatingPoint, FloatingPointType: BitSize)
                => (Primitive, PrimitiveType) => Single => Element => FirstClass => Any
            {
                $($Type,)+
            }
        }

        impl BitSize for FloatingPoint {
            fn bit_size(&self) -> u32 {
                match self {
                    $(Self::$Type(_) => $bit_size,)+
                }
            }
        }

        $(
            declare_base_type! {
                $into_ty;
                ($Type, $TypeCat: BitSize) => (FloatingPoint, FloatingPointType)
                    => Primitive => Single => Element => FirstClass => Any
            }

            impl $Type {
                pub const fn new_literal() -> Self {
                    Self::Literal
                }
            }

            impl BitSize for $Type {
                fn bit_size(&self) -> u32 { $bit_size }
            }

            impl Type for $Type {
                fn equiv_to(&self, _other: &Self) -> bool { true }
                fn has_opaque_struct(&self) -> bool { false }
                fn has_scalable_vec(&self) -> bool { false }
                impl_base_type_is_identified! {}
            }

            impl crate::FmtAsLlvmAsmMC for $Type {
                impl_base_type_fmt! { |f, opts, module| f.write_str($fmt_str) }
            }
        )+
    };
}

declare_floating_point_type! {
    Half: HalfType => (16, "half", into_half),
    BFloat: BFloatType => (16, "bfloat", into_bfloat),
    Float: FloatType => (32, "float", into_float),
    Double: DoubleType => (64, "double", into_double),
    Fp128: Fp128Type => (128, "fp128", into_fp128),
    X86Fp80: X86Fp80Type => (80, "x86fp80", into_x86fp80),
    PpcFp128: PpcFp128Type => (128, "ppcfp128", into_ppcfp128),
}
