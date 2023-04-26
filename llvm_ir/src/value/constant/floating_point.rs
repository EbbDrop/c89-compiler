use crate::constant::{Element, FirstClass, Primitive, PrimitiveConstant, Single};
use crate::ty;
use crate::value::FloatingPointValue;

declare_base_constant! {
    into_floating_point_constant;

    (FloatingPoint(ConstantFloatingPoint): ty::FloatingPoint, (FloatingPointConstant, FloatingPointValue): ty::FloatingPointType)
        => (Primitive, PrimitiveConstant) => Single => Element => FirstClass
    {
        Half(u16),
        BFloat(u16),
        Float(f32),
        Double(f64),
        Fp128(u128),
        X86Fp80(u128),
        PpcFp128(u128),
    }

    fn fmt_as_llvm_asm(self, f, _opts) {
        ConstantFloatingPoint::Half(fp) => write!(f, "{fp:#0x}"),
        ConstantFloatingPoint::BFloat(fp) => write!(f, "{fp:#0x}"),
        ConstantFloatingPoint::Float(fp) => write!(f, "{:#0x}", fp.to_bits()),
        ConstantFloatingPoint::Double(fp) => write!(f, "{:#0x}", fp.to_bits()),
        ConstantFloatingPoint::Fp128(fp) => write!(f, "{fp:#0x}"),
        ConstantFloatingPoint::X86Fp80(fp) => write!(f, "{fp:#0x}"),
        ConstantFloatingPoint::PpcFp128(fp) => write!(f, "{fp:#0x}"),
    }
}

macro_rules! impl_constructor {
    ($name:ident, $name_typed:ident, $type:ident, $value_ty:ty) => {
        pub fn $name(value: $value_ty) -> Self {
            Self {
                ty: ty::$type::new_literal().into(),
                value: ConstantFloatingPoint::$type(value),
            }
        }

        pub fn $name_typed(ty: ty::$type, value: $value_ty) -> Self {
            Self {
                ty: ty.into(),
                value: ConstantFloatingPoint::$type(value),
            }
        }
    };
}

impl FloatingPoint {
    pub fn zero_typed(ty: ty::FloatingPoint) -> Self {
        let value = match &ty {
            ty::FloatingPoint::Half(_) => ConstantFloatingPoint::Half(0),
            ty::FloatingPoint::BFloat(_) => ConstantFloatingPoint::BFloat(0),
            ty::FloatingPoint::Float(_) => ConstantFloatingPoint::Float(0.0),
            ty::FloatingPoint::Double(_) => ConstantFloatingPoint::Double(0.0),
            ty::FloatingPoint::Fp128(_) => ConstantFloatingPoint::Fp128(0),
            ty::FloatingPoint::X86Fp80(_) => ConstantFloatingPoint::X86Fp80(0),
            ty::FloatingPoint::PpcFp128(_) => ConstantFloatingPoint::PpcFp128(0),
        };
        Self { ty, value }
    }

    impl_constructor! { new_half, new_half_typed, Half, u16 }
    impl_constructor! { new_bfloat, new_bfloat_typed, BFloat, u16 }
    impl_constructor! { new_float, new_float_typed, Float, f32 }
    impl_constructor! { new_double, new_double_typed, Double, f64 }
    impl_constructor! { new_fp128, new_fp128_typed, Fp128, u128 }
    impl_constructor! { new_x86fp80, new_x86fp80_typed, X86Fp80, u128 }
    impl_constructor! { new_ppcfp128, new_ppcfp128_typed, PpcFp128, u128 }
}
