use crate::constant::{Element, FirstClass, Primitive, PrimitiveConstant, Single};
use crate::ty;
use crate::value::{BooleanValue, IntegerValue, Value, ValueConversionError};

declare_base_constant! {
    into_integer_constant;

    (Integer(ConstantInteger): ty::Integer, (IntegerConstant, IntegerValue): ty::IntegerType)
        => (Primitive, PrimitiveConstant) => Single => Element => FirstClass
    {
        Number(i128)
    }

    fn fmt_as_llvm_asm(self, f, _opts) {
        ConstantInteger::Number(n) => write!(f, "{n}"),
    }
}

impl Integer {
    pub fn new<T: ty::IntegerType>(ty: T, value: i128) -> Self {
        // TODO: maybe check if the bit size required for `value` actually fits in `ty`?
        Self {
            ty: ty.into(),
            value: ConstantInteger::Number(value),
        }
    }

    pub fn value(&self) -> Option<i128> {
        match self.value {
            ConstantInteger::Undef => None,
            ConstantInteger::Poison => None,
            ConstantInteger::ZeroInitializer => Some(0),
            ConstantInteger::Number(n) => Some(n),
        }
    }
}

//
// Boolean
//

impl From<Boolean> for Integer {
    fn from(value: Boolean) -> Self {
        Self {
            ty: value.ty.into(),
            value: match value.value {
                ConstantBoolean::Undef => ConstantInteger::Undef,
                ConstantBoolean::Poison => ConstantInteger::Poison,
                ConstantBoolean::ZeroInitializer => ConstantInteger::ZeroInitializer,
                ConstantBoolean::Bool(false) => ConstantInteger::Number(0),
                ConstantBoolean::Bool(true) => ConstantInteger::Number(1),
            },
        }
    }
}

impl TryFrom<Integer> for Boolean {
    type Error = ValueConversionError;

    fn try_from(value: Integer) -> Result<Self, Self::Error> {
        if value.ty().bit_size() != 1 {
            return Err(ValueConversionError::new::<Integer, Self>(value));
        }
        Ok(match value.value {
            ConstantInteger::Undef => crate::constant::Undef(ty::I1).into(),
            ConstantInteger::Poison => crate::constant::Poison(ty::I1).into(),
            ConstantInteger::ZeroInitializer => crate::constant::ZeroInitializer(ty::I1).into(),
            ConstantInteger::Number(0) => Boolean::new(false),
            ConstantInteger::Number(1) => Boolean::new(true),
            _ => return Err(ValueConversionError::new::<Integer, Self>(value)),
        })
    }
}

declare_base_constant! {
    into_boolean_constant;

    (Boolean(ConstantBoolean): ty::Int<1>, (BooleanConstant, BooleanValue): ty::IntType<1>)
        => (Integer, IntegerConstant) => Primitive => Single => Element => FirstClass
    {
        Bool(bool)
    }

    fn fmt_as_llvm_asm(self, f, _opts) {
        ConstantBoolean::Bool(b) => write!(f, "{b}"),
    }
}

impl Boolean {
    pub const fn new(value: bool) -> Self {
        Self {
            ty: ty::Int::<1>::Literal,
            value: ConstantBoolean::Bool(value),
        }
    }

    pub const fn new_typed(ty: ty::Int<1>, value: bool) -> Self {
        Self {
            ty,
            value: ConstantBoolean::Bool(value),
        }
    }
}
