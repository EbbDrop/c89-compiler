#[macro_use]
mod macros;

mod register;

pub mod constant;

pub use register::*;

use crate::ty;

#[derive(Debug)]
pub struct ValueConversionError(String);

impl ValueConversionError {
    fn new<From: Value, To: Value>(value: From) -> Self {
        Self(format!(
            "value conversion of {} into {} failed: error converting value: {value:?}",
            std::any::type_name::<From>(),
            std::any::type_name::<To>(),
        ))
    }

    fn wrap<From: Value, To: Value>(value: From, inner_err: impl std::fmt::Display) -> Self {
        Self(format!(
            "value conversion of {} into {} failed: error converting value: {value:?}; failed due to: {inner_err}",
            std::any::type_name::<From>(),
            std::any::type_name::<To>(),
        ))
    }
}

impl std::fmt::Display for ValueConversionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::error::Error for ValueConversionError {}

impl From<ValueConversionError> for String {
    fn from(value: ValueConversionError) -> Self {
        value.to_string()
    }
}

pub trait Value: std::fmt::Debug + Clone + crate::FmtAsLlvmAsmFC {
    type Type: ty::FirstClassType;

    fn ty(&self) -> Self::Type;
}

declare_value_cat! {
    into_first_class;
    (FirstClass: ty::FirstClass, FirstClassValue: ty::FirstClassType) {
        Label,
        Element,
    }
}

declare_value_cat! {
    into_element;
    (Element: ty::Element, ElementValue: ty::ElementType)
        => (FirstClass, FirstClassValue)
    {
        Single,
        Aggregate,
    }
}

declare_value_cat! {
    into_single;
    (Single: ty::Single, SingleValue: ty::SingleType)
        => (Element, ElementValue) => FirstClass
    {
        Primitive,
        Vector,
    }
}

declare_value_cat! {
    into_aggregate;
    (Aggregate: ty::Aggregate, AggregateValue: ty::AggregateType)
        => (Element, ElementValue) => FirstClass
    {
        Array,
        Structure,
    }
}

declare_value_cat! {
    into_primitive;
    (Primitive: ty::Primitive, PrimitiveValue: ty::PrimitiveType)
        => (Single, SingleValue) => Element => FirstClass
    {
        Integer,
        FloatingPoint,
        Pointer,
    }
}

declare_base_value! {
    into_label;
    @FmtAsLlvmAsmFC _;
    (Label: ty::Label, LabelValue: ty::LabelType)
        => (FirstClass, FirstClassValue)
}

declare_base_value! {
    into_array;
    (Array: ty::Array, ArrayValue: ty::ArrayType)
        => (Aggregate, AggregateValue) => Element => FirstClass
}

declare_base_value! {
    into_structure;
    (Structure: ty::Structure, StructureValue: ty::StructureType)
        => (Aggregate, AggregateValue) => Element => FirstClass
}

declare_base_value! {
    into_vector;
    (Vector: ty::Vector, VectorValue: ty::VectorType)
        => (Single, SingleValue) => Element => FirstClass
}

declare_base_value! {
    into_integer;
    (Integer: ty::Integer, IntegerValue: ty::IntegerType)
        => (Primitive, PrimitiveValue) => Single => Element => FirstClass
}

declare_base_value! {
    into_boolean;
    (Boolean: ty::Int<1>, BooleanValue: ty::IntType<1>)
        => (Integer, IntegerValue) => Primitive => Single => Element => FirstClass
}

declare_base_value! {
    into_floating_point;
    (FloatingPoint: ty::FloatingPoint, FloatingPointValue: ty::FloatingPointType)
        => (Primitive, PrimitiveValue) => Single => Element => FirstClass
}

declare_base_value! {
    into_pointer;
    (Pointer: ty::Pointer, PointerValue: ty::PointerType)
        => (Primitive, PrimitiveValue) => Single => Element => FirstClass
}

impl From<Boolean> for Integer {
    fn from(value: Boolean) -> Self {
        match value {
            Boolean::Constant(constant) => Self::Constant(constant.into()),
            Boolean::Register(register) => Self::Register(Register {
                ty: register.ty.into(),
                handle: register.handle,
            }),
        }
    }
}

impl TryFrom<Integer> for Boolean {
    type Error = ValueConversionError;

    fn try_from(value: Integer) -> Result<Self, Self::Error> {
        use crate::convert::TryInto_;
        match value {
            Integer::Constant(constant) => Ok(Self::Constant(constant.try_into()?)),
            Integer::Register(register) => Ok(Self::Register(register.try_into_()?)),
        }
    }
}
