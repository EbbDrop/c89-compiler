mod array;
mod floating_point;
mod integer;
mod label;
mod pointer;
mod structure;
mod vector;

pub use array::*;
pub use floating_point::*;
pub use integer::*;
pub use label::*;
pub use pointer::*;
pub use structure::*;
pub use vector::*;

use crate::ty;
use crate::value::{AggregateValue, ElementValue, FirstClassValue, PrimitiveValue, SingleValue};

declare_constant_cat! {
    into_first_class;
    @no_special _;
    @FmtAsLlvmAsmFC _;
    (FirstClass, (FirstClassConstant, FirstClassValue): ty::FirstClassType)
    {
        Label,
        Element,
    }
}

declare_constant_cat! {
    into_element;
    (Element, (ElementConstant, ElementValue): ty::ElementType)
        => (FirstClass, FirstClassConstant)
    {
        Single,
        Aggregate,
    }
}

declare_constant_cat! {
    into_single;
    (Single, (SingleConstant, SingleValue): ty::SingleType)
        => (Element, ElementConstant) => FirstClass
    {
        Primitive,
        Vector,
    }
}

declare_constant_cat! {
    into_aggregate;
    (Aggregate, (AggregateConstant, AggregateValue): ty::AggregateType)
        => (Element, ElementConstant) => FirstClass
    {
        Array,
        Structure,
    }
}

declare_constant_cat! {
    into_primitive;
    (Primitive, (PrimitiveConstant, PrimitiveValue): ty::PrimitiveType)
        => (Single, SingleConstant) => Element => FirstClass
    {
        Integer,
        FloatingPoint,
        Pointer,
    }
}

#[derive(Debug, Clone)]
pub struct Undef<T: ty::FirstClassType>(pub T);

#[derive(Debug, Clone)]
pub struct Poison<T: ty::FirstClassType>(pub T);

#[derive(Debug, Clone)]
pub struct ZeroInitializer<T: ty::FirstClassType>(pub T);
