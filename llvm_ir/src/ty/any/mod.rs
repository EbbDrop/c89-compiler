mod array;
mod floating_point;
mod function;
mod integer;
mod label;
mod pointer;
mod structure;
mod vector;

pub use array::*;
pub use floating_point::*;
pub use function::*;
pub use integer::*;
pub use label::*;
pub use pointer::*;
pub use structure::*;
pub use vector::*;

declare_type_cat! {
    into_any;
    try_from_any;
    (Any, AnyType)
    {
        Function,
        FirstClass,
    }
}

declare_type_cat! {
    into_first_class;
    try_from_first_class;
    (FirstClass, FirstClassType) => (Any, AnyType)
    {
        Label,
        Element,
    }
}

declare_type_cat! {
    into_element;
    try_from_element;
    (Element, ElementType) => (FirstClass, FirstClassType) => Any
    {
        Single,
        Aggregate,
    }
}

declare_type_cat! {
    into_single;
    try_from_single;
    (Single, SingleType) => (Element, ElementType) => FirstClass => Any
    {
        Primitive,
        Vector,
    }
}

declare_type_cat! {
    into_aggregate;
    try_from_aggregate;
    (Aggregate, AggregateType) => (Element, ElementType) => FirstClass => Any
    {
        Array,
        Structure,
    }
}

declare_type_cat! {
    into_primitive;
    try_from_primitive;
    (Primitive, PrimitiveType) => (Single, SingleType) => Element => FirstClass => Any
    {
        Integer,
        FloatingPoint,
        Pointer,
    }
}
