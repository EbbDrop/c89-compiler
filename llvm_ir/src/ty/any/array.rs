use crate::ty::{Aggregate, AggregateType, Any, Element, FirstClass, Single, Type};
use std::fmt::Write;

declare_base_type! {
    into_array;
    (Array(LiteralArray), ArrayType)
        => (Aggregate, AggregateType) => Element => FirstClass => Any
}

impl Array {
    pub fn new_literal(element_type: Element) -> Result<LiteralArrayBuilder, String> {
        if let Element::Single(Single::Vector(vector)) = &element_type {
            if vector.is_scalable() {
                return Err("invalid arguments to literal array type constructor: arrays can only have sized elements")?;
            }
        }
        Ok(LiteralArrayBuilder(LiteralArray {
            size: 0,
            element_type: Box::new(element_type),
        }))
    }

    impl_base_type_getter! { size: usize = |literal| literal.size }
    impl_base_type_getter! { element_type: &Element = |literal| &literal.element_type }
}

impl Type for Array {
    fn equiv_to(&self, other: &Self) -> bool {
        self.size() == other.size() && self.element_type().equiv_to(other.element_type())
    }

    fn has_opaque_struct(&self) -> bool {
        self.element_type().has_opaque_struct()
    }

    fn has_scalable_vec(&self) -> bool {
        self.element_type().has_scalable_vec()
    }

    impl_base_type_is_identified! {}
}

impl crate::FmtAsLlvmAsmMC for Array {
    impl_base_type_fmt! {
        |literal, f, opts, module| {
            write!(f, "[{} x ", literal.size)?;
            literal.element_type.fmt_as_llvm_asm(f, opts, module)?;
            f.write_char(']')
        }
    }
}

#[derive(Debug, Clone)]
pub struct LiteralArray {
    size: usize,
    element_type: Box<Element>,
}

#[derive(Debug, Clone)]
pub struct LiteralArrayBuilder(LiteralArray);

impl LiteralArrayBuilder {
    pub fn with_size(mut self, size: usize) -> Self {
        self.0.size = size;
        self
    }

    pub fn build(self) -> Array {
        Array::Literal(self.0)
    }
}
