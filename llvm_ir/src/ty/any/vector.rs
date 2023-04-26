use crate::ty::{Any, Element, FirstClass, Primitive, Single, SingleType, Type};
use std::fmt::Write;

declare_base_type! {
    into_vector;
    (Vector(LiteralVector), VectorType)
        => (Single, SingleType) => Element => FirstClass => Any
}

impl Vector {
    pub fn new_literal(element_type: Primitive) -> LiteralVectorBuilder {
        LiteralVectorBuilder(LiteralVector {
            size: 1,
            element_type: Box::new(element_type),
            is_scalable: false,
        })
    }

    impl_base_type_getter! { size: usize = |literal| literal.size }
    impl_base_type_getter! { element_type: &Primitive = |literal| &literal.element_type }
    impl_base_type_getter! { is_scalable: bool = |literal| literal.is_scalable }
}

impl Type for Vector {
    fn equiv_to(&self, other: &Self) -> bool {
        self.is_scalable() == other.is_scalable()
            && self.size() == other.size()
            && self.element_type().equiv_to(other.element_type())
    }

    fn has_opaque_struct(&self) -> bool {
        self.element_type().has_opaque_struct()
    }

    fn has_scalable_vec(&self) -> bool {
        self.is_scalable()
    }

    impl_base_type_is_identified! {}
}

impl crate::FmtAsLlvmAsmMC for Vector {
    impl_base_type_fmt! {
        |literal, f, opts, module| {
            f.write_char('<')?;
            if literal.is_scalable {
                f.write_str("vscale x ")?;
            }
            write!(f, "{} x ", literal.size)?;
            literal.element_type.fmt_as_llvm_asm(f, opts, module)?;
            f.write_char('>')
        }
    }
}

#[derive(Debug, Clone)]
pub struct LiteralVector {
    size: usize,
    element_type: Box<Primitive>,
    is_scalable: bool,
}

#[derive(Debug, Clone)]
pub struct LiteralVectorBuilder(LiteralVector);

impl LiteralVectorBuilder {
    pub fn with_size(mut self, size: usize) -> Result<Self, String> {
        if size > 0 {
            self.0.size = size;
            Ok(self)
        } else {
            Err(format!("invalid size for vector type: {size}"))
        }
    }

    pub fn with_scalable(mut self, is_scalable: bool) -> Self {
        self.0.is_scalable = is_scalable;
        self
    }

    pub fn build(self) -> Vector {
        Vector::Literal(self.0)
    }
}
