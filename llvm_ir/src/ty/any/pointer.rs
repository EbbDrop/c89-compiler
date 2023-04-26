use crate::ty::{Any, Element, FirstClass, Primitive, PrimitiveType, Single, Type};
use crate::AddressSpace;
use std::fmt::Write;

declare_base_type! {
    into_pointer;
    (Pointer(LiteralPointer), PointerType)
        => (Primitive, PrimitiveType) => Single => Element => FirstClass => Any
}

impl Default for Pointer {
    fn default() -> Self {
        Self::new_literal().build()
    }
}

impl Pointer {
    pub const fn new_literal() -> LiteralPointerBuilder {
        LiteralPointerBuilder(LiteralPointer {
            address_space: AddressSpace::default(),
        })
    }

    impl_base_type_getter! { address_space: AddressSpace = |literal| literal.address_space}
}

impl Type for Pointer {
    fn equiv_to(&self, other: &Self) -> bool {
        self.address_space() == other.address_space()
    }

    fn has_opaque_struct(&self) -> bool {
        false
    }

    fn has_scalable_vec(&self) -> bool {
        false
    }

    impl_base_type_is_identified! {}
}

impl crate::FmtAsLlvmAsmMC for Pointer {
    impl_base_type_fmt! {
        |literal, f, opts, module| {
            f.write_str("ptr")?;
            if literal.address_space != AddressSpace::default() {
                f.write_char(' ')?;
                literal.address_space.fmt_as_llvm_asm(f, opts, module)?;
            }
            Ok(())
        }
    }
}

#[derive(Debug, Clone)]
pub struct LiteralPointer {
    address_space: AddressSpace,
}

pub struct LiteralPointerBuilder(LiteralPointer);

impl LiteralPointerBuilder {
    pub const fn with_address_space(mut self, address_space: AddressSpace) -> Self {
        self.0.address_space = address_space;
        self
    }

    pub const fn build(self) -> Pointer {
        Pointer::Literal(self.0)
    }
}
