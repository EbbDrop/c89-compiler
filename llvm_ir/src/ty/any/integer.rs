use crate::ty::{
    Any, BitSize, Element, FirstClass, Identified, Primitive, PrimitiveType, Single, Type,
};

pub const I1: Int<1> = Int::new_literal();
pub const I8: Int<8> = Int::new_literal();
pub const I16: Int<16> = Int::new_literal();
pub const I32: Int<32> = Int::new_literal();
pub const I64: Int<64> = Int::new_literal();

declare_base_type! {
    into_integer;
    (Integer(LiteralInteger), IntegerType: BitSize)
        => (Primitive, PrimitiveType) => Single => Element => FirstClass => Any
}

impl Integer {
    /// The bit size of integer types must be at least 1, and less than `2.pow(23)`
    pub fn new_literal(bit_size: u32) -> Result<Self, String> {
        if (1..(1 << 23)).contains(&bit_size) {
            Ok(Self::Literal(LiteralInteger(bit_size)))
        } else {
            Err(format!("invalid arguments to literal integer type constructor: invalid bit size: {bit_size}"))
        }
    }

    /// The bit size of integer types must be at least 1, and less than `2.pow(23)`
    pub const fn const_new_literal(bit_size: u32) -> Self {
        if bit_size >= 1 && bit_size < (1 << 23) {
            Self::Literal(LiteralInteger(bit_size))
        } else {
            panic!("invalid arguments to const literal integer constructor: invalid bit size")
        }
    }

    impl_base_type_getter! { bit_size: u32 = |literal| literal.0 }
}

impl BitSize for Integer {
    fn bit_size(&self) -> u32 {
        self.bit_size()
    }
}

impl Type for Integer {
    fn equiv_to(&self, other: &Self) -> bool {
        self.bit_size() == other.bit_size()
    }

    fn has_opaque_struct(&self) -> bool {
        false
    }

    fn has_scalable_vec(&self) -> bool {
        false
    }

    impl_base_type_is_identified! {}
}

impl crate::FmtAsLlvmAsmMC for Integer {
    impl_base_type_fmt! {
        |literal, f, opts, module| write!(f, "i{}", literal.0)
    }
}

#[derive(Debug, Clone)]
pub struct LiteralInteger(u32);

//
// Int<N>
//

impl<const N: u32> From<Int<N>> for Integer {
    /// Will panic if the const `N` is not a valid bit size.
    fn from(value: Int<N>) -> Self {
        match value {
            Int::Literal => Self::new_literal(N).expect(
                "the constant bit size N of Int<N> must be at least 1, and less than `2.pow(23)`",
            ),
            Int::Identified(identified) => Self::Identified(Box::new(Identified {
                ty: identified.ty.into(),
                handle: identified.handle,
            })),
        }
    }
}

impl<const N: u32> TryFrom<Integer> for Int<N> {
    type Error = crate::ty::TypeConversionError;

    fn try_from(value: Integer) -> Result<Self, Self::Error> {
        match value {
            Integer::Literal(LiteralInteger(n)) => {
                if n == N {
                    Ok(Self::new_literal())
                } else {
                    Err(crate::ty::TypeConversionError::new::<Integer, Self>(value))
                }
            }
            Integer::Identified(identified) => Ok(Self::Identified(Box::new(Identified {
                ty: identified.ty.try_into()?,
                handle: identified.handle,
            }))),
        }
    }
}

declare_base_type! {
    <{const N: u32}>
    into_int;
    (Int, IntType: BitSize)<{N}>
        => (Integer, IntegerType) => Primitive => Single => Element => FirstClass => Any
}

impl<const N: u32> Int<N> {
    pub const fn new_literal() -> Self {
        Self::Literal
    }
}

impl<const N: u32> BitSize for Int<N> {
    fn bit_size(&self) -> u32 {
        N
    }
}

impl<const N: u32> Type for Int<N> {
    fn equiv_to(&self, _other: &Self) -> bool {
        true
    }

    fn has_opaque_struct(&self) -> bool {
        false
    }

    fn has_scalable_vec(&self) -> bool {
        false
    }

    impl_base_type_is_identified! {}
}

impl<const N: u32> crate::FmtAsLlvmAsmMC for Int<N> {
    impl_base_type_fmt! { |f, opts, module| write!(f, "i{N}") }
}
