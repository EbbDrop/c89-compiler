use crate::constant::{Aggregate, AggregateConstant, Element, FirstClass};
use crate::ty::{self, Type};
use crate::value::{ArrayValue, Value};
use std::fmt::Write;

declare_base_constant! {
    into_array_constant;

    (Array(ConstantArray): ty::Array, (ArrayConstant, ArrayValue): ty::ArrayType)
        => (Aggregate, AggregateConstant) => Element => FirstClass
    {
        Literal { elements: Vec<Element> },
        CharArray { elements: Vec<u8> },
    }

    fn fmt_as_llvm_asm(self, f, opts, module) {
        ConstantArray::Literal { elements } => {
            f.write_char('[')?;
            if let [head, tail @ ..] = elements.as_slice() {
                head.ty().fmt_as_llvm_asm(f, opts, module)?;
                f.write_char(' ')?;
                head.fmt_as_llvm_asm(f, opts, module)?;
                for element in tail {
                    f.write_str(", ")?;
                    element.ty().fmt_as_llvm_asm(f, opts, module)?;
                    f.write_char(' ')?;
                    element.fmt_as_llvm_asm(f, opts, module)?;
                }
            }
            f.write_char(']')
        }
        ConstantArray::CharArray { elements } => {
            f.write_char('c')?;
            crate::fmt_as_llvm_quoted_string(f, elements.iter().copied())
        }
    }
}

impl Array {
    pub fn new_char_array_typed(ty: ty::Array, elements: Vec<u8>) -> Result<Self, String> {
        if ty.size() != elements.len() {
            return Err(format!(
                "invalid arguments to typed char array constant constructor: found {} elements, while array type expected size {}",
                elements.len(),
                ty.size(),
            ));
        }
        if !ty.element_type().equiv_to(&ty::I8.into()) {
            return Err(format!(
                "invalid arguments to typed char array constant constructor: wrong element type: found {:?}, expected type i8",
                ty.element_type(),
            ));
        }
        Ok(Self {
            ty,
            value: ConstantArray::CharArray { elements },
        })
    }

    pub fn new_char_array(elements: Vec<u8>) -> Self {
        Self {
            ty: ty::Array::new_literal(ty::I8.into())
                .unwrap()
                .with_size(elements.len())
                .build(),
            value: ConstantArray::CharArray { elements },
        }
    }

    /// NOTE: this does **not** add a NULL byte to the end!
    pub fn new_char_array_from_str(elements: impl Into<String>) -> Self {
        Self::new_char_array(elements.into().into_bytes())
    }

    pub fn new_typed(ty: ty::Array, elements: Vec<Element>) -> Result<Self, String> {
        if ty.size() != elements.len() {
            return Err(format!(
                "invalid arguments to typed array constant constructor: found {} elements, while array type expected size {}",
                elements.len(),
                ty.size(),
            ));
        }
        {
            let ety = ty.element_type();
            for (i, element) in elements.iter().enumerate() {
                if !element.ty().equiv_to(ety) {
                    return Err(format!(
                        "invalid arguments to typed array constant constructor: invalid element at position {i}: found element of type {:?}, expected element type {:?}",
                        ty.element_type(),
                        ety,
                    ));
                }
            }
        }
        Ok(Self {
            ty,
            value: ConstantArray::Literal { elements },
        })
    }
}
