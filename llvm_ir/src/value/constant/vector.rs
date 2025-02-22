use crate::constant::{Element, FirstClass, Primitive, Single, SingleConstant};
use crate::ty::{self, Type};
use crate::value::{Value, VectorValue};
use std::fmt::Write;

// TODO: add this doc comment to the definition of Vector
// /// Vectors of size 0 are not allowed

declare_base_constant! {
    into_vector_constant;

    (Vector(ConstantVector): ty::Vector, (VectorConstant, VectorValue): ty::VectorType)
        => (Single, SingleConstant) => Element => FirstClass

    {
        Literal { elements: Vec<Primitive> }
    }

    fn fmt_as_llvm_asm(self, f, opts, module) {
        ConstantVector::Literal { elements } => {
            f.write_char('<')?;
            let [head, tail @ ..] = elements.as_slice() else { panic!() };
            head.ty().fmt_as_llvm_asm(f, opts, module)?;
            f.write_char(' ')?;
            head.fmt_as_llvm_asm(f, opts, module)?;
            for element in tail {
                f.write_str(", ")?;
                element.ty().fmt_as_llvm_asm(f, opts, module)?;
                f.write_char(' ')?;
                element.fmt_as_llvm_asm(f, opts, module)?;
            }
            f.write_char('>')
        }
    }
}

impl Vector {
    /// A vector must contain at least one element.
    pub fn new(elements: Vec<Primitive>) -> Result<Self, String> {
        let element_type = elements
            .first()
            .ok_or("invalid arguments to vector constant constructor: expected at least 1 element")?
            .ty();

        let ty: ty::Vector = ty::Vector::new_literal(element_type.clone())
            .with_size(elements.len())?
            .build();

        for (i, element) in elements.iter().enumerate() {
            if !element.ty().equiv_to(&element_type) {
                return Err(format!(
                    "invalid arguments to vector constant constructor: invalid element at position {i}: found element of type {:?}, expected element type {:?}",
                    ty.element_type(),
                    element_type,
                ));
            }
        }

        Ok(Self {
            ty,
            value: ConstantVector::Literal { elements },
        })
    }

    pub fn new_typed(ty: ty::Vector, elements: Vec<Primitive>) -> Result<Self, String> {
        if ty.is_scalable() {
            return Err("invalid arguments to typed vector constant constructor: vector constants cannot be scalable")?;
        }

        if ty.size() != elements.len() {
            return Err(format!(
                "invalid arguments to typed vector constant constructor: found {} elements, while vector type expected size {}",
                elements.len(),
                ty.size()
            ));
        }

        {
            let ety = ty.element_type();
            for (i, element) in elements.iter().enumerate() {
                if !element.ty().equiv_to(ety) {
                    return Err(format!(
                        "invalid arguments to typed vector constant constructor: invalid element at position {i}: found element of type {:?}, expected element type {:?}",
                        ty.element_type(),
                        ety,
                    ));
                }
            }
        }

        Ok(Self {
            ty,
            value: ConstantVector::Literal { elements },
        })
    }
}
