use crate::constant::{Aggregate, AggregateConstant, Element, FirstClass};
use crate::ty::{self, Type};
use crate::value::{StructureValue, Value};
use std::fmt::Write;

declare_base_constant! {
    into_structure_constant;

    (Structure(ConstantStructure): ty::Structure, (StructureConstant, StructureValue): ty::StructureType)
        => (Aggregate, AggregateConstant) => Element => FirstClass
    {
        Literal { body: Vec<Element> }
    }

    fn fmt_as_llvm_asm(self, f, opts, module) {
        ConstantStructure::Literal { body } => {
            let is_packed = self
                .ty()
                .is_packed()
                .expect("constant structures cannot be opaque");
            f.write_str(if is_packed { "<{" } else { "{" })?;
            if let [head, tail @ ..] = body.as_slice() {
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
            f.write_str(if is_packed { "}>" } else { "}" })
        }
    }
}

impl Structure {
    pub fn new(body: Vec<Element>) -> Self {
        Self {
            ty: ty::Structure::new_literal(body.iter().map(Value::ty).collect()).build(),
            value: ConstantStructure::Literal { body },
        }
    }

    pub fn new_typed(ty: ty::Structure, body: Vec<Element>) -> Result<Self, String> {
        let body_types = ty
            .body_types()
            .ok_or("invalid arguments to typed structure constant constructor: cannot create constant of opaque structure type")?;

        for (i, (t, e)) in body_types.iter().zip(&body).enumerate() {
            if !e.ty().equiv_to(t) {
                return Err(format!(
                    "invalid arguments to typed structure constructor: invalid element at position {i}: found element of type {:?}, expected element of type {:?}",
                    e.ty(),
                    t
                ));
            }
        }

        Ok(Self {
            ty,
            value: ConstantStructure::Literal { body },
        })
    }
}
