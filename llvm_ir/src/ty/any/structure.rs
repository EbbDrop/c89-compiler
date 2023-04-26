use crate::module;
use crate::ty::{Aggregate, AggregateType, Any, Element, FirstClass, Identified, Type};

declare_base_type! {
    into_structure;
    (Structure(LiteralStructure), StructureType)
        => (Aggregate, AggregateType) => Element => FirstClass => Any
}

impl Structure {
    pub(crate) const fn new_opaque(handle: module::LocalIdHandle) -> Identified<Self> {
        Identified {
            ty: Self::Literal(LiteralStructure(InnerLiteralStructure::Opaque)),
            handle,
        }
    }

    pub fn new_literal(body_types: Vec<Element>) -> LiteralStructureBuilder {
        LiteralStructureBuilder(ConcreteLiteralStructure {
            body_types,
            is_packed: false,
        })
    }

    impl_base_type_getter! {
        body_types: Option<&[Element]> = |literal| match &literal.0 {
            InnerLiteralStructure::Opaque => None,
            InnerLiteralStructure::Concrete(ConcreteLiteralStructure { body_types, .. }) => Some(body_types),
        }
    }

    impl_base_type_getter! {
        is_packed: Option<bool> = |literal| match &literal.0 {
            InnerLiteralStructure::Opaque => None,
            InnerLiteralStructure::Concrete(ConcreteLiteralStructure { is_packed, .. }) => Some(*is_packed)
        }
    }
}

impl Identified<Structure> {
    fn unwrap_aliases(&self) -> (&InnerLiteralStructure, &module::LocalIdHandle) {
        match &self.ty {
            Structure::Literal(literal) => (&literal.0, &self.handle),
            Structure::Identified(identified) => {
                if self.handle == identified.handle {
                    // type is an alias to itself
                    unreachable!()
                } else {
                    identified.unwrap_aliases()
                }
            }
        }
    }
}

impl Type for Structure {
    /// Opaque structs are not considered equivalent, unless identified by the same identifier
    fn equiv_to(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Identified(i1), Self::Identified(i2)) => {
                if let (InnerLiteralStructure::Opaque, handle1) = i1.unwrap_aliases() {
                    if let (InnerLiteralStructure::Opaque, handle2) = i2.unwrap_aliases() {
                        return handle1 == handle2;
                    }
                }
            }
            // Literal opaque structs cannot be created directly. The only way to create an opaque
            // struct is by 'declaring' an opaque identified type in the module. This ensures every
            // opaque struct type that exists, is wrapped in at least one Identifier<Structure>.
            // So all opaque struct cases should be handled by the above. Of course the struct
            // can still contain opaque elements, which is checked by the rest of this function.
            (Self::Literal(LiteralStructure(InnerLiteralStructure::Opaque)), _)
            | (_, Self::Literal(LiteralStructure(InnerLiteralStructure::Opaque))) => unreachable!(),
            _ => {}
        }

        let Some(self_is_packed) = self.is_packed() else { return false; };
        let Some(other_is_packed) = self.is_packed() else { return false; };
        self_is_packed == other_is_packed && {
            let Some(self_body_types) = self.body_types() else { return false; };
            let Some(other_body_types) = other.body_types() else { return false; };
            self_body_types
                .iter()
                .zip(other_body_types)
                .all(|(a, b)| a.equiv_to(b))
        }
    }

    fn has_opaque_struct(&self) -> bool {
        match self {
            Structure::Literal(literal) => match &literal.0 {
                InnerLiteralStructure::Opaque => true,
                InnerLiteralStructure::Concrete(ConcreteLiteralStructure {
                    body_types, ..
                }) => body_types.iter().any(|t| t.has_opaque_struct()),
            },
            Structure::Identified(identified) => identified.ty.has_opaque_struct(),
        }
    }

    /// Returns `false` for opaque structures.
    fn has_scalable_vec(&self) -> bool {
        let Some(body_types) = self.body_types() else { return false; };
        body_types.iter().any(|t| t.has_scalable_vec())
    }

    impl_base_type_is_identified! {}
}

impl crate::FmtAsLlvmAsmMC for Structure {
    impl_base_type_fmt! {
        |literal, f, opts, module| match &literal.0 {
            InnerLiteralStructure::Opaque => f.write_str("opaque"),
            InnerLiteralStructure::Concrete(ConcreteLiteralStructure {
                body_types,
                is_packed,
            }) => {
                f.write_str(if *is_packed { "<{" } else { "{" })?;
                if let [head, tail @ ..] = body_types.as_slice() {
                    head.fmt_as_llvm_asm(f, opts, module)?;
                    for ty in tail {
                        f.write_str(", ")?;
                        ty.fmt_as_llvm_asm(f, opts, module)?;
                    }
                }
                f.write_str(if *is_packed { "}>" } else { "}" })
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct LiteralStructure(InnerLiteralStructure);

#[derive(Debug, Clone)]
enum InnerLiteralStructure {
    Opaque,
    Concrete(ConcreteLiteralStructure),
}

#[derive(Debug, Clone)]
struct ConcreteLiteralStructure {
    body_types: Vec<Element>,
    is_packed: bool,
}

#[derive(Debug, Clone)]
pub struct LiteralStructureBuilder(ConcreteLiteralStructure);

impl LiteralStructureBuilder {
    pub fn with_packed(mut self, is_packed: bool) -> Self {
        self.0.is_packed = is_packed;
        self
    }

    pub fn build(self) -> Structure {
        Structure::Literal(LiteralStructure(InnerLiteralStructure::Concrete(self.0)))
    }
}
