use crate::ty::{Any, FirstClass, FirstClassType, Type};

declare_base_type! {
    into_label;
    (Label, LabelType) => (FirstClass, FirstClassType) => Any
}

impl Label {
    pub const fn new_literal() -> Self {
        Self::Literal
    }
}

impl Type for Label {
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

impl crate::FmtAsLlvmAsmMC for Label {
    impl_base_type_fmt! { |f, opts, module| f.write_str("label") }
}
