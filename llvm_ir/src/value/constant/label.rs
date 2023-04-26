use crate::constant::FirstClassConstant;
use crate::value::LabelValue;
use crate::{function, ty};
use std::fmt;

declare_base_constant! {
    @no_special
    into_label;
    (Label: pub(crate) ty::Label, (LabelConstant, LabelValue)) => (FirstClass, FirstClassConstant)
    {
        pub(crate) handle: function::LocalIdHandle
    }
}

impl Label {
    pub(crate) fn with_literal_type(handle: function::LocalIdHandle) -> Self {
        Self {
            ty: ty::Label::new_literal(),
            handle,
        }
    }
}

impl crate::FmtAsLlvmAsmFC for Label {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
        function: &crate::FunctionDeclaration,
    ) -> fmt::Result {
        function
            .id(self.handle)
            .fmt_as_llvm_asm(f, opts, module, function)
    }
}
