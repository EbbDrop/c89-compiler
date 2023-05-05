use crate::value::Value;
use crate::{constant, ty, value};
use crate::{AddressSignificance, AddressSpace, Alignment, Linkage, Visibility};
use std::fmt::{self, Write};

#[derive(Debug, Clone)]
pub struct GlobalVarDeclaration {
    pub comment: Option<String>,
    pub linkage: Linkage,
    pub visibility: Visibility,
    pub address_significance: Option<AddressSignificance>,
    pub address_space: AddressSpace,
    pub alignment: Alignment,
    pub is_constant: bool,
    pub ty: ty::Element,
}

impl GlobalVarDeclaration {
    pub fn new(ty: ty::Element, constant: bool) -> Self {
        Self {
            comment: None,
            linkage: Default::default(),
            visibility: Default::default(),
            address_significance: Default::default(),
            address_space: Default::default(),
            alignment: Default::default(),
            is_constant: constant,
            ty,
        }
    }

    pub fn new_global(ty: ty::Element) -> Self {
        Self::new(ty, false)
    }

    pub fn new_constant(ty: ty::Element) -> Self {
        Self::new(ty, true)
    }

    pub fn with_comment(mut self, comments: Option<String>) -> Self {
        self.comment = comments;
        self
    }

    pub fn with_linkage(mut self, linkage: Linkage) -> Self {
        self.linkage = linkage;
        self
    }

    pub fn with_visibility(mut self, visibility: Visibility) -> Self {
        self.visibility = visibility;
        self
    }

    pub fn with_address_significance(mut self, address_significance: AddressSignificance) -> Self {
        self.address_significance = Some(address_significance);
        self
    }

    pub fn with_address_space(mut self, address_space: AddressSpace) -> Self {
        self.address_space = address_space;
        self
    }

    pub fn with_alignment(mut self, alignment: Alignment) -> Self {
        self.alignment = alignment;
        self
    }
}

impl crate::FmtAsLlvmAsmMC for GlobalVarDeclaration {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
    ) -> fmt::Result {
        // For global variable *declarations*, the linkage must always be displayed since it is
        // used to distinguish between declaration and definition. The latter being the default.
        self.linkage.fmt_as_llvm_asm(f, opts, module)?;
        f.write_char(' ')?;

        if self.visibility != Visibility::default() {
            self.visibility.fmt_as_llvm_asm(f, opts, module)?;
            f.write_char(' ')?;
        }

        if let Some(address_significance) = self.address_significance {
            address_significance.fmt_as_llvm_asm(f, opts, module)?;
            f.write_char(' ')?;
        }

        if self.address_space != AddressSpace::default() {
            self.address_space.fmt_as_llvm_asm(f, opts, module)?;
            f.write_char(' ')?;
        }

        f.write_str(if self.is_constant {
            "constant "
        } else {
            "global "
        })?;

        self.ty.fmt_as_llvm_asm(f, opts, module)?;

        if self.alignment != Alignment::default() {
            f.write_str(", ")?;
            self.alignment.fmt_as_llvm_asm(f, opts, module)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct GlobalVarDefinition {
    pub comment: Option<String>,
    pub linkage: Linkage,
    pub visibility: Visibility,
    pub address_significance: Option<AddressSignificance>,
    pub address_space: AddressSpace,
    pub alignment: Alignment,
    pub is_constant: bool,
    pub value: value::constant::Element,
}

impl GlobalVarDefinition {
    pub fn new<V: constant::ElementConstant>(value: V, constant: bool) -> Self {
        Self {
            comment: None,
            linkage: Default::default(),
            visibility: Default::default(),
            address_significance: Default::default(),
            address_space: Default::default(),
            alignment: Default::default(),
            // metadata: Default::default(),
            is_constant: constant,
            value: value.into(),
        }
    }

    pub fn new_global<V: constant::ElementConstant>(value: V) -> Self {
        Self::new(value, false)
    }

    pub fn new_constant<V: constant::ElementConstant>(value: V) -> Self {
        Self::new(value, true)
    }

    pub fn ty(&self) -> ty::Element {
        self.value.ty()
    }

    pub fn with_comment(mut self, comments: Option<String>) -> Self {
        self.comment = comments;
        self
    }

    pub fn with_linkage(mut self, linkage: Linkage) -> Self {
        self.linkage = linkage;
        self
    }

    pub fn with_visibility(mut self, visibility: Visibility) -> Self {
        self.visibility = visibility;
        self
    }

    pub fn with_address_significance(mut self, address_significance: AddressSignificance) -> Self {
        self.address_significance = Some(address_significance);
        self
    }

    pub fn with_address_space(mut self, address_space: AddressSpace) -> Self {
        self.address_space = address_space;
        self
    }

    pub fn with_alignment(mut self, alignment: Alignment) -> Self {
        self.alignment = alignment;
        self
    }
}

impl crate::FmtAsLlvmAsmMC for GlobalVarDefinition {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
    ) -> fmt::Result {
        if self.linkage != Linkage::default() {
            self.linkage.fmt_as_llvm_asm(f, opts, module)?;
            f.write_char(' ')?;
        }

        if self.visibility != Visibility::default() {
            self.visibility.fmt_as_llvm_asm(f, opts, module)?;
            f.write_char(' ')?;
        }

        if let Some(address_significance) = self.address_significance {
            address_significance.fmt_as_llvm_asm(f, opts, module)?;
            f.write_char(' ')?;
        }

        if self.address_space != AddressSpace::default() {
            self.address_space.fmt_as_llvm_asm(f, opts, module)?;
            f.write_char(' ')?;
        }

        f.write_str(if self.is_constant {
            "constant "
        } else {
            "global "
        })?;

        self.value.ty().fmt_as_llvm_asm(f, opts, module)?;
        f.write_char(' ')?;
        self.value.fmt_as_llvm_asm(f, opts, module)?;

        if self.alignment != Alignment::default() {
            f.write_str(", ")?;
            self.alignment.fmt_as_llvm_asm(f, opts, module)?;
        }

        Ok(())
    }
}
