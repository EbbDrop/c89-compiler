mod basic_block;
mod builder;

pub use basic_block::*;
pub use builder::*;

use crate::convert::Into_;
use crate::id_store::{IdHandle, IdStore};
use crate::ty::{self, Type};
use crate::value::{self, Value};
use crate::{id, instruction};
use crate::{AddressSignificance, AddressSpace, Alignment, CallingConv, Linkage, Name, Visibility};
use std::fmt::{self, Write};
use std::ops::Deref;

/// The default function declaration returns void and doesn't have any parameters.
#[derive(Debug, Default, Clone)]
pub struct FunctionDeclaration {
    pub comment: Option<String>,
    pub linkage: Linkage,
    pub visibility: Visibility,
    pub address_significance: Option<AddressSignificance>,
    pub address_space: AddressSpace,
    pub alignment: Alignment,
    pub calling_conv: CallingConv,
    pub return_type: ReturnType,
    params: Vec<Param>,
    pub is_vararg: bool,
    symbols: IdStore<id::Local>,
}

impl FunctionDeclaration {
    pub fn new(return_type: ReturnType) -> Self {
        Self {
            return_type,
            ..Default::default()
        }
    }

    pub fn params(&self) -> &[Param] {
        &self.params
    }

    pub fn ty(&self) -> ty::Function {
        ty::Function::new_literal(self.return_type.clone())
            .with_params(self.params.iter().map(Value::ty))
            .with_vararg(self.is_vararg)
            .build()
    }

    pub fn with_comment(mut self, comment: Option<String>) -> Self {
        self.comment = comment;
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

    pub fn with_calling_conv(mut self, calling_conv: CallingConv) -> Self {
        self.calling_conv = calling_conv;
        self
    }

    pub fn with_vararg(mut self, is_vararg: bool) -> Self {
        self.is_vararg = is_vararg;
        self
    }

    pub fn add_param<T: ty::FirstClassType>(
        &mut self,
        ty: T,
        // attrs: ParamAttrs,
    ) -> value::Register<T> {
        let handle = self.create_unnamed_id_handle();
        let reg = value::Register { ty, handle };
        self.params.push(Param {
            // attrs,
            value: reg.clone().into_(),
        });
        reg
    }

    pub fn add_param_named(
        &mut self,
        name: Name,
        ty: ty::FirstClass,
        // attrs: ParamAttrs,
    ) -> Result<value::Register<ty::FirstClass>, String> {
        let handle = self.create_named_id_handle(name)?;
        let reg = value::Register { ty, handle };
        self.params.push(Param {
            // attrs,
            value: reg.clone(),
        });
        Ok(reg)
    }

    pub fn to_definition_builder(self) -> FunctionDefinitionBuilder {
        FunctionDefinitionBuilder::new(self)
    }

    pub(crate) fn fmt_as_llvm_asm_with_id(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
        function_id: &id::Global,
    ) -> fmt::Result {
        fmt_function_as_llvm_asm_with_id(self, f, opts, module, "declare", function_id)
    }

    pub(crate) fn id(&self, handle: &LocalIdHandle) -> &id::Local {
        self.symbols.id(&handle.0)
    }

    fn create_unnamed_id_handle(&mut self) -> LocalIdHandle {
        LocalIdHandle(self.symbols.create_unnamed_id_handle())
    }

    fn create_named_id_handle(&mut self, name: Name) -> Result<LocalIdHandle, String> {
        self.symbols
            .create_named_id_handle(name)
            .map(LocalIdHandle)
            .map_err(|name| format!("function local name {name:?} shouldn't already exist"))
    }

    fn update_unnamed_id(&mut self, handle: &LocalIdHandle) -> bool {
        self.symbols.update_unnamed_id(&handle.0)
    }

    #[deprecated = "implementation is not yet complete"]
    fn _remove_id(&mut self, handle: &mut LocalIdHandle) -> id::Local {
        let _removed_id = self.symbols.remove_id(&mut handle.0);
        // TODO: check all existing blocks and instructions for validity and patch if necessary
        todo!()
    }

    #[deprecated = "implementation is not yet complete"]
    fn _merge_ids(
        &mut self,
        handle1: &mut LocalIdHandle,
        handle2: &mut LocalIdHandle,
    ) -> Option<id::Local> {
        let _removed_id = self.symbols.merge_ids(&mut handle1.0, &mut handle2.0);
        // TODO: check all existing blocks and instructions for validity and patch if necessary
        todo!()
    }
}

impl crate::FmtAsLlvmAsmMC for FunctionDeclaration {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
    ) -> fmt::Result {
        let function_id = module.global_id(
            module
                .get_function_declaration_id_handle(self)
                .expect("this function declaration should be declared in the given module"),
        );
        self.fmt_as_llvm_asm_with_id(f, opts, module, function_id)
    }
}

/// The default function declaration returns void and doesn't have any parameters.
#[derive(Debug, Default)]
pub struct FunctionDefinition {
    declaration: FunctionDeclaration,
    body: Vec<BasicBlock>,
}

impl Deref for FunctionDefinition {
    type Target = FunctionDeclaration;

    fn deref(&self) -> &Self::Target {
        &self.declaration
    }
}

impl FunctionDefinition {
    fn new(declaration: FunctionDeclaration) -> Self {
        Self {
            declaration,
            body: Vec::new(),
        }
    }

    pub fn body(&self) -> &[BasicBlock] {
        &self.body
    }

    fn push_block(&mut self, block: BasicBlock) {
        self.update_unnamed_ids(&block);
        self.body.push(block)
    }

    fn update_unnamed_ids(&mut self, block: &BasicBlock) {
        self.declaration.update_unnamed_id(&block.label.handle);
        for instruction in &block.instructions {
            match instruction {
                instruction::Instruction::Yielding(result, _)
                | instruction::Instruction::MaybeYielding(Some(result), _) => {
                    self.declaration.update_unnamed_id(&result.handle);
                }
                instruction::Instruction::MaybeYielding(None, _)
                | instruction::Instruction::Void(_) => {}
            }
        }
    }

    pub(crate) fn fmt_as_llvm_asm_with_id(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
        function_id: &id::Global,
    ) -> fmt::Result {
        fmt_function_as_llvm_asm_with_id(
            &self.declaration,
            f,
            opts,
            module,
            "define",
            function_id,
        )?;
        f.write_str(" {\n")?;
        for block in &self.body {
            use crate::FmtAsLlvmAsmFC;
            block.fmt_as_llvm_asm(f, opts, module, &self.declaration)?;
        }
        f.write_char('}')
    }
}

impl crate::FmtAsLlvmAsmMC for FunctionDefinition {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
    ) -> fmt::Result {
        let function_id = module.global_id(
            module
                .get_function_definition_id_handle(self)
                .expect("this function definition should be defined in the given module"),
        );
        self.fmt_as_llvm_asm_with_id(f, opts, module, function_id)
    }
}

fn fmt_function_as_llvm_asm_with_id(
    function: &FunctionDeclaration,
    f: &mut fmt::Formatter,
    opts: &crate::FmtOpts,
    module: &crate::Module,
    declare_or_define: &'static str,
    function_id: &id::Global,
) -> fmt::Result {
    use crate::FmtAsLlvmAsmMC;

    if let Some(comment) = &function.comment {
        for line in comment.lines() {
            writeln!(f, ";{line}")?;
        }
    }

    write!(f, "{declare_or_define} ")?;

    if function.linkage != Linkage::default() {
        function.linkage.fmt_as_llvm_asm(f, opts, module)?;
        f.write_char(' ')?;
    }

    if function.visibility != Visibility::default() {
        function.visibility.fmt_as_llvm_asm(f, opts, module)?;
        f.write_char(' ')?;
    }

    if function.calling_conv != CallingConv::default() {
        function.calling_conv.fmt_as_llvm_asm(f, opts, module)?;
        f.write_char(' ')?;
    }

    function.return_type.fmt_as_llvm_asm(f, opts, module)?;
    f.write_char(' ')?;

    function_id.fmt_as_llvm_asm(f, opts, module)?;

    f.write_str(" (")?;
    {
        use crate::FmtAsLlvmAsmFC;
        if let [head, tail @ ..] = function.params.as_slice() {
            head.fmt_as_llvm_asm(f, opts, module, function)?;
            for param in tail {
                f.write_str(", ")?;
                param.fmt_as_llvm_asm(f, opts, module, function)?;
            }
        }
        if function.is_vararg {
            if !function.params.is_empty() {
                f.write_str(", ")?;
            }
            f.write_str("...")?;
        }
    }
    f.write_char(')')?;

    if let Some(address_significance) = function.address_significance {
        f.write_char(' ')?;
        address_significance.fmt_as_llvm_asm(f, opts, module)?;
    }

    if function.address_space != AddressSpace::default() {
        function.address_space.fmt_as_llvm_asm(f, opts, module)?;
        f.write_char(' ')?;
    }

    if function.alignment != Alignment::default() {
        f.write_char(' ')?;
        function.alignment.fmt_as_llvm_asm(f, opts, module)?;
    }

    Ok(())
}

#[derive(Debug, Clone)]
pub struct Param {
    // attrs: ParamAttrs,
    value: value::Register<ty::FirstClass>,
}

impl Param {
    pub fn value(&self) -> &value::Register<ty::FirstClass> {
        &self.value
    }
}

impl crate::FmtAsLlvmAsmFC for Param {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
        function: &crate::FunctionDeclaration,
    ) -> fmt::Result {
        self.value.ty().fmt_as_llvm_asm(f, opts, module, function)?;
        if opts.display_unnamed_ids || self.value.is_named(function) {
            f.write_char(' ')?;
            self.value.fmt_as_llvm_asm(f, opts, module, function)?;
        }
        Ok(())
    }
}

impl value::Value for Param {
    type Type = ty::FirstClass;

    fn ty(&self) -> Self::Type {
        self.value.ty()
    }
}

/// [`LocalIdHandle`]'s have interior mutability. They should not be used as keys in maps!
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct LocalIdHandle(IdHandle);

// Note that `ReturnType` is not a `ty::Type`! (because void is not a type)
#[derive(Debug, Clone, Default)]
pub enum ReturnType {
    #[default]
    Void,
    Element(ty::Element),
}

impl Type for ReturnType {
    fn equiv_to(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Void, Self::Void) => true,
            (Self::Element(e1), Self::Element(e2)) => e1.equiv_to(e2),
            _ => false,
        }
    }

    fn has_opaque_struct(&self) -> bool {
        match self {
            ReturnType::Void => false,
            ReturnType::Element(element) => element.has_opaque_struct(),
        }
    }

    fn has_scalable_vec(&self) -> bool {
        match self {
            ReturnType::Void => false,
            ReturnType::Element(element) => element.has_scalable_vec(),
        }
    }

    fn is_identified(&self) -> bool {
        match self {
            ReturnType::Void => false,
            ReturnType::Element(element) => element.is_identified(),
        }
    }
}

impl crate::FmtAsLlvmAsmMC for ReturnType {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
    ) -> fmt::Result {
        match self {
            ReturnType::Void => f.write_str("void"),
            ReturnType::Element(element) => element.fmt_as_llvm_asm(f, opts, module),
        }
    }
}

// Generic From<T> is possible here, because this is the only From implementation for ReturnType
impl<T: Into<ty::Element>> From<T> for ReturnType {
    fn from(value: T) -> Self {
        Self::Element(value.into())
    }
}
