use crate::id_store::{IdHandle, IdStore};
use crate::{id, ty, value};
use crate::{
    FunctionDeclaration, FunctionDefinition, GlobalVarDeclaration, GlobalVarDefinition, Name,
};
use std::fmt;

#[derive(Debug, Clone)]
pub struct Display<'a> {
    module: &'a Module,
    opts: crate::FmtOpts,
}

impl fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use crate::FmtAsLlvmAsm;
        self.module.fmt_as_llvm_asm(f, &self.opts)
    }
}

// TODO: disallow names with dashes or spaces
#[derive(Debug)]
pub struct TargetTriple {
    pub architecture: String,
    pub vendor: String,
    pub operating_system: String,
    pub environment: Option<String>,
}

impl fmt::Display for TargetTriple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}-{}-{}",
            self.architecture, self.vendor, self.operating_system
        )?;
        if let Some(environment) = &self.environment {
            write!(f, "-{environment}")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Module {
    name: String,
    source_filename: Option<String>,
    target_triple: Option<TargetTriple>,
    global_var_declarations: Vec<(GlobalIdHandle, GlobalVarDeclaration)>,
    global_var_definitions: Vec<(GlobalIdHandle, GlobalVarDefinition)>,
    // A `None` value means the declaration has been replaced by a definition.
    function_declarations: Vec<Option<(GlobalIdHandle, FunctionDeclaration)>>,
    function_definitions: Vec<(GlobalIdHandle, FunctionDefinition)>,
    // A `None` value means the type is only declared (i.e. opaque struct), or the declaration has
    // been replaced by a definition and the item is no longer in use.
    types: Vec<Option<(LocalIdHandle, ty::Any)>>,
    global_symbols: IdStore<id::Global>,
    local_symbols: IdStore<id::Local>,
}

impl Module {
    pub fn new(name: String) -> Self {
        Self {
            name,
            source_filename: None,
            target_triple: None,
            global_var_declarations: Vec::new(),
            global_var_definitions: Vec::new(),
            function_declarations: Vec::new(),
            function_definitions: Vec::new(),
            types: Vec::new(),
            global_symbols: IdStore::new(),
            local_symbols: IdStore::new(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn source_filename(&self) -> Option<&str> {
        self.source_filename.as_deref()
    }

    pub fn set_source_filename(&mut self, source_filename: String) {
        self.source_filename = Some(source_filename);
    }

    pub fn target_triple(&self) -> Option<&TargetTriple> {
        self.target_triple.as_ref()
    }

    pub fn set_target_triple(&mut self, target_triple: TargetTriple) {
        self.target_triple = Some(target_triple);
    }

    pub fn get_global_var_declaration(
        &self,
        handle: GlobalVarDeclarationHandle,
    ) -> &GlobalVarDeclaration {
        &self.global_var_declarations.get(handle.0).unwrap().1
    }

    pub fn get_global_var_definition(
        &self,
        handle: GlobalVarDefinitionHandle,
    ) -> &GlobalVarDefinition {
        &self.global_var_definitions.get(handle.0).unwrap().1
    }

    pub fn get_function_declaration(
        &self,
        handle: FunctionDeclarationHandle,
    ) -> &FunctionDeclaration {
        &self
            .function_declarations
            .get(handle.0)
            .unwrap()
            .as_ref()
            .unwrap()
            .1
    }

    pub fn get_function_definition(&self, handle: FunctionDefinitionHandle) -> &FunctionDefinition {
        &self.function_definitions.get(handle.0).unwrap().1
    }

    pub fn declare_type(&mut self) -> (TypeDeclarationHandle, ty::Identified<ty::Structure>) {
        let handle = self.create_local_unnamed_id_handle();
        let ty = ty::Structure::new_opaque(handle);
        let index = self.types.len();
        self.types.push(Some((handle, ty.clone().into())));
        (TypeDeclarationHandle(index), ty)
    }

    pub fn declare_type_named(
        &mut self,
        name: Name,
    ) -> Result<(TypeDeclarationHandle, ty::Identified<ty::Structure>), String> {
        let handle = self.create_local_named_id_handle(name)?;
        let ty = ty::Structure::new_opaque(handle);
        let index = self.types.len();
        self.types.push(Some((handle, ty.clone().into())));
        Ok((TypeDeclarationHandle(index), ty))
    }

    pub fn define_type<T: ty::FirstClassType>(&mut self, ty: T) -> ty::Identified<T> {
        let handle = self.create_local_unnamed_id_handle();
        self.types.push(Some((handle, ty.clone().into())));
        ty::Identified { ty, handle }
    }

    pub fn define_type_named<T: ty::FirstClassType>(
        &mut self,
        name: Name,
        ty: T,
    ) -> Result<ty::Identified<T>, String> {
        let handle = self.create_local_named_id_handle(name)?;
        self.types.push(Some((handle, ty.clone().into())));
        Ok(ty::Identified { ty, handle })
    }

    /// NOTE: the passed `new_ty` must be a *literal* structure type
    pub fn map_type_declaration_to_definition<T: ty::StructureType>(
        &mut self,
        handle: TypeDeclarationHandle,
        new_ty: T,
    ) -> Result<ty::Identified<T>, &'static str> {
        if new_ty.is_identified() {
            return Err("forward declaring type aliases is not allowed");
        }
        let (id_handle, _) = self.types[handle.0].take().unwrap();
        self.update_local_unnamed_id(id_handle);
        self.types.push(Some((id_handle, new_ty.clone().into())));
        Ok(ty::Identified {
            ty: new_ty,
            handle: id_handle,
        })
    }

    pub fn declare_global_var(
        &mut self,
        global_var_declaration: GlobalVarDeclaration,
    ) -> (GlobalVarDeclarationHandle, value::constant::Pointer) {
        let address_space = global_var_declaration.address_space;

        let index = self.global_var_declarations.len();
        let handle = self.create_global_unnamed_id_handle();
        self.global_var_declarations
            .push((handle, global_var_declaration));

        (
            GlobalVarDeclarationHandle(index),
            value::constant::Pointer::from_global_address(
                ty::Pointer::new_literal()
                    .with_address_space(address_space)
                    .build(),
                handle,
            ),
        )
    }

    pub fn declare_global_var_named(
        &mut self,
        name: Name,
        global_var_declaration: GlobalVarDeclaration,
    ) -> Result<(GlobalVarDeclarationHandle, value::constant::Pointer), String> {
        let address_space = global_var_declaration.address_space;

        let index = self.global_var_declarations.len();
        let handle = self.create_global_named_id_handle(name)?;
        self.global_var_declarations
            .push((handle, global_var_declaration));

        Ok((
            GlobalVarDeclarationHandle(index),
            value::constant::Pointer::from_global_address(
                ty::Pointer::new_literal()
                    .with_address_space(address_space)
                    .build(),
                handle,
            ),
        ))
    }

    pub fn define_global_var(
        &mut self,
        global_var_definition: GlobalVarDefinition,
    ) -> (GlobalVarDefinitionHandle, value::constant::Pointer) {
        let address_space = global_var_definition.address_space;

        let index = self.global_var_definitions.len();
        let handle = self.create_global_unnamed_id_handle();
        self.global_var_definitions
            .push((handle, global_var_definition));

        (
            GlobalVarDefinitionHandle(index),
            value::constant::Pointer::from_global_address(
                ty::Pointer::new_literal()
                    .with_address_space(address_space)
                    .build(),
                handle,
            ),
        )
    }

    pub fn define_global_var_named(
        &mut self,
        name: Name,
        global_var_definition: GlobalVarDefinition,
    ) -> Result<(GlobalVarDefinitionHandle, value::constant::Pointer), String> {
        let address_space = global_var_definition.address_space;

        let index = self.global_var_definitions.len();
        let handle = self.create_global_named_id_handle(name)?;
        self.global_var_definitions
            .push((handle, global_var_definition));

        Ok((
            GlobalVarDefinitionHandle(index),
            value::constant::Pointer::from_global_address(
                ty::Pointer::new_literal()
                    .with_address_space(address_space)
                    .build(),
                handle,
            ),
        ))
    }

    pub fn declare_function(
        &mut self,
        function_declaration: FunctionDeclaration,
    ) -> (FunctionDeclarationHandle, value::constant::Pointer) {
        let address_space = function_declaration.address_space;

        let index = self.function_declarations.len();
        let handle = self.create_global_unnamed_id_handle();
        self.function_declarations
            .push(Some((handle, function_declaration)));

        (
            FunctionDeclarationHandle(index),
            value::constant::Pointer::from_global_address(
                ty::Pointer::new_literal()
                    .with_address_space(address_space)
                    .build(),
                handle,
            ),
        )
    }

    pub fn declare_function_named(
        &mut self,
        name: Name,
        function_declaration: FunctionDeclaration,
    ) -> Result<(FunctionDeclarationHandle, value::constant::Pointer), String> {
        let address_space = function_declaration.address_space;

        let index = self.function_declarations.len();
        let handle = self.create_global_named_id_handle(name)?;
        self.function_declarations
            .push(Some((handle, function_declaration)));

        Ok((
            FunctionDeclarationHandle(index),
            value::constant::Pointer::from_global_address(
                ty::Pointer::new_literal()
                    .with_address_space(address_space)
                    .build(),
                handle,
            ),
        ))
    }

    pub fn define_function(
        &mut self,
        function_definition: FunctionDefinition,
    ) -> (FunctionDefinitionHandle, value::constant::Pointer) {
        let address_space = function_definition.address_space;

        let index = self.function_definitions.len();
        let handle = self.create_global_unnamed_id_handle();
        self.function_definitions
            .push((handle, function_definition));

        (
            FunctionDefinitionHandle(index),
            value::constant::Pointer::from_global_address(
                ty::Pointer::new_literal()
                    .with_address_space(address_space)
                    .build(),
                handle,
            ),
        )
    }

    pub fn define_function_named(
        &mut self,
        name: Name,
        function_definition: FunctionDefinition,
    ) -> Result<(FunctionDefinitionHandle, value::constant::Pointer), String> {
        let address_space = function_definition.address_space;

        let index = self.function_definitions.len();
        let handle = self.create_global_named_id_handle(name)?;
        self.function_definitions
            .push((handle, function_definition));

        Ok((
            FunctionDefinitionHandle(index),
            value::constant::Pointer::from_global_address(
                ty::Pointer::new_literal()
                    .with_address_space(address_space)
                    .build(),
                handle,
            ),
        ))
    }

    pub fn map_function_declaration_to_definition<F>(
        &mut self,
        handle: FunctionDeclarationHandle,
        f: F,
    ) -> FunctionDefinitionHandle
    where
        F: FnOnce(FunctionDeclaration) -> FunctionDefinition,
    {
        let (id_handle, declaration) = self
            .function_declarations
            .get_mut(handle.0)
            .unwrap()
            .take()
            .unwrap();
        self.update_global_unnamed_id(id_handle);
        let definition = f(declaration);
        let index = self.function_definitions.len();
        self.function_definitions.push((id_handle, definition));
        FunctionDefinitionHandle(index)
    }

    pub fn display(&self, opts: crate::FmtOpts) -> Display {
        Display { module: self, opts }
    }

    pub(crate) fn _get_global_var_declaration_id_handle(
        &self,
        global_var_declaration: &GlobalVarDeclaration,
    ) -> Option<GlobalIdHandle> {
        self.global_var_declarations
            .iter()
            .find_map(|(handle, gvd)| std::ptr::eq(gvd, global_var_declaration).then_some(*handle))
    }

    pub(crate) fn _get_global_var_definition_id_handle(
        &self,
        global_var_definition: &GlobalVarDefinition,
    ) -> Option<GlobalIdHandle> {
        self.global_var_definitions
            .iter()
            .find_map(|(handle, gvd)| std::ptr::eq(gvd, global_var_definition).then_some(*handle))
    }

    pub(crate) fn get_function_declaration_id_handle(
        &self,
        function_declaration: &FunctionDeclaration,
    ) -> Option<GlobalIdHandle> {
        self.function_declarations.iter().find_map(|item| {
            item.as_ref()
                .and_then(|(handle, fd)| std::ptr::eq(fd, function_declaration).then_some(*handle))
        })
    }

    pub(crate) fn get_function_definition_id_handle(
        &self,
        function_definition: &FunctionDefinition,
    ) -> Option<GlobalIdHandle> {
        self.function_definitions
            .iter()
            .find_map(|(handle, fd)| std::ptr::eq(fd, function_definition).then_some(*handle))
    }

    pub(crate) fn global_id(&self, handle: GlobalIdHandle) -> &id::Global {
        self.global_symbols.id(handle.0)
    }

    pub(crate) fn local_id(&self, handle: LocalIdHandle) -> &id::Local {
        self.local_symbols.id(handle.0)
    }

    fn create_global_unnamed_id_handle(&mut self) -> GlobalIdHandle {
        GlobalIdHandle(self.global_symbols.create_unnamed_id_handle())
    }

    fn create_global_named_id_handle(&mut self, name: Name) -> Result<GlobalIdHandle, String> {
        self.global_symbols
            .create_named_id_handle(name)
            .map(GlobalIdHandle)
            .map_err(|name| format!("global name {name:?} shouldn't already exist"))
    }

    fn create_local_unnamed_id_handle(&mut self) -> LocalIdHandle {
        LocalIdHandle(self.local_symbols.create_unnamed_id_handle())
    }

    fn create_local_named_id_handle(&mut self, name: Name) -> Result<LocalIdHandle, String> {
        self.local_symbols
            .create_named_id_handle(name)
            .map(LocalIdHandle)
            .map_err(|name| format!("module local name {name:?} shouldn't already exist"))
    }

    fn update_global_unnamed_id(&mut self, handle: GlobalIdHandle) -> bool {
        self.global_symbols.update_unnamed_id(handle.0)
    }

    fn update_local_unnamed_id(&mut self, handle: LocalIdHandle) -> bool {
        self.local_symbols.update_unnamed_id(handle.0)
    }
}

impl crate::FmtAsLlvmAsm for Module {
    fn fmt_as_llvm_asm(&self, f: &mut fmt::Formatter, opts: &crate::FmtOpts) -> fmt::Result {
        writeln!(f, "; module = {}", self.name)?;
        if let Some(source_filename) = &self.source_filename {
            write!(f, "source_filename = ")?;
            crate::fmt_as_llvm_quoted_string(f, source_filename.bytes())?;
            writeln!(f)?;
        }
        if let Some(target_triple) = &self.target_triple {
            write!(f, "target triple = ")?;
            crate::fmt_as_llvm_quoted_string(f, target_triple.to_string().bytes())?;
            writeln!(f)?;
        }

        for (id_handle, ty) in self.types.iter().flatten() {
            writeln!(f)?;
            self.local_id(*id_handle).fmt_as_llvm_asm(f, opts)?;
            f.write_str(" = type ")?;
            {
                use crate::FmtAsLlvmAsmMC;
                ty.fmt_as_llvm_asm(f, opts, self)?;
            }
            writeln!(f)?;
        }

        for (id_handle, gvd) in &self.global_var_declarations {
            writeln!(f)?;
            self.global_id(*id_handle).fmt_as_llvm_asm(f, opts)?;
            f.write_str(" = ")?;
            {
                use crate::FmtAsLlvmAsmMC;
                gvd.fmt_as_llvm_asm(f, opts, self)?;
            }
            writeln!(f)?;
        }

        for (id_handle, gvd) in &self.global_var_definitions {
            writeln!(f)?;
            self.global_id(*id_handle).fmt_as_llvm_asm(f, opts)?;
            f.write_str(" = ")?;
            {
                use crate::FmtAsLlvmAsmMC;
                gvd.fmt_as_llvm_asm(f, opts, self)?;
            }
            writeln!(f)?;
        }

        for (id_handle, fd) in self.function_declarations.iter().flatten() {
            writeln!(f)?;
            fd.fmt_as_llvm_asm_with_id(f, opts, self, self.global_id(*id_handle))?;
            writeln!(f)?;
        }

        for (id_handle, fd) in &self.function_definitions {
            writeln!(f)?;
            fd.fmt_as_llvm_asm_with_id(f, opts, self, self.global_id(*id_handle))?;
            writeln!(f)?;
        }

        Ok(())
    }
}

// struct NamedMetadata;

// TODO: add some way to link this to the module instance that created it
#[derive(Debug, Clone, Copy)]
pub struct GlobalVarDeclarationHandle(usize);
#[derive(Debug, Clone, Copy)]
pub struct GlobalVarDefinitionHandle(usize);
#[derive(Debug, Clone, Copy)]
pub struct FunctionDeclarationHandle(usize);
#[derive(Debug, Clone, Copy)]
pub struct FunctionDefinitionHandle(usize);

#[derive(Debug, Clone, Copy)]
pub struct TypeDeclarationHandle(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct GlobalIdHandle(IdHandle);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct LocalIdHandle(IdHandle);
