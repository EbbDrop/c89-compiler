use crate::id::{GlobalName, GlobalNameId, Name};
use crate::{FunctionDefinition, GlobalVarDefinition};
use std::collections::HashSet;

#[derive(Debug)]
pub struct Module {
    name: Name,
    source_filename: String,
    global_vars: Vec<GlobalVarDefinition>,
    functions: Vec<FunctionDefinition>,
    symbols: HashSet<GlobalNameId>,
}

impl Module {
    pub fn new(name: Name, source_filename: String) -> Self {
        Self {
            name,
            source_filename,
            global_vars: Vec::new(),
            functions: Vec::new(),
            symbols: HashSet::new(),
        }
    }

    pub fn add_global_identifier(&mut self, name: Name) -> crate::Result<GlobalName> {
        let id = GlobalNameId::new(name);
        let inserted = self.symbols.insert(id.clone());
        match inserted {
            true => Ok(GlobalName(id)),
            false => Err(crate::Error),
        }
    }

    pub fn define_global_var(&mut self, global_var: GlobalVarDefinition) {
        self.global_vars.push(global_var);
    }

    pub fn define_function(&mut self, function: FunctionDefinition) {
        self.functions.push(function);
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "; module = {}", self.name)?;
        // TODO: escape filename string
        writeln!(f, "source_filename = \"{}\"", self.source_filename)?;
        // TEMP: hard-coded printf
        writeln!(
            f,
            r#"
@.printf_int = private unnamed_addr constant [4 x i8] c"%i\0a\00"
@.printf_float = private unnamed_addr constant [4 x i8] c"%f\0a\00"
@.printf_ptr = private unnamed_addr constant [4 x i8] c"%p\0a\00"

declare i32 @printf(ptr noalias nocapture, ...)"#
        )?;
        for global_var in &self.global_vars {
            writeln!(f, "\n{global_var}")?;
        }
        for function in &self.functions {
            writeln!(f, "\n{function}")?;
        }
        std::fmt::Result::Ok(())
    }
}
