use crate::id::{GlobalName, GlobalNameId};
use crate::TypedValue;
use std::fmt;

#[derive(Debug, Clone)]
pub struct GlobalVarDefinition {
    name: GlobalNameId,
    // Since this is a definition, both a type and value are needed
    tyval: TypedValue,
    is_constant: bool,
}

impl fmt::Display for GlobalVarDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            name,
            is_constant,
            tyval,
        } = self;
        write!(
            f,
            "@{name} = private unnamed_addr {} {tyval}",
            if *is_constant { "constant" } else { "global" },
        )
    }
}

#[derive(Debug, Clone)]
pub struct GlobalVarDefinitionBuilder {
    definition: GlobalVarDefinition,
}

impl GlobalVarDefinitionBuilder {
    pub fn new(name: GlobalName, tyval: TypedValue) -> Self {
        Self {
            definition: GlobalVarDefinition {
                name: name.0,
                tyval,
                is_constant: false,
            },
        }
    }

    pub fn constant(&mut self, is_constant: bool) -> &mut Self {
        self.definition.is_constant = is_constant;
        self
    }

    pub fn build(self) -> GlobalVarDefinition {
        self.definition
    }
}
