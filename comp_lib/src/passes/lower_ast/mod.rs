mod expr;
mod ext_decl;
mod stmt;
mod symbol_table;
mod type_checking;
mod util;

use std::collections::HashMap;

use crate::diagnostic::AggregateResult;
use crate::settings::Settings;
use crate::{ast, ir};
use ext_decl::build_ir_from_external_declaration;

pub fn build_ir_from_ast(ast: &ast::Ast, settings: &Settings) -> AggregateResult<ir::Root> {
    let mut root = ir::Root {
        vars: HashMap::new(),
        functions: HashMap::new(),
    };

    let mut res = AggregateResult::new_ok(());

    for ext_decl in &ast.global_declarations {
        res = res.aggregate(build_ir_from_external_declaration(
            ext_decl, &mut root, settings,
        ));
    }

    res.map(|_| root)
}
