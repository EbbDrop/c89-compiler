mod dot_tree;
mod inspect_ast;
mod inspect_ir;

use crate::{ast, ir};
use dot_tree::DotTree;

trait ToDot {
    fn to_dot(&self) -> DotTree;
}

pub fn inspect_ast(ast: &ast::Ast) -> String {
    ast.to_dot().to_string()
}

pub fn inspect_ir(ir: &ir::Root) -> String {
    ir.to_dot().to_string()
}
