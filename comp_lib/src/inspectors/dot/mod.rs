mod dot_tree;
mod inspect_ast;

use crate::ast;
use dot_tree::DotTree;

trait ToDot {
    fn to_dot(&self) -> DotTree;
}

pub fn inspect_ast(ast: &ast::Ast) -> String {
    ast.to_dot().to_string()
}
