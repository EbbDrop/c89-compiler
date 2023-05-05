mod decl;
mod expr;
mod func;
mod stmt;
mod ty;

pub use decl::*;
pub use expr::*;
pub use func::*;
pub use stmt::*;
pub use ty::*;

use crate::diagnostic::Span;

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub struct Ast {
    pub global_declarations: Vec<ExternalDeclarationNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternalDeclarationNode {
    pub span: Span,
    pub data: ExternalDeclaration,
    pub comments: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExternalDeclaration {
    FunctionDefinition(FunctionDefinition),
    Declaration(Declaration),
}
