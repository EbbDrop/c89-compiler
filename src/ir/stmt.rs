use crate::diagnostic::Span;

use super::expr::ExprNode;

#[derive(Debug, Clone)]
pub struct Root {
    pub global: Block,
}

#[derive(Debug, Clone)]
pub struct Block(pub Vec<StmtNode>);

#[derive(Debug, Clone)]
pub struct StmtNode {
    pub span: Span,
    pub stmt: Stmt,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(ExprNode),
}
