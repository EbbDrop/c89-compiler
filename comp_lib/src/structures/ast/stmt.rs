use super::{Declaration, ExpressionNode, Span};

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatementNode {
    pub span: Span,
    pub stmts: Vec<StatementNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StatementNode {
    pub span: Span,
    pub data: Statement,
    pub comments: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Declaration(Declaration),
    Expression(ExpressionNode),
    If(IfStatement),
    Switch(SwitchStatement),
    While(WhileStatement),
    For(ForStatement),
    Break,
    Continue,
    Return(Span, Option<ExpressionNode>),
    BlockStatement(BlockStatementNode),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: ExpressionNode,
    pub if_body: BlockStatementNode,
    pub else_body: Option<BlockStatementNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SwitchStatement {
    pub expr: ExpressionNode,
    /// Guaranteed to have exactly 0 or 1 `Switch Case::Default` nodes. This can't be a seperate
    /// since the relative order is importand.
    pub cases: Vec<SwitchCase>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SwitchCase {
    Expr(SwithCaseExprNode),
    Default(SwitchCaseDefaultNode),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SwithCaseExprNode {
    pub label_span: Span,
    pub expr: ExpressionNode,
    pub body: BlockStatementNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SwitchCaseDefaultNode {
    pub label_span: Span,
    pub body: BlockStatementNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub condition: ExpressionNode,
    pub body: BlockStatementNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement {
    pub init: Option<Box<StatementNode>>,
    pub condition: Option<ExpressionNode>,
    pub iter: Option<ExpressionNode>,
    pub body: BlockStatementNode,
}
