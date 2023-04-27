use crate::diagnostic::Span;

use super::{expr::ExprNode, table::Table};

#[derive(Debug, Clone)]
pub struct Root {
    pub global: BlockNode,
    pub table: Table,
}

#[derive(Debug, Clone)]
pub struct BlockNode {
    pub span: Span,
    pub stmts: Vec<StmtNode>,
}

#[derive(Debug, Clone)]
pub struct StmtNode {
    pub comments: Option<String>,
    pub span: Span,
    pub stmt: Stmt,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(ExprNode),
    IfStmt(IfStmtNode),
    SwitchStmt(SwitchStmtNode),
    LoopStmt(LoopStmtNode),
    Printf(ExprNode),
    Break,
    Continue,
    Return(Option<ExprNode>),
}

/// To model the if and if-else "selection statements" from the C standard.
#[derive(Debug, Clone)]
pub struct IfStmtNode {
    pub span: Span,
    /// Controlling expression. Has scalar type.
    ///
    /// # C89 standard
    ///
    /// > The controlling expression of an if statement shall have scalar type.
    pub condition: ExprNode,
    pub if_branch: BlockNode,
    pub else_branch: Option<BlockNode>,
}

#[derive(Debug, Clone)]
pub struct SwitchStmtNode {
    pub span: Span,
    /// Controlling expression. Has integral type.
    ///
    /// # C89 standard
    ///
    /// > The controlling expression of a switch statement shall have integral type.
    /// > The expression of each case label shall be an integral constant expression.
    /// > No two of the case constant expressions in the same switch statement shall have the same
    /// > value after conversion. There may be at most one default label in a switch statement.
    /// > (Any enclosed switch statement may have a default label or case constant expressions with
    /// > values that duplicate case constant expressions in the enclosing switch statement.)
    pub expr: ExprNode,
    /// Consists of all 'regular' cases and at most one default case.
    pub cases: Vec<SwitchStmtCaseNode>,
    /// Whether the cases contains a default case.
    pub has_default: bool,
}

#[derive(Debug, Clone)]
pub struct SwitchStmtCaseNode {
    pub span: Span,
    pub data: SwitchStmtCase,
}

#[derive(Debug, Clone)]
pub enum SwitchStmtCase {
    Case {
        /// Shall be an integral constant expression.
        label: i128,
        body: BlockNode,
    },
    Default {
        body: BlockNode,
    },
}

/// To model the "iteration statements" from the C standard. This includes for statements, while
/// statements, and in the future possibly do statements.
#[derive(Debug, Clone)]
pub struct LoopStmtNode {
    pub span: Span,
    /// Controlling expression.
    pub condition: Option<ExprNode>,
    /// Loop body.
    pub body: BlockNode,
    /// Optional continuation part of a for statement.
    pub continuation: Option<ExprNode>,
}
