use super::{
    ctype::CType,
    expr::{Constant, ExprNode},
    table::{ItemId, Table, VariableItem},
};
use crate::diagnostic::Span;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Root {
    pub vars: HashMap<String, GlobalVarNode>,
    pub functions: HashMap<String, FunctionNode>,
}

#[derive(Debug, Clone)]
pub struct GlobalVarNode {
    /// The span where this global variable was declared/defined.
    pub original_span: Span,
    pub comments: Option<String>,
    /// The type.
    pub ty: CType,
    /// Defined as const.
    pub is_const: bool,
    /// `Some(<constant value>)` if the global was initialized.
    pub value: Option<Constant>,
}

#[derive(Debug, Clone)]
pub struct FunctionNode {
    /// The span where this function was first declared or defined.
    pub original_span: Span,
    pub comments: Option<String>,
    pub return_type: CType,
    pub params: Vec<FunctionParamNode>,
    pub is_vararg: bool,
    pub body: Option<BlockNode>,
    pub table: Table<VariableItem>,
}

impl FunctionNode {
    pub fn is_declaration(&self) -> bool {
        self.body.is_none()
    }
}

// One might think a single `ItemId` would be sufficient for this, since the type and other metadata
// can be retrieved from the symbol table. However, it is valid to declare params without an
// identifier, only with a type. These params are still important since they affect the signature of
// the function. Therefore the type (and other data that can also be found in `LvalueExprNode`s) are
// stored here as well, with the `ident` made optional.
#[derive(Debug, Clone)]
pub struct FunctionParamNode {
    pub span: Span,
    pub is_const: bool,
    pub ty: CType,
    pub ident: Option<ItemId>,
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
    /// Controlling expression. Can be `None` for e.g. `for (;;)`.
    pub condition: Option<ExprNode>,
    /// Loop body.
    pub body: BlockNode,
    /// Optional continuation part of a for statement.
    pub continuation: Option<ExprNode>,
}
