use super::{ctype::CType, table::ItemId};
use crate::diagnostic::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct ExprNode {
    pub span: Span,
    // The type the expr returns
    pub ty: CType,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Not from the standart but will just be pointer derefrence on almost all platforms
    LvalueDeref(Box<LvalueExprNode>),
    /// 3.3.1
    Constant(Constant),

    // 3.3.1
    // StringLiteral

    // 3.3.2.1
    // Array subscripting can already be converted to a Add and Deref

    // 3.3.2.2
    // first expr should have type functionPointer
    // FunctionCall(ExprNode, Vec<ExprNode>)

    // 3.3.2.3
    // . and ->

    //
    /// 3.3.2.4
    /// Inner expr will be be a scalar type, The out type will be the same type
    PostfixInc(Box<LvalueExprNode>),
    /// 3.3.2.4
    /// Inner expr will be be a scalar type, The out type will be the same type
    PostfixDec(Box<LvalueExprNode>),

    /// 3.3.3.1
    /// Inner expr will be be a scalar type, The out type will be the same type
    PrefixInc(Box<LvalueExprNode>),
    // 3.3.3.1
    /// Inner expr will be be a scalar type, The out type will be the same type
    PrefixDec(Box<LvalueExprNode>),

    //
    /// 3.3.3.2
    /// Inner expr will be any non bit-field, non register class type, The out type will be a
    /// Pointer version of that type
    Reference(Box<LvalueExprNode>),

    /// 3.3.3.3
    /// Iner expr will have a arithmetic type, the out type is the same type but promoted
    UnaryArith(UnaryOp, Box<ExprNode>),

    /// 3.3.5 - 3.3.12
    /// See [`BinaryOp`] for type details. This does not include logical && nor ||, since those
    /// need to short circuiting.
    Binary(Box<ExprNode>, BinaryOp, Box<ExprNode>),

    /// 3.3.8
    /// Either
    /// * Each operand will be the same arithmetic type,
    /// * Or Both arguments are pointers to compatible types
    /// In both cases the out type will be int
    ///
    /// (Void pointers will already be converted to real pointers)
    Relation(Box<ExprNode>, RelationOp, Box<ExprNode>),

    /// 3.3.13
    /// Each operand will have a scalar type, the out type is always int
    LogicalAnd(Box<ExprNode>, Box<ExprNode>),
    /// 3.3.14
    /// Each operand will have a scalar type, the out type is always int
    LogicalOr(Box<ExprNode>, Box<ExprNode>),

    // 3.3.14
    // Ternary

    //
    /// 3.3.16
    /// (The rules here are more lenient than the standard to follow g++)
    ///
    /// Both sides will be the exact same type (cast's are inserted on the right)
    Assign(Box<LvalueExprNode>, Box<ExprNode>),

    // 3.3.17
    // Comma(Box<Expr>, Box<Expr>)

    //
    /// 3.3.4
    /// Implicit or explicit cast
    Cast(Box<ExprNode>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    /// Iner expr should have a promoted arithmetic type, the output is the same promoted type
    Neg,
    /// Inner expr should have a promoted integer type, the output is the same promoted type
    BitNot,
    /// Inner expr should have a scalar type, the output is the is always a int
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    /// 3.3.5
    /// Each operand and the out type will be the same arithmetic type
    Mul,
    /// 3.3.5
    /// Each operand and the out type will be the same arithmetic type
    Div,
    /// 3.3.5
    /// Each operand and the out type will be the same integral type
    Rem,
    /// 3.3.6
    /// Either
    /// * Each operand and the out type will be the same arithmetic type,
    /// * Or one operand will be a pointer and the other will be a integral type
    /// the out type is than the same type as the pointer
    Add,
    /// 3.3.6
    /// Either
    /// * Each operand and the out type will be the same arithmetic type,
    /// * Or Both arguments are pointers to compatible types, the out type will be a integral type
    /// * Or the left operand will be a pointer and the right will be a integral type
    /// the out type is than the same type as the pointer
    Sub,

    /// 3.3.7
    /// Each operand and to out type will be the same integral type
    ShiftLeft,
    /// 3.3.7
    /// Each operand and to out type will be the same integral type
    ShiftRight,

    /// 3.3.8
    /// Each operand and to out type will be the same integral type
    Bitwise(BitwiseOp),
}

impl BinaryOp {
    pub fn long_name(&self) -> &'static str {
        match self {
            BinaryOp::Mul => "multiplication",
            BinaryOp::Div => "division",
            BinaryOp::Rem => "modulus",
            BinaryOp::Add => "addition",
            BinaryOp::Sub => "subtraction",
            BinaryOp::ShiftLeft => "left shift",
            BinaryOp::ShiftRight => "right shift",
            BinaryOp::Bitwise(b) => b.long_name(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RelationOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Ge,
    Le,
}

impl RelationOp {
    pub fn long_name(&self) -> &'static str {
        match self {
            RelationOp::Eq => "equality",
            RelationOp::Ne | RelationOp::Lt | RelationOp::Gt | RelationOp::Ge | RelationOp::Le => {
                "comparison"
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BitwiseOp {
    And,
    Or,
    Xor,
}

impl BitwiseOp {
    pub fn long_name(&self) -> &'static str {
        match self {
            BitwiseOp::And => "bitwise and",
            BitwiseOp::Or => "bitwise or",
            BitwiseOp::Xor => "bitwise xor",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LvalueExprNode {
    pub span: Span,
    pub is_const: bool,
    // The type the expr returns
    pub ty: CType,
    pub expr: LvalueExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LvalueExpr {
    /// 3.3.1
    Ident(ItemId),

    /// 3.3.3.2
    /// Inner expr will be pointer type, the out type will be the type that was pointed to
    Dereference(Box<ExprNode>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Integer(i128),
    Float(f64),
}
