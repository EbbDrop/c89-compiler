use super::{QualifiedTypeNode, Span};

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionNode {
    pub span: Span,
    pub data: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Assignment(
        Box<ExpressionNode>,
        AssignmentOperatorNode,
        Box<ExpressionNode>,
    ),
    Binary(Box<ExpressionNode>, BinaryOperatorNode, Box<ExpressionNode>),
    ArraySubscript(Box<ExpressionNode>, Box<ExpressionNode>),
    Unary(UnaryOperatorNode, Box<ExpressionNode>),
    Cast(QualifiedTypeNode, Box<ExpressionNode>),
    FunctionCall(FunctionCall),
    Literal(LiteralNode),
    Ident(IdentNode),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub ident: IdentNode,
    pub args: Vec<ExpressionNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralNode {
    pub span: Span,
    pub data: Literal,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Dec(i128),
    Hex(i128),
    Octal(i128),
    Char(u8),
    Float(f64),
    String(Vec<u8>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentOperatorNode {
    pub span: Span,
    // pub data: AssignmentOperator, TODO Allow for assignments like +=, -=, ...
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperatorNode {
    pub span: Span,
    pub data: BinaryOperator,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Star,
    Slash,
    Pipe,
    Caret,
    Ampersand,
    AngleLeft,
    AngleRight,
    DoubleEquals,
    DoubleAmpersand,
    DoublePipe,
    BangEquals,
    Percent,
    AngleLeftEquals,
    AngleRightEquals,
    DoubleAngleLeft,
    DoubleAngleRight,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOperatorNode {
    pub span: Span,
    pub data: UnaryOperator,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Bang,
    Plus,
    Minus,
    DoublePlusPrefix,
    DoubleMinusPrefix,
    DoublePlusPostfix,
    DoubleMinusPostfix,
    Tilde,
    Ampersand,
    Star,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentNode {
    pub span: Span,
    pub data: String,
}
