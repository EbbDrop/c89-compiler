use crate::diagnostic::Span;

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub struct Ast {
    pub global: BlockStatement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement(pub Vec<StatementNode>);

#[derive(Debug, Clone, PartialEq)]
pub struct StatementNode {
    pub span: Span,
    pub data: Statement,
    pub comments: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Declaration {
        type_name: QualifiedTypeNode,
        ident: IdentNode,
        initializer: Option<(Span, ExpressionNode)>,
    },
    Expression(ExpressionNode),
    Printf(ExpressionNode),
    BlockStatement(BlockStatement),
}

#[derive(Debug, Clone, PartialEq)]
pub struct QualifiedTypeNode {
    pub span: Span,
    pub data: QualifiedType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct QualifiedType {
    pub is_const: Option<Span>,
    // pub is_volitile: bool,
    // pub is_restrict: bool,
    pub inner: UnqualifiedTypeNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnqualifiedTypeNode {
    pub span: Span,
    pub data: UnqualifiedType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnqualifiedType {
    PointerType(Box<QualifiedTypeNode>),
    // ArrayType,
    // FunctionType,
    PlainType(PlainType),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PlainType {
    Primitive(PrimitiveType),
    // StructType(String),
    // EnumType(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    Char,
    Int,
    Float,
}

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
    Unary(UnaryOperatorNode, Box<ExpressionNode>),
    Cast(QualifiedTypeNode, Box<ExpressionNode>),
    Literal(LiteralNode),
    Ident(IdentNode),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralNode {
    pub span: Span,
    pub data: Literal,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Literal {
    Dec(i128),
    Hex(i128),
    Octal(i128),
    Char(i128),
    Float(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Integer(i128), //TODO change this to big int?
    Float(f64),
    // Void,
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
