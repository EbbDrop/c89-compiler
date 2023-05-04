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

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub return_type: QualifiedTypeNode,
    pub ident: IdentNode,
    pub params: Vec<FunctionParamNode>,
    pub is_vararg: bool,
    pub body: BlockStatementNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub return_type: QualifiedTypeNode,
    pub ident: IdentNode,
    pub params: Vec<FunctionParamNode>,
    pub is_vararg: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParamNode {
    pub span: Span,
    pub type_name: QualifiedTypeNode,
    pub ident: Option<IdentNode>,
    pub array_parts: Vec<ArrayDeclarationNode>,
}

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
pub enum Declaration {
    Variable(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub type_name: QualifiedTypeNode,
    pub ident: IdentNode,
    pub array_parts: Vec<ArrayDeclarationNode>,
    pub initializer: Option<(Span, ExpressionNode)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayDeclarationNode {
    pub span: Span,
    pub data: ArrayDeclaration,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArrayDeclaration {
    /// `arr[];`
    Unknown,
    /// `arr[3];`
    Known(ExpressionNode),
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
    Void,
    // Floating types
    Float,
    Double,
    LongDouble,
    // Integer types
    Char,
    SignedChar,
    SignedShortInt,
    SignedInt,
    SignedLongInt,
    UnsignedChar,
    UnsignedShortInt,
    UnsignedInt,
    UnsignedLongInt,
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
