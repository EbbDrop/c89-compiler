#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Ast {
    BlockStatement(BlockStatement),
}

#[derive(Debug, Clone)]
pub struct BlockStatement(pub Vec<Statement>);

#[derive(Debug, Clone)]

pub enum Statement {
    Declaration {
        type_name: Type,
        ident: String,
        initializer: Option<Expression>,
    },
    Assignment {
        ident: String,
        rhs: Expression,
    },
    Expression(Expression),
    BlockStatement(BlockStatement),
}

type Type = QualifiedType;

#[derive(Debug, Clone)]
pub struct QualifiedType {
    pub is_const: bool,
    // pub is_volitile: bool,
    // pub is_restrict: bool,
    pub inner: UnqualifiedType,
}

#[derive(Debug, Clone)]
pub enum UnqualifiedType {
    PointerType(Box<QualifiedType>),
    // ArrayType,
    // FunctionType,
    PlainType(PlainType),
}

#[derive(Debug, Clone)]
pub enum PlainType {
    Primitive(PrimitiveType),
    // StructType(String),
    // EnumType(String),
}

#[derive(Debug, Clone)]
pub enum PrimitiveType {
    Char,
    Int,
    Float,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Expression {
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Cast(Type, Box<Expression>),
    Literal(Literal),
    Ident(String),
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Literal {
    pub value: LiteralValue,
    pub t: UnqualifiedType,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum LiteralValue {
    Integer(i128), //TODO change this to big int?
    Float(f64),
    // Void,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
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
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
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
