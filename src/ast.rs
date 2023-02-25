#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Ast {
    Expression(Expression),
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Expression {
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Literal(String),
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Star,
    Slash,
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
    DoublePlus,
    DoubleMinus,
}
