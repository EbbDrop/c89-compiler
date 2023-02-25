#[rustfmt::skip]
mod mainlistener;

#[rustfmt::skip]
mod mainlexer;

#[rustfmt::skip]
mod mainparser;

pub use mainlexer::MainLexer as Lexer;
pub use mainparser::{MainParser as Parser, MainParserContextType as ParserContextType};

pub mod context {
    #[rustfmt::skip]
    pub use super::mainparser::{
        CondAndContextAll       as CondAnd,
        CondOrContextAll        as CondOr,
        ExprArithContextAll     as ExprArith,
        ExprContextAll          as Expr,
        ExprFactorContextAll    as ExprFactor,
        ExprTermContextAll      as ExprTerm,
        FullExprContextAll      as FullExpr,
    };
}
