use crate::generated;
use antlr_rust::{token_factory::TokenFactory as AntlrTF, token_stream::TokenStream as AntlrTS};

#[rustfmt::skip]
pub use crate::generated::cparser::{
    ArithExprContextAll                 as ArithExpr,
    AssignmentStatementContextAll       as AssignmentStatement,
    BitwiseAndExprContextAll            as BitwiseAndExpr,
    BitwiseOrExprContextAll             as BitwiseOrExpr,
    BitwiseXorExprContextAll            as BitwiseXorExpr,
    BlockStatementContextAll            as BlockStatement,
    CastExprContextAll                  as CastExpr,
    CondExprContextAll                  as CondExpr,
    DeclarationStatementContextAll      as DeclarationStatement,
    EqualityExprContextAll              as EqualityExpr,
    ExprContextAll                      as Expr,
    IdentifierContextAll                as Identifier,
    InequalityExprContextAll            as InequalityExpr,
    IntegerLiteralContextAll            as IntegerLiteral,
    LiteralContextAll                   as Literal,
    LogicalAndExprContextAll            as LogicalAndExpr,
    LogicalOrExprContextAll             as LogicalOrExpr,
    PostfixExprContextAll               as PostfixExpr,
    PrimaryExprContextAll               as PrimaryExpr,
    PrimitiveTypeContextAll             as PrimitiveType,
    StatementContextAll                 as Statement,
    TermExprContextAll                  as TermExpr,
    TranslationUnitContextAll           as TranslationUnit,
    TypeNameContextAll                  as TypeName,
    TypeQualifierContextAll             as TypeQualifier,
    TypeSpecifierContextAll             as TypeSpecifier,
    UnaryExprContextAll                 as UnaryExpr,
};

pub type TokenStream<'a> = generated::TokenStream<'a, &'a str>;
pub type TokenFactory<'a> = <TokenStream<'a> as AntlrTS<'a>>::TF;
pub type TFTok<'a> = <TokenFactory<'a> as AntlrTF<'a>>::Tok;
pub type TFInner<'a> = <TokenFactory<'a> as AntlrTF<'a>>::Inner;
pub type TFData<'a> = <TokenFactory<'a> as AntlrTF<'a>>::Data;

pub struct Cst<'a> {
    pub(crate) translation_unit: TranslationUnit<'a>,
    pub(crate) token_stream: TokenStream<'a>,
}
