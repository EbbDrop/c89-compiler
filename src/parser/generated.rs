#[rustfmt::skip]
mod mainlistener;

#[rustfmt::skip]
mod mainlexer;

#[rustfmt::skip]
#[allow(unused_parens)]
mod mainparser;

pub use mainlexer::{MainLexer, MainLexerActions};
pub use mainparser::{MainParser, MainParserContextType, MainParserExt};

pub mod context {
    #[rustfmt::skip]
    pub use super::mainparser::{
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
}
