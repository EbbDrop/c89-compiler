#[rustfmt::skip]
#[allow(clippy::all)]
mod cparserlistener;

#[rustfmt::skip]
#[allow(clippy::all)]
pub mod clexer;

#[rustfmt::skip]
#[allow(clippy::all)]
#[allow(unused_parens)]
pub mod cparser;

pub use clexer::{CLexer, CLexerActions};
pub use cparser::{CParser, CParserContextType, CParserExt};

pub mod context {
    #[rustfmt::skip]
    pub use super::cparser::{
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
}
