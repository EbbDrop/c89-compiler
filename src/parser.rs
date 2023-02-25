mod generated;

use antlr_rust::{
    common_token_stream::CommonTokenStream, errors::ANTLRError, DefaultErrorStrategy, InputStream,
    Parser as _,
};
use generated::{Lexer, Parser, ParserContextType};
use std::{fmt, ops::DerefMut};

use super::ast;

#[derive(Debug)]
pub struct ParseError(ANTLRError);

#[derive(Debug)]
pub struct UnspecifiedAntlrError;

impl fmt::Display for UnspecifiedAntlrError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "unspecified ANTLR error")
    }
}

impl std::error::Error for UnspecifiedAntlrError {}

pub fn parse(input: &str) -> Result<ast::Ast, ParseError> {
    let lexer = build_lexer(input);
    let token_stream = CommonTokenStream::new(lexer);
    let mut parser = build_parser(token_stream);

    let full_expr = parser.fullExpr().map_err(ParseError)?;

    ast_builder::build_from_full_expr(full_expr.as_ref())
        .map(ast::Ast::Expression)
        .map_err(ParseError)
}

fn build_lexer(input: &str) -> Lexer<'_, InputStream<&str>> {
    let input = InputStream::new(input.into());
    let mut lexer = Lexer::new(input);
    lexer.remove_error_listeners();
    // TODO: add custom error listener
    // lexer.add_error_listener(Box::new(ConsoleErrorListener {}));
    lexer
}

fn build_parser<'input>(
    token_stream: CommonTokenStream<'input, Lexer<'input, InputStream<&'input str>>>,
) -> Parser<
    'input,
    CommonTokenStream<'input, Lexer<'input, InputStream<&'input str>>>,
    DefaultErrorStrategy<'input, ParserContextType>,
> {
    let mut parser = Parser::new(token_stream);
    let base_parser = parser.deref_mut();
    base_parser.remove_error_listeners();
    // TODO: add custom error listener
    // base_parser.add_error_listener(Box::new(DiagnosticErrorListener::new(false)));
    parser
}

mod ast_builder {
    use antlr_rust::{errors::ANTLRError, token::Token};
    use std::rc::Rc;

    use crate::parser::UnspecifiedAntlrError;

    use super::{ast, generated::context};

    pub fn build_from_full_expr(ctx: &context::FullExpr) -> Result<ast::Expression, ANTLRError> {
        build_from_cond_or(ctx.value.as_deref().unwrap())
    }

    fn build_from_cond_or(ctx: &context::CondOr) -> Result<ast::Expression, ANTLRError> {
        use context::CondOr;
        match ctx {
            CondOr::CondOrSingularContext(singular) => {
                build_from_cond_and(singular.value.as_deref().unwrap())
            }
            CondOr::CondOrComposedContext(composed) => Ok(ast::Expression::Binary(
                Box::new(build_from_cond_or(composed.lhs.as_deref().unwrap())?),
                ast::BinaryOperator::DoublePipe,
                Box::new(build_from_cond_and(composed.rhs.as_deref().unwrap())?),
            )),
            CondOr::Error(base_ctx) => Err(base_ctx
                .exception
                .clone()
                .map(Box::into_inner)
                .unwrap_or(ANTLRError::FallThrough(Rc::new(UnspecifiedAntlrError {})))),
        }
    }

    fn build_from_cond_and(ctx: &context::CondAnd) -> Result<ast::Expression, ANTLRError> {
        use context::CondAnd;
        match ctx {
            CondAnd::CondAndSingularContext(singular) => {
                build_from_expr(singular.value.as_deref().unwrap())
            }
            CondAnd::CondAndComposedContext(composed) => Ok(ast::Expression::Binary(
                Box::new(build_from_cond_and(composed.lhs.as_deref().unwrap())?),
                ast::BinaryOperator::DoubleAmpersand,
                Box::new(build_from_expr(composed.rhs.as_deref().unwrap())?),
            )),
            CondAnd::Error(base_ctx) => Err(base_ctx
                .exception
                .clone()
                .map(Box::into_inner)
                .unwrap_or(ANTLRError::FallThrough(Rc::new(UnspecifiedAntlrError {})))),
        }
    }

    fn build_from_expr(ctx: &context::Expr) -> Result<ast::Expression, ANTLRError> {
        use context::Expr;
        match ctx {
            Expr::ExprSingularContext(singular) => {
                build_from_expr_arith(singular.value.as_deref().unwrap())
            }
            Expr::ExprComposedContext(composed) => Ok(ast::Expression::Binary(
                Box::new(build_from_expr(composed.lhs.as_deref().unwrap())?),
                match composed.op.as_deref().unwrap().get_text() {
                    "<" => ast::BinaryOperator::AngleLeft,
                    ">" => ast::BinaryOperator::AngleRight,
                    "==" => ast::BinaryOperator::DoubleEquals,
                    "<=" => ast::BinaryOperator::AngleLeftEquals,
                    ">=" => ast::BinaryOperator::AngleRightEquals,
                    "!=" => ast::BinaryOperator::BangEquals,
                    _ => unreachable!(),
                },
                Box::new(build_from_expr_arith(composed.rhs.as_deref().unwrap())?),
            )),
            Expr::Error(base_ctx) => Err(base_ctx
                .exception
                .clone()
                .map(Box::into_inner)
                .unwrap_or(ANTLRError::FallThrough(Rc::new(UnspecifiedAntlrError {})))),
        }
    }

    fn build_from_expr_arith(ctx: &context::ExprArith) -> Result<ast::Expression, ANTLRError> {
        use context::ExprArith;
        match ctx {
            ExprArith::ExprArithSingularContext(singular) => {
                build_from_expr_term(singular.value.as_deref().unwrap())
            }
            ExprArith::ExprArithComposedContext(composed) => Ok(ast::Expression::Binary(
                Box::new(build_from_expr_arith(composed.lhs.as_deref().unwrap())?),
                match composed.op.as_deref().unwrap().get_text() {
                    "+" => ast::BinaryOperator::Plus,
                    "-" => ast::BinaryOperator::Minus,
                    _ => unreachable!(),
                },
                Box::new(build_from_expr_term(composed.rhs.as_deref().unwrap())?),
            )),
            ExprArith::Error(base_ctx) => Err(base_ctx
                .exception
                .clone()
                .map(Box::into_inner)
                .unwrap_or(ANTLRError::FallThrough(Rc::new(UnspecifiedAntlrError {})))),
        }
    }

    fn build_from_expr_term(ctx: &context::ExprTerm) -> Result<ast::Expression, ANTLRError> {
        use context::ExprTerm;
        match ctx {
            ExprTerm::ExprTermSingularContext(singular) => {
                build_from_expr_factor(singular.value.as_deref().unwrap())
            }
            ExprTerm::ExprTermComposedContext(composed) => Ok(ast::Expression::Binary(
                Box::new(build_from_expr_term(composed.lhs.as_deref().unwrap())?),
                match composed.op.as_deref().unwrap().get_text() {
                    "*" => ast::BinaryOperator::Star,
                    "/" => ast::BinaryOperator::Slash,
                    _ => unreachable!(),
                },
                Box::new(build_from_expr_factor(composed.rhs.as_deref().unwrap())?),
            )),
            ExprTerm::Error(base_ctx) => Err(base_ctx
                .exception
                .clone()
                .map(Box::into_inner)
                .unwrap_or(ANTLRError::FallThrough(Rc::new(UnspecifiedAntlrError {})))),
        }
    }

    fn build_from_expr_factor(ctx: &context::ExprFactor) -> Result<ast::Expression, ANTLRError> {
        use context::ExprFactor;
        match ctx {
            ExprFactor::ExprFactorWrappedContext(wrapped) => {
                build_from_full_expr(wrapped.inner.as_deref().unwrap())
            }
            ExprFactor::ExprFactorUnaryOpContext(unary_op) => Ok(ast::Expression::Unary(
                match unary_op.op.as_deref().unwrap().get_text() {
                    "!" => ast::UnaryOperator::Bang,
                    "+" => ast::UnaryOperator::Plus,
                    "-" => ast::UnaryOperator::Minus,
                    _ => unreachable!(),
                },
                Box::new(build_from_expr_factor(unary_op.value.as_deref().unwrap())?),
            )),
            ExprFactor::ExprFactorLiteralContext(literal) => Ok(ast::Expression::Literal(
                literal.value.as_deref().unwrap().get_text().to_owned(),
            )),
            ExprFactor::Error(base_ctx) => Err(base_ctx
                .exception
                .clone()
                .map(Box::into_inner)
                .unwrap_or(ANTLRError::FallThrough(Rc::new(UnspecifiedAntlrError {})))),
        }
    }
}
