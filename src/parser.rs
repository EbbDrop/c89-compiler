mod generated;

use antlr_rust::{common_token_stream::CommonTokenStream, InputStream};
use generated::{Lexer, Parser};

use super::ast;

pub fn parse(input: &str) -> ast::Ast {
    let input = InputStream::new(input.into());
    let lexer = Lexer::new(input);
    let token_stream = CommonTokenStream::new(lexer);
    let mut parser = Parser::new(token_stream);

    let full_expr = parser.fullExpr().expect("parser error");

    ast::Ast::Expression(ast_builder::build_from_full_expr(&*full_expr))
}

mod ast_builder {
    use antlr_rust::token::Token;

    use super::{ast, generated::context};

    pub fn build_from_full_expr(ctx: &context::FullExpr) -> ast::Expression {
        build_from_cond_or(ctx.value.as_deref().unwrap())
    }

    fn build_from_cond_or(ctx: &context::CondOr) -> ast::Expression {
        match ctx {
            context::CondOr::CondOrSingularContext(singular) => {
                build_from_cond_and(singular.value.as_deref().unwrap())
            }
            context::CondOr::CondOrComposedContext(composed) => ast::Expression::Binary(
                Box::new(build_from_cond_or(composed.lhs.as_deref().unwrap())),
                ast::BinaryOperator::DoublePipe,
                Box::new(build_from_cond_and(composed.rhs.as_deref().unwrap())),
            ),
            context::CondOr::Error(_) => panic!(),
        }
    }

    fn build_from_cond_and(ctx: &context::CondAnd) -> ast::Expression {
        match ctx {
            context::CondAnd::CondAndSingularContext(singular) => {
                build_from_expr(singular.value.as_deref().unwrap())
            }
            context::CondAnd::CondAndComposedContext(composed) => ast::Expression::Binary(
                Box::new(build_from_cond_and(composed.lhs.as_deref().unwrap())),
                ast::BinaryOperator::DoubleAmpersand,
                Box::new(build_from_expr(composed.rhs.as_deref().unwrap())),
            ),
            context::CondAnd::Error(_) => panic!(),
        }
    }

    fn build_from_expr(ctx: &context::Expr) -> ast::Expression {
        match ctx {
            context::Expr::ExprSingularContext(singular) => {
                build_from_expr_arith(singular.value.as_deref().unwrap())
            }
            context::Expr::ExprComposedContext(composed) => ast::Expression::Binary(
                Box::new(build_from_expr(composed.lhs.as_deref().unwrap())),
                match composed.op.as_deref().unwrap().get_text() {
                    "<" => ast::BinaryOperator::AngleLeft,
                    ">" => ast::BinaryOperator::AngleRight,
                    "==" => ast::BinaryOperator::DoubleEquals,
                    "<=" => ast::BinaryOperator::AngleLeftEquals,
                    ">=" => ast::BinaryOperator::AngleRightEquals,
                    "!=" => ast::BinaryOperator::BangEquals,
                    _ => unreachable!(),
                },
                Box::new(build_from_expr_arith(composed.rhs.as_deref().unwrap())),
            ),
            context::Expr::Error(_) => panic!(),
        }
    }

    fn build_from_expr_arith(ctx: &context::ExprArith) -> ast::Expression {
        match ctx {
            context::ExprArith::ExprArithSingularContext(singular) => {
                build_from_expr_term(singular.value.as_deref().unwrap())
            }
            context::ExprArith::ExprArithComposedContext(composed) => ast::Expression::Binary(
                Box::new(build_from_expr_arith(composed.lhs.as_deref().unwrap())),
                match composed.op.as_deref().unwrap().get_text() {
                    "+" => ast::BinaryOperator::Plus,
                    "-" => ast::BinaryOperator::Minus,
                    _ => unreachable!(),
                },
                Box::new(build_from_expr_term(composed.rhs.as_deref().unwrap())),
            ),
            context::ExprArith::Error(_) => panic!(),
        }
    }

    fn build_from_expr_term(ctx: &context::ExprTerm) -> ast::Expression {
        match ctx {
            context::ExprTerm::ExprTermSingularContext(singular) => {
                build_from_expr_factor(singular.value.as_deref().unwrap())
            }
            context::ExprTerm::ExprTermComposedContext(composed) => ast::Expression::Binary(
                Box::new(build_from_expr_term(composed.lhs.as_deref().unwrap())),
                match composed.op.as_deref().unwrap().get_text() {
                    "*" => ast::BinaryOperator::Star,
                    "/" => ast::BinaryOperator::Slash,
                    _ => unreachable!(),
                },
                Box::new(build_from_expr_factor(composed.rhs.as_deref().unwrap())),
            ),
            context::ExprTerm::Error(_) => panic!(),
        }
    }

    fn build_from_expr_factor(ctx: &context::ExprFactor) -> ast::Expression {
        match ctx {
            context::ExprFactor::ExprFactorWrappedContext(wrapped) => {
                build_from_full_expr(wrapped.inner.as_deref().unwrap())
            }
            context::ExprFactor::ExprFactorUnaryOpContext(unary_op) => ast::Expression::Unary(
                match unary_op.op.as_deref().unwrap().get_text() {
                    "!" => ast::UnaryOperator::Bang,
                    "+" => ast::UnaryOperator::Plus,
                    "-" => ast::UnaryOperator::Minus,
                    _ => unreachable!(),
                },
                Box::new(build_from_expr_factor(unary_op.value.as_deref().unwrap())),
            ),
            context::ExprFactor::ExprFactorLiteralContext(literal) => {
                ast::Expression::Literal(literal.value.as_deref().unwrap().get_text().to_owned())
            }
            context::ExprFactor::Error(_) => panic!(),
        }
    }
}
