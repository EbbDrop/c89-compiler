mod generated;

use antlr_rust::{common_token_stream::CommonTokenStream, InputStream};
use generated::MainLexer as Lexer;
use generated::MainParser as Parser;

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

    use super::{
        ast,
        generated::{
            CondAndContextAll, CondOrContextAll, ExprArithContextAll, ExprContextAll,
            ExprFactorContextAll, ExprTermContextAll, FullExprContextAll,
        },
    };

    pub fn build_from_full_expr(ctx: &FullExprContextAll) -> ast::Expression {
        build_from_cond_or(ctx.value.as_deref().unwrap())
    }

    fn build_from_cond_or(ctx: &CondOrContextAll) -> ast::Expression {
        match ctx {
            CondOrContextAll::CondOrSingularContext(singular) => {
                build_from_cond_and(singular.value.as_deref().unwrap())
            }
            CondOrContextAll::CondOrComposedContext(composed) => ast::Expression::Binary(
                Box::new(build_from_cond_or(composed.lhs.as_deref().unwrap())),
                ast::BinaryOperator::DoublePipe,
                Box::new(build_from_cond_and(composed.rhs.as_deref().unwrap())),
            ),
            CondOrContextAll::Error(_) => panic!(),
        }
    }

    fn build_from_cond_and(ctx: &CondAndContextAll) -> ast::Expression {
        match ctx {
            CondAndContextAll::CondAndSingularContext(singular) => {
                build_from_expr(singular.value.as_deref().unwrap())
            }
            CondAndContextAll::CondAndComposedContext(composed) => ast::Expression::Binary(
                Box::new(build_from_cond_and(composed.lhs.as_deref().unwrap())),
                ast::BinaryOperator::DoubleAmpersand,
                Box::new(build_from_expr(composed.rhs.as_deref().unwrap())),
            ),
            CondAndContextAll::Error(_) => panic!(),
        }
    }

    fn build_from_expr(ctx: &ExprContextAll) -> ast::Expression {
        match ctx {
            ExprContextAll::ExprSingularContext(singular) => {
                build_from_expr_arith(singular.value.as_deref().unwrap())
            }
            ExprContextAll::ExprComposedContext(composed) => ast::Expression::Binary(
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
            ExprContextAll::Error(_) => panic!(),
        }
    }

    fn build_from_expr_arith(ctx: &ExprArithContextAll) -> ast::Expression {
        match ctx {
            ExprArithContextAll::ExprArithSingularContext(singular) => {
                build_from_expr_term(singular.value.as_deref().unwrap())
            }
            ExprArithContextAll::ExprArithComposedContext(composed) => ast::Expression::Binary(
                Box::new(build_from_expr_arith(composed.lhs.as_deref().unwrap())),
                match composed.op.as_deref().unwrap().get_text() {
                    "+" => ast::BinaryOperator::Plus,
                    "-" => ast::BinaryOperator::Minus,
                    _ => unreachable!(),
                },
                Box::new(build_from_expr_term(composed.rhs.as_deref().unwrap())),
            ),
            ExprArithContextAll::Error(_) => panic!(),
        }
    }

    fn build_from_expr_term(ctx: &ExprTermContextAll) -> ast::Expression {
        match ctx {
            ExprTermContextAll::ExprTermSingularContext(singular) => {
                build_from_expr_factor(singular.value.as_deref().unwrap())
            }
            ExprTermContextAll::ExprTermComposedContext(composed) => ast::Expression::Binary(
                Box::new(build_from_expr_term(composed.lhs.as_deref().unwrap())),
                match composed.op.as_deref().unwrap().get_text() {
                    "*" => ast::BinaryOperator::Star,
                    "/" => ast::BinaryOperator::Slash,
                    _ => unreachable!(),
                },
                Box::new(build_from_expr_factor(composed.rhs.as_deref().unwrap())),
            ),
            ExprTermContextAll::Error(_) => panic!(),
        }
    }

    fn build_from_expr_factor(ctx: &ExprFactorContextAll) -> ast::Expression {
        match ctx {
            ExprFactorContextAll::ExprFactorWrappedContext(wrapped) => {
                build_from_full_expr(wrapped.inner.as_deref().unwrap())
            }
            ExprFactorContextAll::ExprFactorUnaryOpContext(unary_op) => ast::Expression::Unary(
                match unary_op.op.as_deref().unwrap().get_text() {
                    "!" => ast::UnaryOperator::Bang,
                    "+" => ast::UnaryOperator::Plus,
                    "-" => ast::UnaryOperator::Minus,
                    _ => unreachable!(),
                },
                Box::new(build_from_expr_factor(unary_op.value.as_deref().unwrap())),
            ),
            ExprFactorContextAll::ExprFactorLiteralContext(literal) => {
                ast::Expression::Literal(literal.value.as_deref().unwrap().get_text().to_owned())
            }
            ExprFactorContextAll::Error(_) => panic!(),
        }
    }
}
