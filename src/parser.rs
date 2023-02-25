mod generated;

use antlr_rust::{common_token_stream::CommonTokenStream, InputStream};
use generated::mainLexer as Lexer;
use generated::mainParser as Parser;

use super::ast;

pub fn parse(input: &str) -> ast::Ast {
    let input = InputStream::new(input.into());
    let lexer = Lexer::new(input);
    let token_stream = CommonTokenStream::new(lexer);
    let mut parser = Parser::new(token_stream);

    let full_expr = parser.full_expr().expect("parser error");

    ast::Ast::Expression(ast_builder::transform_full_expr(&*full_expr))
}

mod ast_builder {
    use antlr_rust::token::Token;

    use super::{
        ast,
        generated::{
            Cond_andContextAll, Cond_orContextAll, ExprContextAll, Expr_arithContextAll,
            Expr_factorContextAll, Expr_termContextAll, Full_exprContextAll,
        },
    };

    pub fn transform_full_expr(ctx: &Full_exprContextAll) -> ast::Expression {
        transform_cond_or(ctx.value.as_deref().unwrap())
    }

    fn transform_cond_or(ctx: &Cond_orContextAll) -> ast::Expression {
        match ctx {
            Cond_orContextAll::CondOrSingularContext(singular) => {
                transform_cond_and(singular.value.as_deref().unwrap())
            }
            Cond_orContextAll::CondOrComposedContext(composed) => ast::Expression::Binary(
                Box::new(transform_cond_or(composed.lhs.as_deref().unwrap())),
                ast::BinaryOperator::DoublePipe,
                Box::new(transform_cond_and(composed.rhs.as_deref().unwrap())),
            ),
            Cond_orContextAll::Error(_) => panic!(),
        }
    }

    fn transform_cond_and(ctx: &Cond_andContextAll) -> ast::Expression {
        match ctx {
            Cond_andContextAll::CondAndSingularContext(singular) => {
                transform_expr(singular.value.as_deref().unwrap())
            }
            Cond_andContextAll::CondAndComposedContext(composed) => ast::Expression::Binary(
                Box::new(transform_cond_and(composed.lhs.as_deref().unwrap())),
                ast::BinaryOperator::DoubleAmpersand,
                Box::new(transform_expr(composed.rhs.as_deref().unwrap())),
            ),
            Cond_andContextAll::Error(_) => panic!(),
        }
    }

    fn transform_expr(ctx: &ExprContextAll) -> ast::Expression {
        match ctx {
            ExprContextAll::ExprSingularContext(singular) => {
                transform_expr_arith(singular.value.as_deref().unwrap())
            }
            ExprContextAll::ExprComposedContext(composed) => ast::Expression::Binary(
                Box::new(transform_expr(composed.lhs.as_deref().unwrap())),
                match composed.op.as_deref().unwrap().get_text() {
                    "<" => ast::BinaryOperator::AngleLeft,
                    ">" => ast::BinaryOperator::AngleRight,
                    "==" => ast::BinaryOperator::DoubleEquals,
                    "<=" => ast::BinaryOperator::AngleLeftEquals,
                    ">=" => ast::BinaryOperator::AngleRightEquals,
                    "!=" => ast::BinaryOperator::BangEquals,
                    _ => unreachable!(),
                },
                Box::new(transform_expr_arith(composed.rhs.as_deref().unwrap())),
            ),
            ExprContextAll::Error(_) => panic!(),
        }
    }

    fn transform_expr_arith(ctx: &Expr_arithContextAll) -> ast::Expression {
        match ctx {
            Expr_arithContextAll::ExprArithSingularContext(singular) => {
                transform_expr_term(singular.value.as_deref().unwrap())
            }
            Expr_arithContextAll::ExprArithComposedContext(composed) => ast::Expression::Binary(
                Box::new(transform_expr_arith(composed.lhs.as_deref().unwrap())),
                match composed.op.as_deref().unwrap().get_text() {
                    "+" => ast::BinaryOperator::Plus,
                    "-" => ast::BinaryOperator::Minus,
                    _ => unreachable!(),
                },
                Box::new(transform_expr_term(composed.rhs.as_deref().unwrap())),
            ),
            Expr_arithContextAll::Error(_) => panic!(),
        }
    }

    fn transform_expr_term(ctx: &Expr_termContextAll) -> ast::Expression {
        match ctx {
            Expr_termContextAll::ExprTermSingularContext(singular) => {
                transform_expr_factor(singular.value.as_deref().unwrap())
            }
            Expr_termContextAll::ExprTermComposedContext(composed) => ast::Expression::Binary(
                Box::new(transform_expr_term(composed.lhs.as_deref().unwrap())),
                match composed.op.as_deref().unwrap().get_text() {
                    "*" => ast::BinaryOperator::Star,
                    "/" => ast::BinaryOperator::Slash,
                    _ => unreachable!(),
                },
                Box::new(transform_expr_factor(composed.rhs.as_deref().unwrap())),
            ),
            Expr_termContextAll::Error(_) => panic!(),
        }
    }

    fn transform_expr_factor(ctx: &Expr_factorContextAll) -> ast::Expression {
        match ctx {
            Expr_factorContextAll::ExprFactorWrappedContext(wrapped) => {
                transform_full_expr(wrapped.inner.as_deref().unwrap())
            }
            Expr_factorContextAll::ExprFactorUnaryOpContext(unary_op) => ast::Expression::Unary(
                match unary_op.op.as_deref().unwrap().get_text() {
                    "!" => ast::UnaryOperator::Bang,
                    "+" => ast::UnaryOperator::Plus,
                    "-" => ast::UnaryOperator::Minus,
                    _ => unreachable!(),
                },
                Box::new(transform_expr_factor(unary_op.value.as_deref().unwrap())),
            ),
            Expr_factorContextAll::ExprFactorLiteralContext(literal) => {
                ast::Expression::Literal(literal.value.as_deref().unwrap().get_text().to_owned())
            }
            Expr_factorContextAll::Error(_) => panic!(),
        }
    }
}
