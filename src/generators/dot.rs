use std::fmt::Display;

use crate::ast::{Ast, Expression};

struct DotTree {
    name: String,
    children: Vec<DotTree>,
}

impl DotTree {
    fn new(name: String, children: Vec<DotTree>) -> Self {
        Self { name, children }
    }

    fn new_nc(name: String) -> Self {
        Self {
            name,
            children: vec![],
        }
    }

    fn to_inner_dot(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, " \"{:p}\" [label=\"{}\"];", self, self.name)?;
        for c in &self.children {
            writeln!(f, " \"{:p}\" -> \"{:p}\";", self, c)?;
            c.to_inner_dot(f)?
        }
        Ok(())
    }
}

impl Display for DotTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "digraph AST {{")?;
        writeln!(f, "ordering=\"out\"")?;
        self.to_inner_dot(f)?;
        writeln!(f, "}}")?;

        Ok(())
    }
}

pub fn to_dot(ast: &Ast) -> String {
    let base = match ast {
        Ast::Expression(e) => to_dot_expr(e),
    };

    base.to_string()
}

fn to_dot_expr(e: &Expression) -> DotTree {
    match e {
        Expression::Binary(e1, bo, e2) => DotTree::new(
            match bo {
                crate::ast::BinaryOperator::Plus => "+",
                crate::ast::BinaryOperator::Minus => "-",
                crate::ast::BinaryOperator::Star => "*",
                crate::ast::BinaryOperator::Slash => "/",
                crate::ast::BinaryOperator::AngleLeft => "<",
                crate::ast::BinaryOperator::AngleRight => ">",
                crate::ast::BinaryOperator::DoubleEquals => "==",
                crate::ast::BinaryOperator::DoubleAmpersand => "&&",
                crate::ast::BinaryOperator::DoublePipe => "||",
                crate::ast::BinaryOperator::BangEquals => "!=",
                crate::ast::BinaryOperator::Percent => "%",
                crate::ast::BinaryOperator::AngleLeftEquals => "<=",
                crate::ast::BinaryOperator::AngleRightEquals => ">=",
            }
            .to_string(),
            vec![to_dot_expr(e1), to_dot_expr(e2)],
        ),
        Expression::Unary(uo, e) => DotTree::new(
            match uo {
                crate::ast::UnaryOperator::Bang => "!",
                crate::ast::UnaryOperator::Plus => "+",
                crate::ast::UnaryOperator::Minus => "-",
                crate::ast::UnaryOperator::DoublePlus => "++",
                crate::ast::UnaryOperator::DoubleMinus => "--",
            }
            .to_string(),
            vec![to_dot_expr(e)],
        ),
        Expression::Literal(l) => DotTree::new_nc(l.clone()),
    }
}
