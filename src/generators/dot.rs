use std::fmt::Display;

use crate::ast::{
    self, Ast, BlockStatement, Expression, PlainType, PrimitiveType, QualifiedType, Statement,
    UnqualifiedType,
};

struct DotTree {
    name: String,
    children: Vec<(&'static str, DotTree)>,
}

impl DotTree {
    fn new(name: String, children: Vec<(&'static str, DotTree)>) -> Self {
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
            writeln!(f, " \"{:p}\" -> \"{:p}\" [label=\"{}\"];", self, &c.1, c.0)?;
            c.1.to_inner_dot(f)?
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
        Ast::BlockStatement(bs) => to_dot_block_statement(bs),
    };

    base.to_string()
}

fn to_dot_block_statement(bs: &BlockStatement) -> DotTree {
    DotTree::new(
        "block".to_string(),
        bs.0.iter().map(|s| ("stmt", to_dot_statement(s))).collect(),
    )
}

fn to_dot_statement(s: &Statement) -> DotTree {
    match s {
        ast::Statement::Declaration {
            type_name,
            ident,
            initializer,
        } => {
            let mut children = vec![
                ("type", to_dot_qualified_type(type_name)),
                ("ident", to_dot_ident(ident)),
            ];
            if let Some(initializer) = initializer {
                children.push(("rhs", to_dot_expr(initializer)))
            }

            DotTree::new("declaration".to_string(), children)
        }
        ast::Statement::Assignment { ident, rhs } => DotTree::new(
            "assignement".to_string(),
            vec![("ident", to_dot_ident(ident)), ("rhs", to_dot_expr(rhs))],
        ),

        ast::Statement::Expression(e) => to_dot_expr(e),
        ast::Statement::BlockStatement(bs) => to_dot_block_statement(bs),
    }
}

fn to_dot_expr(e: &Expression) -> DotTree {
    match e {
        Expression::Binary(e1, bo, e2) => DotTree::new(
            match bo {
                ast::BinaryOperator::Plus => "+",
                ast::BinaryOperator::Minus => "-",
                ast::BinaryOperator::Star => "*",
                ast::BinaryOperator::Slash => "/",
                ast::BinaryOperator::Pipe => "|",
                ast::BinaryOperator::Caret => "^",
                ast::BinaryOperator::Ampersand => "&",
                ast::BinaryOperator::AngleLeft => "<",
                ast::BinaryOperator::AngleRight => ">",
                ast::BinaryOperator::DoubleEquals => "==",
                ast::BinaryOperator::DoubleAmpersand => "&&",
                ast::BinaryOperator::DoublePipe => "||",
                ast::BinaryOperator::BangEquals => "!=",
                ast::BinaryOperator::Percent => "%",
                ast::BinaryOperator::AngleLeftEquals => "<=",
                ast::BinaryOperator::AngleRightEquals => ">=",
            }
            .to_string(),
            vec![("lhs", to_dot_expr(e1)), ("rhs", to_dot_expr(e2))],
        ),
        Expression::Unary(uo, e) => DotTree::new(
            match uo {
                ast::UnaryOperator::Bang => "!◌",
                ast::UnaryOperator::Plus => "+◌",
                ast::UnaryOperator::Minus => "-◌",
                ast::UnaryOperator::Tilde => "~◌",
                ast::UnaryOperator::Ampersand => "&◌",
                ast::UnaryOperator::Star => "*◌",
                ast::UnaryOperator::DoublePlusPrefix => "++◌",
                ast::UnaryOperator::DoubleMinusPrefix => "--◌",
                ast::UnaryOperator::DoublePlusPostfix => "◌++",
                ast::UnaryOperator::DoubleMinusPostfix => "◌--",
            }
            .to_string(),
            vec![("", to_dot_expr(e))],
        ),
        Expression::Cast(t, e) => DotTree::new(
            "cast".to_string(),
            vec![("type", to_dot_qualified_type(t)), ("expr", to_dot_expr(e))],
        ),
        Expression::Ident(i) => to_dot_ident(i),
        Expression::Literal(l) => DotTree::new_nc(l.clone()),
    }
}

fn to_dot_qualified_type(t: &QualifiedType) -> DotTree {
    let mut children = Vec::new();
    if t.is_const {
        children.push(("", DotTree::new_nc("const".to_string())));
    }
    children.push(("iner", to_dot_unqualified_type(&t.inner)));
    DotTree::new("q_type".to_string(), children)
}

fn to_dot_unqualified_type(t: &UnqualifiedType) -> DotTree {
    match t {
        ast::UnqualifiedType::PointerType(t) => DotTree::new(
            "pointer".to_string(),
            vec![("type", to_dot_qualified_type(&t))],
        ),
        ast::UnqualifiedType::PlainType(t) => {
            DotTree::new("plain".to_string(), vec![("type", to_dot_plain_type(t))])
        }
    }
}

fn to_dot_plain_type(t: &PlainType) -> DotTree {
    match t {
        PlainType::Primitive(p) => DotTree::new(
            "primitive".to_string(),
            vec![("", to_dot_primitive_type(p))],
        ),
    }
}

fn to_dot_primitive_type(t: &PrimitiveType) -> DotTree {
    match t {
        PrimitiveType::Char => DotTree::new_nc("char".to_string()),
        PrimitiveType::Int => DotTree::new_nc("int".to_string()),
        PrimitiveType::Float => DotTree::new_nc("float".to_string()),
    }
}

fn to_dot_ident(i: &String) -> DotTree {
    DotTree::new_nc(i.clone())
}
