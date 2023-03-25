use super::{DotTree, ToDot};
use crate::ast;

impl ToDot for ast::Ast {
    fn to_dot(&self) -> DotTree {
        self.global.to_dot()
    }
}

impl ToDot for ast::BlockStatement {
    fn to_dot(&self) -> DotTree {
        DotTree::new(
            "block".to_owned(),
            self.0.iter().map(|s| ("stmt", s.data.to_dot())).collect(),
        )
    }
}

impl ToDot for ast::Statement {
    fn to_dot(&self) -> DotTree {
        match self {
            ast::Statement::Declaration {
                type_name,
                ident,
                initializer,
            } => {
                let mut children = vec![
                    ("type", type_name.data.to_dot()),
                    ("ident", to_dot_ident(&ident.data)),
                ];
                if let Some(initializer) = initializer {
                    children.push(("rhs", initializer.data.to_dot()))
                }

                DotTree::new("declaration".to_owned(), children)
            }

            ast::Statement::Expression(e) => e.data.to_dot(),
            ast::Statement::BlockStatement(bs) => bs.to_dot(),
        }
    }
}

impl ToDot for ast::Expression {
    fn to_dot(&self) -> DotTree {
        use ast::{BinaryOperator, UnaryOperator};
        match self {
            Self::Assignment(e1, _, e2) => DotTree::new(
                "=".to_owned(),
                vec![("lhs", e1.data.to_dot()), ("rhs", e2.data.to_dot())],
            ),
            Self::Binary(e1, bo, e2) => DotTree::new(
                match bo.data {
                    BinaryOperator::Plus => "+",
                    BinaryOperator::Minus => "-",
                    BinaryOperator::Star => "*",
                    BinaryOperator::Slash => "/",
                    BinaryOperator::Pipe => "|",
                    BinaryOperator::Caret => "^",
                    BinaryOperator::Ampersand => "&",
                    BinaryOperator::AngleLeft => "<",
                    BinaryOperator::AngleRight => ">",
                    BinaryOperator::DoubleEquals => "==",
                    BinaryOperator::DoubleAmpersand => "&&",
                    BinaryOperator::DoublePipe => "||",
                    BinaryOperator::BangEquals => "!=",
                    BinaryOperator::Percent => "%",
                    BinaryOperator::AngleLeftEquals => "<=",
                    BinaryOperator::AngleRightEquals => ">=",
                }
                .to_owned(),
                vec![("lhs", e1.data.to_dot()), ("rhs", e2.data.to_dot())],
            ),
            Self::Unary(uo, e) => DotTree::new(
                match &uo.data {
                    UnaryOperator::Bang => "!◌",
                    UnaryOperator::Plus => "+◌",
                    UnaryOperator::Minus => "-◌",
                    UnaryOperator::Tilde => "~◌",
                    UnaryOperator::Ampersand => "&◌",
                    UnaryOperator::Star => "*◌",
                    UnaryOperator::DoublePlusPrefix => "++◌",
                    UnaryOperator::DoubleMinusPrefix => "--◌",
                    UnaryOperator::DoublePlusPostfix => "◌++",
                    UnaryOperator::DoubleMinusPostfix => "◌--",
                }
                .to_owned(),
                vec![("", e.data.to_dot())],
            ),
            Self::Cast(t, e) => DotTree::new(
                "cast".to_owned(),
                vec![("type", t.data.to_dot()), ("expr", e.data.to_dot())],
            ),
            Self::Ident(i) => to_dot_ident(&i.data),
            Self::Literal(lit) => lit.data.to_dot(),
        }
    }
}

impl ToDot for ast::Literal {
    fn to_dot(&self) -> DotTree {
        let (name, value) = match self {
            ast::Literal::Dec(i) => ("dec", i),
            ast::Literal::Hex(i) => ("hex", i),
            ast::Literal::Octal(i) => ("octal", i),
            ast::Literal::Char(i) => ("char", i),
            ast::Literal::Float(f) => {
                return DotTree::new(
                    "float".to_owned(),
                    vec![("value", DotTree::new_leaf(f.to_string()))],
                )
            }
        };
        DotTree::new(
            name.to_owned(),
            vec![("value", DotTree::new_leaf(value.to_string()))],
        )
    }
}

impl ToDot for ast::QualifiedType {
    fn to_dot(&self) -> DotTree {
        let mut children = Vec::new();
        if self.is_const.is_some() {
            children.push(("", DotTree::new_leaf("const".to_owned())));
        }
        children.push(("iner", self.inner.data.to_dot()));
        DotTree::new("q_type".to_owned(), children)
    }
}

impl ToDot for ast::UnqualifiedType {
    fn to_dot(&self) -> DotTree {
        match self {
            Self::PointerType(t) => {
                DotTree::new("pointer".to_owned(), vec![("type", t.data.to_dot())])
            }
            Self::PlainType(t) => DotTree::new("plain".to_owned(), vec![("type", t.to_dot())]),
        }
    }
}

impl ToDot for ast::PlainType {
    fn to_dot(&self) -> DotTree {
        match self {
            Self::Primitive(p) => DotTree::new("primitive".to_owned(), vec![("", p.to_dot())]),
        }
    }
}

impl ToDot for ast::PrimitiveType {
    fn to_dot(&self) -> DotTree {
        match self {
            Self::Char => DotTree::new_leaf("char".to_owned()),
            Self::Int => DotTree::new_leaf("int".to_owned()),
            Self::Float => DotTree::new_leaf("float".to_owned()),
        }
    }
}

fn to_dot_ident(i: &str) -> DotTree {
    DotTree::new_leaf(i.to_owned())
}
