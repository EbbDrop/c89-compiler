use std::iter;

use crate::ir::{
    ctype::CType, BinaryOp, BitwiseOp, BlockNode, Constant, Expr, ExprNode, IfStmtNode,
    LoopStmtNode, LvalueExpr, LvalueExprNode, RelationOp, Root, Stmt, SwitchStmtCase,
    SwitchStmtNode, UnaryOp,
};

use super::{dot_tree::DotTree, ToDot};

impl ToDot for Root {
    fn to_dot(&self) -> DotTree {
        let mut childeren = Vec::new();
        for (name, var) in &self.vars {
            childeren.push((
                "var",
                DotTree::new(name.clone(), vec![("type", var.ty.to_dot())]),
            ));
        }

        for (name, func) in &self.functions {
            childeren.push((
                "func",
                DotTree::new(
                    name.clone(),
                    iter::once(("ret type", func.return_type.to_dot()))
                        .chain(func.params.iter().map(|param| ("param", param.ty.to_dot())))
                        .chain(func.body.as_ref().map(|b| ("body", b.to_dot())))
                        .collect(),
                ),
            ));
        }

        DotTree::new("root".to_owned(), childeren)
    }
}

impl ToDot for BlockNode {
    fn to_dot(&self) -> DotTree {
        DotTree::new(
            "block".to_owned(),
            self.stmts
                .iter()
                .map(|stmt| ("stmt", stmt.stmt.to_dot()))
                .collect(),
        )
    }
}

impl ToDot for Stmt {
    fn to_dot(&self) -> DotTree {
        match self {
            Stmt::Expr(e) => e.to_dot(),
            Stmt::IfStmt(i) => i.to_dot(),
            Stmt::SwitchStmt(i) => i.to_dot(),
            Stmt::LoopStmt(i) => i.to_dot(),
            Stmt::Break => DotTree::new_leaf("break".to_owned()),
            Stmt::Continue => DotTree::new_leaf("continue".to_owned()),
            Stmt::Return(e) => DotTree::new(
                "return".to_owned(),
                e.iter().map(|e| ("value", e.to_dot())).collect(),
            ),
        }
    }
}

impl ToDot for IfStmtNode {
    fn to_dot(&self) -> DotTree {
        DotTree::new(
            "if".to_owned(),
            [
                ("cond", self.condition.to_dot()),
                ("if body", self.if_branch.to_dot()),
            ]
            .into_iter()
            .chain(
                self.else_branch
                    .as_ref()
                    .map(|else_body| ("else body", else_body.to_dot())),
            )
            .collect(),
        )
    }
}

impl ToDot for SwitchStmtNode {
    fn to_dot(&self) -> DotTree {
        DotTree::new(
            "switch".to_owned(),
            self.cases
                .iter()
                .map(|case| ("case", case.data.to_dot()))
                .collect(),
        )
    }
}

impl ToDot for SwitchStmtCase {
    fn to_dot(&self) -> DotTree {
        match self {
            SwitchStmtCase::Case { label, body } => DotTree::new(
                "case".to_owned(),
                vec![
                    ("value", DotTree::new_leaf(label.to_string())),
                    ("body", body.to_dot()),
                ],
            ),
            SwitchStmtCase::Default { body } => {
                DotTree::new("default".to_owned(), vec![("body", body.to_dot())])
            }
        }
    }
}

impl ToDot for LoopStmtNode {
    fn to_dot(&self) -> DotTree {
        DotTree::new(
            "loop".to_owned(),
            self.condition
                .iter()
                .map(|cond| ("cond", cond.to_dot()))
                .chain(
                    self.continuation
                        .as_ref()
                        .map(|cont| ("continuation", cont.to_dot())),
                )
                .chain(iter::once(("body", self.body.to_dot())))
                .collect(),
        )
    }
}

impl ToDot for ExprNode {
    fn to_dot(&self) -> DotTree {
        DotTree::new(
            "expr".to_owned(),
            vec![("type", self.ty.to_dot()), ("expr", self.expr.to_dot())],
        )
    }
}

impl ToDot for Expr {
    fn to_dot(&self) -> DotTree {
        let (name, childs) = match self {
            Expr::LvalueDeref(i) => ("lvalue deref", vec![i.to_dot()]),
            Expr::Constant(c) => return c.to_dot(),
            Expr::FunctionCall(name, arg) => {
                return DotTree::new(name.clone(), arg.iter().map(|a| ("", a.to_dot())).collect())
            }
            Expr::PostfixInc(i) => ("◌++", vec![i.to_dot()]),
            Expr::PostfixDec(i) => ("◌--", vec![i.to_dot()]),
            Expr::PrefixInc(i) => ("++◌", vec![i.to_dot()]),
            Expr::PrefixDec(i) => ("--◌", vec![i.to_dot()]),
            Expr::Reference(i) => ("&◌", vec![i.to_dot()]),
            Expr::UnaryArith(un, i) => (
                match un {
                    UnaryOp::Neg => "-◌",
                    UnaryOp::BitNot => "~◌",
                    UnaryOp::Not => "!◌",
                },
                vec![i.to_dot()],
            ),
            Expr::Binary(a, op, b) => (
                match op {
                    BinaryOp::Mul => "◌*◌",
                    BinaryOp::Div => "◌/◌",
                    BinaryOp::Rem => "◌%◌",
                    BinaryOp::Add => "◌+◌",
                    BinaryOp::Sub => "◌-◌",
                    BinaryOp::ShiftLeft => "◌<<◌",
                    BinaryOp::ShiftRight => "◌>>◌",
                    BinaryOp::Bitwise(op) => match op {
                        BitwiseOp::And => "◌&◌",
                        BitwiseOp::Or => "◌|◌",
                        BitwiseOp::Xor => "◌^◌",
                    },
                },
                vec![a.to_dot(), b.to_dot()],
            ),
            Expr::Relation(a, rel, b) => (
                match rel {
                    RelationOp::Eq => "◌==◌",
                    RelationOp::Ne => "◌!=◌",
                    RelationOp::Lt => "◌<◌",
                    RelationOp::Gt => "◌>◌",
                    RelationOp::Ge => "◌>=◌",
                    RelationOp::Le => "◌<=◌",
                },
                vec![a.to_dot(), b.to_dot()],
            ),
            Expr::LogicalAnd(a, b) => ("◌&&◌", vec![a.to_dot(), b.to_dot()]),
            Expr::LogicalOr(a, b) => ("◌||◌", vec![a.to_dot(), b.to_dot()]),
            Expr::Assign(a, b) => ("◌=◌", vec![a.to_dot(), b.to_dot()]),
            Expr::Cast(i) => ("cast", vec![i.to_dot()]),
        };

        DotTree::new(
            name.to_owned(),
            childs.into_iter().map(|c| ("", c)).collect(),
        )
    }
}

impl ToDot for LvalueExprNode {
    fn to_dot(&self) -> DotTree {
        DotTree::new(
            "lvalue\nexpr".to_owned(),
            vec![("type", self.ty.to_dot()), ("expr", self.expr.to_dot())],
        )
    }
}

impl ToDot for LvalueExpr {
    fn to_dot(&self) -> DotTree {
        match self {
            LvalueExpr::Ident(i) => DotTree::new_leaf(format!("ident: {i:?}")),
            LvalueExpr::GlobalIdent(i) => DotTree::new_leaf(format!("global ident: {i}")),
            LvalueExpr::Dereference(i) => DotTree::new("*◌".to_owned(), vec![("", i.to_dot())]),
        }
    }
}

impl ToDot for Constant {
    fn to_dot(&self) -> DotTree {
        match self {
            Constant::Integer(i) => DotTree::new_leaf(i.to_string()),
            Constant::Float(i) => DotTree::new_leaf(i.to_string()),
            Constant::String(i) => {
                DotTree::new_leaf(format!(r#""{}""#, String::from_utf8_lossy(i).into_owned()))
            }
        }
    }
}

impl ToDot for CType {
    fn to_dot(&self) -> DotTree {
        DotTree::new_leaf(self.to_string())
    }
}
