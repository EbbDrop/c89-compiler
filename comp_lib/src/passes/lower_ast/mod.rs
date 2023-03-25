use crate::{
    ast,
    diagnostic::{AggregateResult, DiagnosticBuilder},
    ir::{
        ctype::CType,
        expr::{LvalueExpr, LvalueExprNode},
        stmt::{Block, Root, Stmt, StmtNode},
    },
};

mod expr;
mod util;

pub fn build_ir_from_ast(ast: &ast::Ast) -> AggregateResult<Root> {
    let mut res = AggregateResult::new_ok(Vec::new());
    for statement in &ast.global.0 {
        build_ir_from_statement(statement).add_to(&mut res, |v, s| v.push(s));
    }
    res.map(|stmts| Root {
        global: Block(stmts),
    })
}

fn build_ir_from_statement(statement: &ast::StatementNode) -> AggregateResult<StmtNode> {
    let expr = match &statement.data {
        ast::Statement::Declaration {
            type_name,
            ident,
            initializer,
        } => {
            // TODO should use the same code as Assignment afther adding to symbol table
            let lexpr = LvalueExprNode {
                span: ident.span,
                is_const: type_name.data.is_const.is_some(),
                ty: CType::from_ast_type(&type_name.data.inner.data),
                expr: LvalueExpr::Ident(ident.data.clone()),
            };
            let expr = expr::build_ir_expr(initializer.as_ref().unwrap());
            expr.and_then(|expr| expr::assign(lexpr, expr, statement.span, statement.span))
        }
        ast::Statement::Assignment { ident: _, rhs: _ } => {
            todo!("cant build Assignment (no symbol table yet)")
        }
        ast::Statement::Expression(e) => expr::build_ir_expr(e),
        ast::Statement::BlockStatement(_) => AggregateResult::new_err(
            DiagnosticBuilder::new(statement.span).build_unimplemented("blocks"),
        ),
    };

    expr.map(|expr| StmtNode {
        span: statement.span,
        stmt: Stmt::Expr(expr),
    })
}
