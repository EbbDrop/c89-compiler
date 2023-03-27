use crate::{
    ast,
    diagnostic::{AggregateResult, DiagnosticBuilder},
    ir::{
        ctype::CType,
        expr::{LvalueExpr, LvalueExprNode},
        stmt::{Block, Root, Stmt, StmtNode},
        table::Item,
    },
};

use self::symbol_table::{ScopedHandle, ScopedTable};

mod expr;
mod symbol_table;
mod util;

pub fn build_ir_from_ast(ast: &ast::Ast) -> AggregateResult<Root> {
    let mut root_table = ScopedTable::default();
    let mut root_scope = root_table.get_scoped_handle();

    let mut res = AggregateResult::new_ok(Vec::new());
    for statement in &ast.global.0 {
        build_ir_from_statement(statement, &mut root_scope).add_to(&mut res, |v, s| {
            if let Some(s) = s {
                v.push(s)
            }
        });
    }
    std::mem::drop(root_scope);
    res.map(|stmts| Root {
        global: Block(stmts),
        table: root_table.into_table(),
    })
}

fn build_ir_from_statement(
    statement: &ast::StatementNode,
    scope: &mut ScopedHandle,
) -> AggregateResult<Option<StmtNode>> {
    let expr = match &statement.data {
        ast::Statement::Declaration {
            type_name,
            ident,
            initializer,
        } => {
            // This has to be done first, so that the ident is not declared yet.
            let init_expr = match initializer.as_ref() {
                Some((op_span, init)) => {
                    expr::build_ir_expr(init, scope).map(|init_expr| Some((op_span, init_expr)))
                }
                None => AggregateResult::new_ok(None),
            };

            // Most of the complecity here comes from needing to make shure all diagnostic are
            // alloways reported
            declaration(type_name, ident, scope)
                .zip(init_expr)
                .and_then(|(mut to, init_expr)| {
                    // This is the initializing assignment whitch is allowed to const values
                    to.is_const = false;
                    let init_expr = init_expr.map(|(op_span, init_expr)| {
                        expr::assign(to, init_expr, statement.span, *op_span)
                    });
                    match init_expr {
                        Some(expr) => expr.map(Some),
                        None => AggregateResult::new_ok(None),
                    }
                })
        }
        ast::Statement::Expression(e) => expr::build_ir_expr(e, scope).map(Some),
        ast::Statement::BlockStatement(_) => AggregateResult::new_err(
            // let inner_scope = scope.new_scope();
            DiagnosticBuilder::new(statement.span).build_unimplemented("blocks"),
        ),
    };

    expr.map(|expr| {
        expr.map(|expr| StmtNode {
            span: statement.span,
            stmt: Stmt::Expr(expr),
        })
    })
}

fn declaration(
    type_name: &ast::QualifiedTypeNode,
    ident: &ast::IdentNode,
    scope: &mut ScopedHandle,
) -> AggregateResult<LvalueExprNode> {
    let ty = CType::from_ast_type(&type_name.data.inner.data);
    let is_const = type_name.data.is_const.is_some();

    let item = Item {
        ty: ty.clone(),
        is_const,
        original_span: ident.span,
    };

    match scope.declare(ident.data.clone(), item) {
        Ok(id) => AggregateResult::new_ok(LvalueExprNode {
            span: ident.span,
            is_const,
            ty,
            expr: LvalueExpr::Ident(id),
        }),
        Err(id) => {
            let original_span = scope.root_table().get(id).original_span;
            AggregateResult::new_err(
                DiagnosticBuilder::new(ident.span)
                    .build_already_defined(&ident.data, original_span),
            )
        }
    }
}
