use crate::{
    ast,
    diagnostic::{AggregateResult, DiagnosticBuilder},
    ir::{
        ctype::{self, CType},
        expr::{LvalueExpr, LvalueExprNode},
        stmt::{Block, Root, Stmt, StmtNode},
        table::Item,
    },
};

use self::symbol_table::{ScopedHandle, ScopedTable};

mod expr;
mod symbol_table;
mod type_checking;
mod util;

pub fn build_ir_from_ast(ast: &ast::Ast) -> AggregateResult<Root> {
    let mut res: AggregateResult<Option<&ast::FunctionDefinition>> = AggregateResult::new_ok(None);

    for g in &ast.global_declarations {
        match &g.data {
            ast::ExternalDeclaration::FunctionDefinition(f) => {
                if f.ident.data == "main" {
                    if let Some(other_main) = res.value() {
                        if let Some(other_main) = other_main {
                            res.add_err(
                                DiagnosticBuilder::new(f.ident.span)
                                    .build_already_defined("main", other_main.ident.span),
                            )
                        } else if let Some(v) = res.value_mut() {
                            *v = Some(f);
                        }
                    }
                } else {
                    res.add_rec_diagnostic(
                        DiagnosticBuilder::new(g.span).build_unimplemented("non `main` functions"),
                    );
                }
            }
            ast::ExternalDeclaration::Declaration(ast::Declaration::Variable(_)) => {
                res.add_rec_diagnostic(
                    DiagnosticBuilder::new(g.span).build_unimplemented("global declarations"),
                );
            }
            ast::ExternalDeclaration::Declaration(ast::Declaration::FunctionDeclaration(_)) => {
                res.add_rec_diagnostic(
                    DiagnosticBuilder::new(g.span).build_unimplemented("forward declarations"),
                );
            }
        }
    }

    if let Some(include) = ast.include_stdio {
        res.add_rec_diagnostic(DiagnosticBuilder::new(include).build_unimplemented("include"));
    }

    res.and_then(|main| {
        if let Some(main) = main {
            build_ir_from_block(&main.body)
        } else {
            AggregateResult::new_err(
                DiagnosticBuilder::new(0..0).build_unimplemented("library projects"),
            )
        }
    })
}

fn build_ir_from_block(block: &ast::BlockStatementNode) -> AggregateResult<Root> {
    let mut root_table = ScopedTable::default();
    let mut root_scope = root_table.get_scoped_handle();

    let mut res = AggregateResult::new_ok(Vec::new());
    for statement in &block.stmts {
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
        ast::Statement::Declaration(ast::Declaration::FunctionDeclaration(_)) => {
            AggregateResult::new_rec(
                None,
                DiagnosticBuilder::new(statement.span).build_unimplemented("forward declarations"),
            )
        }
        ast::Statement::Declaration(ast::Declaration::Variable(ast::VariableDeclaration {
            type_name,
            ident,
            is_array,
            initializer,
        })) => {
            if let Some(array) = is_array {
                return AggregateResult::new_err(
                    DiagnosticBuilder::new(array.span).build_unimplemented("array declarations"),
                );
            }
            // This has to be done first, so that the ident is not declared yet.
            let init_expr = match initializer.as_ref() {
                Some((op_span, init)) => {
                    expr::build_ir_expr(init, scope).map(|init_expr| Some((op_span, init_expr)))
                }
                None => AggregateResult::new_ok(None),
            };

            // Most of the complecity here comes from needing to make shure all diagnostic are
            // alloways reported
            declaration(type_name, ident, initializer.is_some(), scope)
                .zip(init_expr)
                .and_then(|(mut to, init_expr)| {
                    // This is the initializing assignment whitch is allowed to const values
                    to.is_const = false;
                    let init_expr = init_expr.map(|(op_span, init_expr)| {
                        expr::assign(to, init_expr, statement.span, *op_span)
                    });
                    match init_expr {
                        Some(expr) => expr.map(|expr| Some(Stmt::Expr(expr))),
                        None => AggregateResult::new_ok(None),
                    }
                })
        }
        ast::Statement::Expression(e) => {
            expr::build_ir_expr(e, scope).map(|expr| Some(Stmt::Expr(expr)))
        }
        ast::Statement::If(_) => AggregateResult::new_err(
            DiagnosticBuilder::new(statement.span).build_unimplemented("if"),
        ),
        ast::Statement::Switch(_) => AggregateResult::new_err(
            DiagnosticBuilder::new(statement.span).build_unimplemented("switch"),
        ),
        ast::Statement::While(_) => AggregateResult::new_err(
            DiagnosticBuilder::new(statement.span).build_unimplemented("while"),
        ),
        ast::Statement::For(_) => AggregateResult::new_err(
            DiagnosticBuilder::new(statement.span).build_unimplemented("for"),
        ),
        ast::Statement::Break => AggregateResult::new_err(
            DiagnosticBuilder::new(statement.span).build_unimplemented("break"),
        ),
        ast::Statement::Continue => AggregateResult::new_err(
            DiagnosticBuilder::new(statement.span).build_unimplemented("continue"),
        ),
        ast::Statement::Return(_) => AggregateResult::new_err(
            DiagnosticBuilder::new(statement.span).build_unimplemented("return"),
        ),
        ast::Statement::Printf(e) => {
            let e = expr::build_ir_expr(e, scope);
            e.map(|expr| match &expr.ty {
                CType::Scalar(ctype::Scalar::Arithmetic(a)) => {
                    if a.is_integral() {
                        let p = a.promote().0;
                        util::maybe_cast(expr, CType::Scalar(ctype::Scalar::Arithmetic(p)))
                    } else {
                        util::maybe_cast(
                            expr,
                            CType::Scalar(ctype::Scalar::Arithmetic(ctype::Arithmetic::Double)),
                        )
                    }
                }
                _ => expr,
            })
            .map(|expr| Some(Stmt::Printf(expr)))
        }
        ast::Statement::BlockStatement(_) => AggregateResult::new_err(
            // let inner_scope = scope.new_scope();
            DiagnosticBuilder::new(statement.span).build_unimplemented("blocks"),
        ),
    };

    expr.map(|expr| {
        expr.map(|stmt| StmtNode {
            span: statement.span,
            stmt,
            comments: statement.comments.clone(),
        })
    })
}

fn declaration(
    type_name: &ast::QualifiedTypeNode,
    ident: &ast::IdentNode,
    will_init: bool,
    scope: &mut ScopedHandle,
) -> AggregateResult<LvalueExprNode> {
    let ty = CType::from_ast_type(&type_name.data.inner.data);
    let is_const = type_name.data.is_const.is_some();

    let item = Item {
        ty: ty.clone(),
        is_const,
        original_span: ident.span,
        initialized: will_init,
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

#[cfg(test)]
mod tests;
