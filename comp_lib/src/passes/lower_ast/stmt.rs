use crate::{
    ast,
    diagnostic::{AggregateResult, DiagnosticBuilder, Span},
    ir::{
        ctype::{self, CType},
        table::Item,
        BlockNode, ExprNode, IfStmtNode, LoopStmtNode, LvalueExpr, LvalueExprNode, Stmt, StmtNode,
        SwitchStmtCase, SwitchStmtCaseNode, SwitchStmtNode,
    },
};

use super::{
    expr,
    type_checking::{AnyScaler, CheckUnErr, PromoteArith, TypeRuleUn},
    util::{self, Scope},
};

/// WARNNG! Does not create its own new scope!
pub fn build_ir_from_block(
    block: &ast::BlockStatementNode,
    scope: &mut Scope,
) -> AggregateResult<BlockNode> {
    let mut res = AggregateResult::new_ok(Vec::new());
    for statement in &block.stmts {
        build_ir_from_statement(statement, scope).add_to(&mut res, |res, s| {
            res.extend_from_slice(&s);
        });
    }
    res.map(|stmts| BlockNode {
        span: block.span,
        stmts,
    })
}

/// Checkes if the expresion has a type that matches the `TypeRule`. But does not do any conversions!
fn check_type<R: TypeRuleUn>(
    rule: R,
    expr: ExprNode,
    field_name: &str,
) -> AggregateResult<ExprNode> {
    match rule.check(&expr.ty) {
        Ok(_) => AggregateResult::new_ok(expr),
        Err(err) => match err {
            CheckUnErr::Expected(type_cat) => AggregateResult::new_err(
                DiagnosticBuilder::new(expr.span)
                    .build_wrong_statement_type(field_name, &expr.ty, type_cat),
            ),
        },
    }
}

pub fn build_ir_from_statement(
    statement: &ast::StatementNode,
    scope: &mut Scope,
) -> AggregateResult<Vec<StmtNode>> {
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
        ast::Statement::If(stmt) => {
            if_statement(stmt, statement.span, scope).map(|stmt| Some(Stmt::IfStmt(stmt)))
        }
        ast::Statement::Switch(stmt) => {
            switch_statement(stmt, statement.span, scope).map(|stmt| Some(Stmt::SwitchStmt(stmt)))
        }
        ast::Statement::While(stmt) => {
            while_statement(stmt, statement.span, scope).map(|stmt| Some(Stmt::LoopStmt(stmt)))
        }
        ast::Statement::For(stmt) => {
            return for_statement(stmt, statement.span, scope).map(|(mut init, loop_stmt)| {
                init.push(StmtNode {
                    comments: statement.comments.clone(),
                    span: statement.span,
                    stmt: Stmt::LoopStmt(loop_stmt),
                });
                init
            });
        }
        ast::Statement::Break => {
            if scope.in_switch || scope.in_loop {
                AggregateResult::new_ok(Some(Stmt::Break))
            } else {
                AggregateResult::new_err(
                    DiagnosticBuilder::new(statement.span).build_invalid_break(),
                )
            }
        }
        ast::Statement::Continue => {
            if scope.in_loop {
                AggregateResult::new_ok(Some(Stmt::Continue))
            } else {
                AggregateResult::new_err(
                    DiagnosticBuilder::new(statement.span).build_invalid_continue(),
                )
            }
        }
        ast::Statement::Return(e) => return_statement(e.as_ref(), scope),
        ast::Statement::BlockStatement(block) => {
            return build_ir_from_block(block, &mut scope.new_scope()).map(|block| block.stmts);
        }
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
    };

    expr.map(|expr| match expr {
        Some(stmt) => vec![StmtNode {
            span: statement.span,
            stmt,
            comments: statement.comments.clone(),
        }],
        None => Vec::new(),
    })
}

fn return_statement(
    e: Option<&ast::ExpressionNode>,
    _scope: &mut Scope,
) -> AggregateResult<Option<Stmt>> {
    match e {
        Some(_e) => AggregateResult::new_err(
            DiagnosticBuilder::new(_e.span).build_unimplemented("value return"),
        ),
        None => AggregateResult::new_ok(Some(Stmt::Return(None))),
    }
}

fn if_statement(
    stmt: &ast::IfStatement,
    span: Span,
    scope: &mut Scope,
) -> AggregateResult<IfStmtNode> {
    let res = expr::build_ir_expr(&stmt.condition, scope);

    let if_branch = build_ir_from_block(&stmt.if_body, &mut scope.new_scope());

    let else_branch = match stmt.else_body.as_ref() {
        Some(else_body) => build_ir_from_block(else_body, &mut scope.new_scope()).map(Some),
        None => AggregateResult::new_ok(None),
    };

    res.and_then(|expr| check_type(AnyScaler, expr, "if condition"))
        .zip(if_branch)
        .zip(else_branch)
        .map(|((condition, if_branch), else_branch)| IfStmtNode {
            span,
            condition,
            if_branch,
            else_branch,
        })
}

fn switch_statement(
    stmt: &ast::SwitchStatement,
    span: Span,
    scope: &mut Scope,
) -> AggregateResult<SwitchStmtNode> {
    let res = expr::build_ir_expr(&stmt.expr, scope)
        .and_then(|expr| check_type(PromoteArith::only_int(), expr, "switch quantity"));

    let mut inner_scope = scope.new_scope().in_switch();

    let mut found_default = false;
    let mut cases = AggregateResult::new_ok(Vec::new());

    for case in &stmt.cases {
        match case {
            ast::SwitchCase::Expr(case) => {
                let value = match &case.expr.data {
                    ast::Expression::Literal(ast::LiteralNode { data, .. }) => match data {
                        ast::Literal::Dec(v) | ast::Literal::Hex(v) | ast::Literal::Octal(v) => {
                            Ok(*v)
                        }
                        ast::Literal::Char(v) => Ok(*v as i128),
                        _ => Err(DiagnosticBuilder::new(case.expr.span).build_case_not_int()),
                    },
                    _ => Err(DiagnosticBuilder::new(case.expr.span).build_case_not_folded()),
                };

                let value = match value {
                    Ok(v) => AggregateResult::new_ok(v),
                    Err(d) => AggregateResult::new_err(d),
                };
                value
                    .zip(build_ir_from_block(&case.body, &mut inner_scope))
                    .map(|(v, block)| SwitchStmtCaseNode {
                        span: block.span,
                        data: SwitchStmtCase::Case {
                            label: v,
                            body: block,
                        },
                    })
                    .add_to(&mut cases, |res, c| res.push(c))
            }
            ast::SwitchCase::Default(case) => {
                assert!(!found_default, "ICE: multiple default cases in ast");
                found_default = true;

                build_ir_from_block(&case.body, &mut inner_scope)
                    .map(|block| SwitchStmtCaseNode {
                        span: block.span,
                        data: SwitchStmtCase::Default { body: block },
                    })
                    .add_to(&mut cases, |res, c| res.push(c))
            }
        }
    }

    res.zip(cases).map(|(expr, cases)| SwitchStmtNode {
        span,
        expr,
        cases,
        has_default: found_default,
    })
}

fn while_statement(
    stmt: &ast::WhileStatement,
    span: Span,
    scope: &mut Scope,
) -> AggregateResult<LoopStmtNode> {
    expr::build_ir_expr(&stmt.condition, scope)
        .and_then(|expr| check_type(AnyScaler, expr, "while condition"))
        .zip(build_ir_from_block(
            &stmt.body,
            &mut scope.new_scope().in_loop(),
        ))
        .map(|(condition, body)| LoopStmtNode {
            span,
            condition: Some(condition),
            body,
            continuation: None,
        })
}

fn for_statement(
    stmt: &ast::ForStatement,
    span: Span,
    scope: &mut Scope,
) -> AggregateResult<(Vec<StmtNode>, LoopStmtNode)> {
    let mut init_scope = scope.new_scope();

    let init = match &stmt.init {
        Some(init) => build_ir_from_statement(init, &mut init_scope),
        None => AggregateResult::new_ok(Vec::new()),
    };

    let condition = match stmt.condition.as_ref() {
        Some(expr) => expr::build_ir_expr(expr, &mut init_scope)
            .and_then(|expr| check_type(AnyScaler, expr, "for condition"))
            .map(Some),
        None => AggregateResult::new_ok(None),
    };

    let mut inner_scope = init_scope.new_scope().in_loop();

    let loop_stmt = condition
        .zip(match stmt.iter.as_ref() {
            Some(expr) => expr::build_ir_expr(expr, &mut inner_scope).map(Some),
            None => AggregateResult::new_ok(None),
        })
        .zip(build_ir_from_block(&stmt.body, &mut inner_scope))
        .map(|((condition, iter), body)| LoopStmtNode {
            span,
            condition,
            body,
            continuation: iter,
        });

    init.zip(loop_stmt)
}

fn declaration(
    type_name: &ast::QualifiedTypeNode,
    ident: &ast::IdentNode,
    will_init: bool,
    scope: &mut Scope,
) -> AggregateResult<LvalueExprNode> {
    let ty = CType::from_ast_type(&type_name.data.inner.data);
    let is_const = type_name.data.is_const.is_some();

    let item = Item {
        ty: ty.clone(),
        is_const,
        original_span: ident.span,
        initialized: will_init,
    };

    match scope.vars.declare(ident.data.clone(), item) {
        Ok(id) => AggregateResult::new_ok(LvalueExprNode {
            span: ident.span,
            is_const,
            ty,
            expr: LvalueExpr::Ident(id),
        }),
        Err(id) => {
            let original_span = scope.vars.root_table().get(id).original_span;
            AggregateResult::new_err(
                DiagnosticBuilder::new(ident.span)
                    .build_already_defined(&ident.data, original_span),
            )
        }
    }
}
