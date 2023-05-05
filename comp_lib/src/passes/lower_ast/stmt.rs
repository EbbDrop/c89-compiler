use std::collections::HashSet;

use crate::{
    ast,
    diagnostic::{builder::InvalidArraySize, AggregateResult, DiagnosticBuilder, Span},
    ir::{
        ctype::{self, CType},
        table::VariableItem,
        BlockNode, ExprNode, IfStmtNode, LoopStmtNode, LvalueExpr, LvalueExprNode, Stmt, StmtNode,
        SwitchStmtCase, SwitchStmtCaseNode, SwitchStmtNode,
    },
    passes::lower_ast::util::maybe_cast,
};

use super::{
    expr,
    type_checking::{check_assign, AnyScaler, CheckUnErr, PromoteArith, TypeRuleUn},
    util::{extract_literal_int, DeclarationType, FunctionScope, LiteralExtractErr},
};

/// WARNING! Does not create its own new scope!
pub fn build_ir_from_block(
    block: &ast::BlockStatementNode,
    scope: &mut FunctionScope,
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
    scope: &mut FunctionScope,
) -> AggregateResult<Vec<StmtNode>> {
    let expr = match &statement.data {
        ast::Statement::Declaration(ast::Declaration::FunctionDeclaration(_)) => {
            AggregateResult::new_rec(
                Vec::new(),
                DiagnosticBuilder::new(statement.span)
                    .build_unimplemented("nested function declarations"),
            )
        }
        ast::Statement::Declaration(ast::Declaration::Variable(decl)) => {
            variable_declaration(decl, statement.span, scope)
        }
        ast::Statement::Expression(e) => {
            expr::build_ir_expr(e, scope).map(|expr| vec![Stmt::Expr(expr)])
        }
        ast::Statement::If(stmt) => {
            if_statement(stmt, statement.span, scope).map(|stmt| vec![Stmt::IfStmt(stmt)])
        }
        ast::Statement::Switch(stmt) => {
            switch_statement(stmt, statement.span, scope).map(|stmt| vec![Stmt::SwitchStmt(stmt)])
        }
        ast::Statement::While(stmt) => {
            while_statement(stmt, statement.span, scope).map(|stmt| vec![Stmt::LoopStmt(stmt)])
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
                AggregateResult::new_ok(vec![Stmt::Break])
            } else {
                AggregateResult::new_err(
                    DiagnosticBuilder::new(statement.span).build_invalid_break(),
                )
            }
        }
        ast::Statement::Continue => {
            if scope.in_loop {
                AggregateResult::new_ok(vec![Stmt::Continue])
            } else {
                AggregateResult::new_err(
                    DiagnosticBuilder::new(statement.span).build_invalid_continue(),
                )
            }
        }
        ast::Statement::Return(span, e) => return_statement(e.as_ref(), *span, scope),
        ast::Statement::BlockStatement(block) => {
            return build_ir_from_block(block, &mut scope.new_scope()).map(|block| block.stmts);
        }
    };

    expr.map(|stmts| {
        let mut stmts: Vec<StmtNode> = stmts
            .into_iter()
            .map(|stmt| StmtNode {
                comments: None,
                span: statement.span,
                stmt,
            })
            .collect();
        if let Some(stmt) = stmts.first_mut() {
            stmt.comments = statement.comments.clone();
        }
        stmts
    })
}

fn return_statement(
    e: Option<&ast::ExpressionNode>,
    span: Span,
    scope: &mut FunctionScope,
) -> AggregateResult<Vec<Stmt>> {
    let (return_type_span, return_type) = scope.func_return_type;
    match e {
        Some(e) => expr::build_ir_expr(e, scope).and_then(|expr| {
            let mut res = AggregateResult::new_ok(());

            let builder = DiagnosticBuilder::new(span);
            use super::type_checking::AssignCheckResult::*;
            match check_assign(return_type, &expr.ty) {
                Ok => {}
                Lossy => res.add_rec_diagnostic(builder.build_implicit_lossy_return(
                    &expr,
                    return_type_span,
                    return_type,
                    false,
                )),
                SignChange => res.add_rec_diagnostic(builder.build_implicit_lossy_return(
                    &expr,
                    return_type_span,
                    return_type,
                    true,
                )),
                LossOfConst => res.add_rec_diagnostic(
                    builder.build_return_const_loss(expr.span, return_type_span),
                ),
                Incompatible | PointerAndInt => res.add_rec_diagnostic(
                    builder.build_incompatible_return(&expr, return_type_span, return_type),
                ),
                PointerAndFloat => res.add_err(builder.build_incompatible_return(
                    &expr,
                    return_type_span,
                    return_type,
                )),
                FromVoid => res.add_err(builder.build_void_used(&expr)),
                ToVoid => {
                    res.add_err(
                        DiagnosticBuilder::new(expr.span)
                            .build_value_return_to_void(return_type_span),
                    );
                }
                ToArray => unreachable!("ICE: Arrays aren't a valid return type for functions"),
                FromArray => {
                    unreachable!("ICE: Array should have been converted to a pointer by now")
                }
            }

            res.map(|()| vec![Stmt::Return(Some(maybe_cast(expr, return_type.clone())))])
        }),
        None => match return_type {
            CType::Void => AggregateResult::new_ok(vec![Stmt::Return(None)]),
            _ => AggregateResult::new_err(
                DiagnosticBuilder::new(span).build_no_return_value(return_type_span, return_type),
            ),
        },
    }
}

fn if_statement(
    stmt: &ast::IfStatement,
    span: Span,
    scope: &mut FunctionScope,
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
    scope: &mut FunctionScope,
) -> AggregateResult<SwitchStmtNode> {
    let res = expr::build_ir_expr(&stmt.expr, scope)
        .and_then(|expr| check_type(PromoteArith::only_int(), expr, "switch quantity"));

    let mut inner_scope = scope.new_scope().in_switch();

    let mut found_default = false;
    let mut cases = AggregateResult::new_ok(Vec::new());

    let mut case_values_used = HashSet::new();

    for case in &stmt.cases {
        match case {
            ast::SwitchCase::Expr(case) => {
                let value = match extract_literal_int(&case.expr.data) {
                    Ok(v) => AggregateResult::new_ok(v),
                    Err(LiteralExtractErr::NotALiteral) => AggregateResult::new_err(
                        DiagnosticBuilder::new(case.expr.span).build_case_not_folded(),
                    ),
                    Err(LiteralExtractErr::NotAnInt) => AggregateResult::new_err(
                        DiagnosticBuilder::new(case.expr.span).build_case_not_int(),
                    ),
                };

                let value = value.and_then(|v| {
                    if !case_values_used.insert(v) {
                        return AggregateResult::new_err(
                            DiagnosticBuilder::new(case.expr.span).build_multiple_same_case_value(),
                        );
                    }
                    AggregateResult::new_ok(v)
                });

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
    scope: &mut FunctionScope,
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
    scope: &mut FunctionScope,
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

fn variable_declaration(
    decl: &ast::VariableDeclaration,
    span: Span,
    scope: &mut FunctionScope,
) -> AggregateResult<Vec<Stmt>> {
    // This has to be done first, so that the ident is not declared yet.
    let init_expr = match decl.initializer.as_ref() {
        Some((op_span, init)) => {
            expr::build_ir_expr(init, scope).map(|init_expr| Some((op_span, init_expr)))
        }
        None => AggregateResult::new_ok(None),
    };

    declaration_type(&decl.type_name, &decl.array_parts)
        .and_then(|ty| {
            if matches!(ty.ty, CType::Void) {
                return AggregateResult::new_err(
                    DiagnosticBuilder::new(decl.type_name.span).build_void_vars(),
                );
            }
            AggregateResult::new_ok(ty)
        })
        .and_then(|DeclarationType { ty, is_const }| {
            let item = VariableItem {
                original_span: span,
                ty: ty.clone(),
                is_const,
                initialized: decl.initializer.is_some(),
            };
            match scope.vars.declare(decl.ident.data.clone(), item) {
                Ok(id) => AggregateResult::new_ok(LvalueExprNode {
                    span: decl.ident.span,
                    ty,
                    is_const,
                    expr: LvalueExpr::Ident(id),
                }),
                Err(id) => {
                    let original_span = scope.vars.root_table().get(id).original_span;
                    AggregateResult::new_err(
                        DiagnosticBuilder::new(decl.ident.span)
                            .build_already_defined(&decl.ident.data, original_span),
                    )
                }
            }
        })
        .zip(init_expr)
        .and_then(|(mut to, init_expr)| {
            // This is the initializing assignment whitch is allowed to const values
            to.is_const = false;
            let init_expr =
                init_expr.map(|(op_span, init_expr)| expr::assign(to, init_expr, span, *op_span));

            let mut res = AggregateResult::new_ok(Vec::new());
            if let Some(expr) = init_expr {
                expr.add_to(&mut res, |res, expr| res.push(Stmt::Expr(expr)));
            }
            res
        })
}

pub fn declaration_type(
    type_name: &ast::QualifiedTypeNode,
    array_parts: &[ast::ArrayDeclarationNode],
) -> AggregateResult<DeclarationType> {
    let ty = CType::from_ast_type(&type_name.unqualified.data);
    let is_const = type_name.is_const.is_some();

    let mut ty = match (&ty, array_parts.last()) {
        (CType::Void, Some(last_part)) => {
            // special case to disallow void arrays
            AggregateResult::new_err(
                DiagnosticBuilder::new(type_name.span).build_void_array(last_part.span),
            )
        }
        _ => AggregateResult::new_ok(ty),
    };

    for array_part in array_parts.iter().rev() {
        match &array_part.data {
            ast::ArrayDeclaration::Unknown => {
                ty.add_err(
                    DiagnosticBuilder::new(array_part.span)
                        .build_unimplemented("implicitly sized arrays"),
                );
            }
            ast::ArrayDeclaration::Known(expr) => {
                let length = match extract_literal_int(&expr.data) {
                    Ok(value) => match value {
                        ..=-1 => AggregateResult::new_err(
                            DiagnosticBuilder::new(array_part.span)
                                .build_invalid_array_size(InvalidArraySize::NegativeSized),
                        ),
                        0 => AggregateResult::new_err(
                            DiagnosticBuilder::new(array_part.span)
                                .build_invalid_array_size(InvalidArraySize::ZeroSized),
                        ),
                        value => AggregateResult::new_ok(value as u128),
                    },
                    Err(LiteralExtractErr::NotALiteral) => AggregateResult::new_err(
                        DiagnosticBuilder::new(array_part.span)
                            .build_unimplemented("dynamically sized arrays"),
                    ),
                    Err(LiteralExtractErr::NotAnInt) => AggregateResult::new_err(
                        DiagnosticBuilder::new(array_part.span)
                            .build_invalid_array_size(InvalidArraySize::NonInt),
                    ),
                };
                ty = ty.zip(length).map(|(ty, length)| {
                    CType::Aggregate(ctype::Aggregate::Array(ctype::Array {
                        inner: Box::new(ty),
                        length,
                    }))
                });
            }
        }
    }

    ty.map(|ty| DeclarationType { ty, is_const })
}
