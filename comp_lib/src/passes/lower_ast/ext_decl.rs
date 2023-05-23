use super::{
    expr::literal,
    stmt::{build_ir_from_block, declaration_type},
    symbol_table::ScopedTable,
    util::{DeclarationType, FunctionScope},
};
use crate::diagnostic::{AggregateResult, DiagnosticBuilder, Span};
use crate::ir::{self, ctype::CType, table::VariableItem};
use crate::{ast, settings::Settings};

pub fn build_ir_from_external_declaration(
    external_declaration: &ast::ExternalDeclarationNode,
    global: &mut ir::Root,
    settings: &Settings,
) -> AggregateResult<()> {
    let span = external_declaration.span;
    match &external_declaration.data {
        ast::ExternalDeclaration::Declaration(ast::Declaration::Variable(decl)) => {
            let var = AstGlobalVar {
                span,
                comments: external_declaration.comments.as_deref(),
                decl,
            };
            add_global_var(var, global, settings)
        }
        ast::ExternalDeclaration::Declaration(ast::Declaration::FunctionDeclaration(fd)) => {
            let function = AstFunction {
                prototype_span: span,
                comments: external_declaration.comments.as_deref(),
                return_type: &fd.return_type,
                ident: &fd.ident,
                params: &fd.params,
                is_vararg: fd.is_vararg,
                body: None,
            };
            add_function(function, global, settings)
        }
        ast::ExternalDeclaration::FunctionDefinition(fd) => {
            let mut function = AstFunction {
                prototype_span: fd.prototype_span,
                comments: None,
                return_type: &fd.return_type,
                ident: &fd.ident,
                params: &fd.params,
                is_vararg: fd.is_vararg,
                body: None,
            };
            // First forward declare the function, so that is already in scope if it is used within
            // its own body (i.e. for recursive functions).
            let _ = add_function(function.clone(), global, settings);
            // Now add the function definition as a whole.
            function.comments = external_declaration.comments.as_deref();
            function.body = Some(&fd.body);
            add_function(function, global, settings)
        }
    }
}

#[derive(Debug)]
struct AstGlobalVar<'a> {
    pub span: Span,
    pub comments: Option<&'a str>,
    pub decl: &'a ast::VariableDeclaration,
}

fn add_global_var(
    ext_decl: AstGlobalVar,
    global: &mut ir::Root,
    settings: &Settings,
) -> AggregateResult<()> {
    let decl = &ext_decl.decl;
    declaration_type(&decl.type_name, &decl.array_parts)
        .zip(AggregateResult::transpose_from(
            decl.initializer.as_ref().map(|(span, expr_node)| {
                extract_global_var_initializer(expr_node, settings).map(|con| (*span, con))
            }),
        ))
        .and_then(
            |(DeclarationType { ty, is_const, .. }, constant)| match ty {
                CType::Void => AggregateResult::new_err(
                    DiagnosticBuilder::new(decl.type_name.span).build_void_vars(),
                ),
                _ => {
                    let res = AggregateResult::transpose_from(
                        constant.map(|(span, constant)| check_constant_init(constant, &ty, span)),
                    );
                    res.map(|constant| ir::GlobalVarNode {
                        original_span: ext_decl.span,
                        comments: ext_decl.comments.map(String::from),
                        ty,
                        is_const,
                        value: constant,
                    })
                }
            },
        )
        .and_then(|global_var| {
            check_global_var_ident(&ext_decl, &global_var, global).map(|should_redefine| {
                if should_redefine {
                    global.vars.insert(decl.ident.data.clone(), global_var);
                }
            })
        })
}

fn check_constant_init(
    constant: ir::Constant,
    to_ty: &CType,
    span: Span,
) -> AggregateResult<ir::Constant> {
    use ir::ctype::{Arithmetic, Pointer, Scalar};
    match &constant {
        ir::Constant::Integer(v) => {
            let int_ty = CType::Scalar(Scalar::Arithmetic(Arithmetic::SignedInt));
            match &to_ty {
                CType::Void | CType::Aggregate(_) | CType::Scalar(Scalar::Pointer(_)) => {
                    AggregateResult::new_err(
                        DiagnosticBuilder::new(span).build_incompatible_global_def(&int_ty, to_ty),
                    )
                }
                CType::Scalar(Scalar::Arithmetic(a)) => {
                    if a.is_floating() {
                        AggregateResult::new_ok(ir::Constant::Float(*v as f64))
                    } else {
                        AggregateResult::new_ok(constant)
                    }
                }
            }
        }
        ir::Constant::Float(v) => {
            let int_ty = CType::Scalar(Scalar::Arithmetic(Arithmetic::Double));
            match &to_ty {
                CType::Void | CType::Aggregate(_) | CType::Scalar(Scalar::Pointer(_)) => {
                    AggregateResult::new_err(
                        DiagnosticBuilder::new(span).build_incompatible_global_def(&int_ty, to_ty),
                    )
                }
                CType::Scalar(Scalar::Arithmetic(a)) => {
                    if a.is_floating() {
                        AggregateResult::new_ok(constant)
                    } else {
                        AggregateResult::new_ok(ir::Constant::Integer(*v as i128))
                    }
                }
            }
        }
        ir::Constant::String(_) => {
            let int_ty = CType::Scalar(Scalar::Pointer(Pointer {
                inner: Box::new(CType::Scalar(Scalar::Arithmetic(Arithmetic::Char))),
                inner_const: true,
            }));
            match &to_ty {
                CType::Void | CType::Aggregate(_) | CType::Scalar(Scalar::Arithmetic(_)) => {
                    AggregateResult::new_err(
                        DiagnosticBuilder::new(span).build_incompatible_global_def(&int_ty, to_ty),
                    )
                }
                CType::Scalar(Scalar::Pointer(Pointer { inner_const, .. })) => {
                    if *inner_const {
                        AggregateResult::new_ok(constant)
                    } else {
                        AggregateResult::new_err(
                            DiagnosticBuilder::new(span)
                                .build_incompatible_global_def(&int_ty, to_ty),
                        )
                    }
                }
            }
        }
    }
}

fn extract_global_var_initializer(
    expr_node: &ast::ExpressionNode,
    settings: &Settings,
) -> AggregateResult<ir::Constant> {
    match &expr_node.data {
        ast::Expression::Literal(lit) => {
            literal(lit, settings).map(|expr_node| match expr_node.expr {
                ir::Expr::Constant(constant) => constant,
                _ => unreachable!("ICE: ast literals can only be mapped to ir constants"),
            })
        }
        // TODO: allow more constant expressions than only literals
        _ => AggregateResult::new_err(
            DiagnosticBuilder::new(expr_node.span).build_non_const_global_initializer(),
        ),
    }
}

/// Merge of [`ast::FunctionDeclaration`] and [`ast::FunctionDefinition`].
#[derive(Debug, Clone)]
struct AstFunction<'a> {
    pub prototype_span: Span,
    pub comments: Option<&'a str>,
    pub return_type: &'a ast::QualifiedTypeNode,
    pub ident: &'a ast::IdentNode,
    pub params: &'a [ast::FunctionParamNode],
    pub is_vararg: bool,
    pub body: Option<&'a ast::BlockStatementNode>,
}

fn add_function(
    function: AstFunction,
    global: &mut ir::Root,
    settings: &Settings,
) -> AggregateResult<()> {
    let return_type = CType::from_ast_type(&function.return_type.unqualified.data);
    let ident = &function.ident.data;

    let res = check_function_ident(&function, &return_type, global);

    let mut function_table = ScopedTable::new();
    let mut function_scope = FunctionScope {
        global,
        vars: function_table.get_scoped_handle(),
        func_return_type: (function.return_type.span, &return_type),
        in_switch: false,
        in_loop: false,
    };

    let res = res
        .zip(function_params(function.params, &mut function_scope))
        .zip(AggregateResult::transpose_from(function.body.map(|body| {
            build_ir_from_block(body, settings, &mut function_scope)
        })));

    std::mem::drop(function_scope);

    let table = function_table.into_table();

    res.map(|((should_redefine, params), body)| {
        if should_redefine {
            global.functions.insert(
                ident.clone(),
                ir::FunctionNode {
                    // Preserve the span of the first declaration/definition.
                    original_span: global
                        .functions
                        .get(ident)
                        .map(|f| f.original_span)
                        .unwrap_or(function.prototype_span),
                    comments: function.comments.map(String::from),
                    return_type,
                    params,
                    is_vararg: function.is_vararg,
                    body,
                    table,
                },
            );
        }
    })
}

pub fn function_params(
    params: &[ast::FunctionParamNode],
    scope: &mut FunctionScope,
) -> AggregateResult<Vec<ir::FunctionParamNode>> {
    let mut res = AggregateResult::new_ok(Vec::new());
    for param in params {
        declaration_type(&param.type_name, &param.array_parts)
            .and_then(|ty| {
                if matches!(ty.ty, CType::Void) {
                    return AggregateResult::new_err(
                        DiagnosticBuilder::new(param.type_name.span).build_void_param(),
                    );
                }
                AggregateResult::new_ok(ty)
            })
            .and_then(
                |DeclarationType {
                     ty,
                     is_const,
                     needs_address,
                 }| {
                    param
                        .ident
                        .as_ref()
                        .map(|ident| {
                            let item = VariableItem {
                                original_span: param.span,
                                ty: ty.clone(),
                                is_const,
                                needs_address,
                                // params will be initialized by the arguments passsed to a function call
                                initialized: true,
                            };
                            match scope.vars.declare(ident.data.clone(), item) {
                                Ok(id) => AggregateResult::new_ok(Some(id)),
                                Err(id) => {
                                    let original_span =
                                        scope.vars.root_table().get(id).original_span;
                                    AggregateResult::new_err(
                                        DiagnosticBuilder::new(ident.span)
                                            .build_already_defined(&ident.data, original_span),
                                    )
                                }
                            }
                        })
                        .unwrap_or(AggregateResult::new_ok(None))
                        .map(|ident| ir::FunctionParamNode {
                            span: param.span,
                            ty,
                            is_const,
                            ident,
                        })
                },
            )
            .add_to(&mut res, |res, d| res.push(d));
    }
    res
}

/// Returns `true` if the function isn't already declared/defined (or a declaration already exists,
/// and this a definition of the same function). Returns `false` if the function is already
/// declared/defined but this is just an additional equivalent declaration. Otherwise the
/// appropriate diagnostics will be added to the returned _err_ result.
fn check_function_ident(
    function: &AstFunction,
    return_type: &CType,
    global: &mut ir::Root,
) -> AggregateResult<bool> {
    let ident = &function.ident.data;

    if let Some(&ir::GlobalVarNode { original_span, .. }) = global.vars.get(ident) {
        return AggregateResult::new_err(
            DiagnosticBuilder::new(function.ident.span)
                .build_func_def_with_name_of_var(ident, original_span),
        );
    }

    let Some(original_function) = global.functions.get_mut(ident) else {
        return AggregateResult::new_ok(true);
    };

    let mut res = AggregateResult::new_ok(false);

    if &original_function.return_type != return_type {
        res.add_err(
            DiagnosticBuilder::new(function.ident.span).build_func_redec_with_different_return(
                ident,
                return_type,
                original_function.original_span,
                &original_function.return_type,
            ),
        );
    }

    let param_types = function
        .params
        .iter()
        .map(|p| CType::from_ast_type(&p.type_name.unqualified.data));

    if original_function.params.len() != param_types.len()
        || original_function.is_vararg != function.is_vararg
        || original_function
            .params
            .iter()
            .zip(param_types)
            .any(|(p, ty)| p.ty != ty)
    {
        res.add_err(
            DiagnosticBuilder::new(function.ident.span)
                .build_func_redec_with_different_parms(ident, original_function.original_span),
        );
    }

    let is_declaration = function.body.is_none();
    let should_redefine = match (original_function.is_declaration(), is_declaration) {
        // Multiple equivalent declarations are ok. Preserve the first.
        (true, true) => false,
        // Definition of forward-declared function is ok.
        (true, false) => true,
        // Redeclaration of already defined function is ok.
        (false, true) => false,
        // Multiple definitions (even if identical) are not ok.
        (false, false) => {
            res.add_err(
                DiagnosticBuilder::new(function.ident.span)
                    .build_function_already_defined(ident, original_function.original_span),
            );
            return res;
        }
    };

    // To limit the loss of information, comments of all the declarations/definition of a
    // function are merged.
    merge_comments(&mut original_function.comments, function.comments);

    res.map(|_| should_redefine)
}

fn merge_comments(original_comments: &mut Option<String>, extra_comments: Option<&str>) {
    match (original_comments, extra_comments) {
        (Some(original_comments), Some(comments)) => {
            original_comments.push('\n');
            original_comments.push_str(comments);
        }
        (original_comments @ None, Some(comments)) => {
            *original_comments = Some(String::from(comments));
        }
        // Nothing to do if there're no extra comments
        (_, None) => {}
    }
}

/// Returns `true` if the global var isn't already declared/defined (or a compatible declaration
/// already exists). Returns `false` if the function is already declared/defined but this is just an
/// additional equivalent declaration. Otherwise the appropriate diagnostics will be added to the
/// returned _err_ result.
fn check_global_var_ident(
    ext_decl: &AstGlobalVar,
    global_var: &ir::GlobalVarNode,
    global: &mut ir::Root,
) -> AggregateResult<bool> {
    let ident = &ext_decl.decl.ident.data;
    let ident_span = ext_decl.decl.ident.span;

    if let Some(&ir::FunctionNode { original_span, .. }) = global.functions.get(ident) {
        return AggregateResult::new_err(
            DiagnosticBuilder::new(ident_span)
                .build_var_def_with_name_of_func(ident, original_span),
        );
    }

    let Some(original_var) = global.vars.get_mut(ident) else {
        return AggregateResult::new_ok(true);
    };

    let mut res = AggregateResult::new_ok(false);

    if original_var.ty != global_var.ty {
        res.add_err(
            DiagnosticBuilder::new(ident_span).build_var_redec_with_different_type(
                ident,
                &global_var.ty,
                original_var.original_span,
                &original_var.ty,
            ),
        )
    } else if original_var.is_const != global_var.is_const {
        res.add_err(
            DiagnosticBuilder::new(ident_span).build_var_redec_with_different_constness(
                ident,
                original_var.original_span,
                global_var.is_const,
            ),
        )
    }

    let should_redefine = match (original_var.is_declaration(), global_var.is_declaration()) {
        // Multiple equivalent declarations are ok. Preserve the first.
        (true, true) => false,
        // Definition of previously declared variable is ok.
        (true, false) => true,
        // Redeclaration of already defined variable is ok.
        (false, true) => false,
        // Multiple definitions (even if identical) are not ok.
        (false, false) => {
            res.add_err(
                DiagnosticBuilder::new(ident_span)
                    .build_global_var_already_defined(ident, original_var.original_span),
            );
            return res;
        }
    };

    // To limit the loss of information, comments of all the declarations/definition of a
    // global variable are merged.
    merge_comments(&mut original_var.comments, global_var.comments.as_deref());

    res.map(|_| should_redefine)
}
