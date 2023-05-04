use super::{
    expr::literal,
    stmt::{build_ir_from_block, declaration_type},
    symbol_table::ScopedTable,
    util::{DeclarationType, FunctionScope},
};
use crate::ast;
use crate::diagnostic::{AggregateResult, DiagnosticBuilder, Span};
use crate::ir::{self, ctype::CType, table::VariableItem};

pub fn build_ir_from_external_declaration(
    external_declaration: &ast::ExternalDeclarationNode,
    global: &mut ir::Root,
) -> AggregateResult<()> {
    let span = external_declaration.span;
    match &external_declaration.data {
        ast::ExternalDeclaration::Declaration(ast::Declaration::Variable(decl)) => {
            declaration_type(&decl.type_name, &decl.array_parts)
                .and_then(|ty| {
                    if matches!(ty.ty, CType::Void) {
                        return AggregateResult::new_err(
                            DiagnosticBuilder::new(decl.type_name.span).build_void_vars(),
                        );
                    }
                    AggregateResult::new_ok(ty)
                })
                .zip(AggregateResult::transpose_from(
                    // TODO: use first span of initializer to point to equals sign to provide
                    // clearer diagnostics (i.e. expected constant to initialize global ...).
                    decl.initializer
                        .as_ref()
                        .map(|(_, expr_node)| extract_global_var_initializer(expr_node)),
                ))
                .map(|(DeclarationType { ty, is_const }, constant)| {
                    // TODO: use expr::assign here
                    ir::GlobalVarNode {
                        original_span: external_declaration.span,
                        comments: external_declaration.comments.clone(),
                        ty,
                        is_const,
                        value: constant,
                    }
                })
                .and_then(|global_var| {
                    if let Some(&ir::FunctionNode { original_span, .. }) =
                        global.functions.get(&decl.ident.data)
                    {
                        // TODO: customized diagnostic message mentioning a variable was expected but a function
                        // with the same name already existed in the global scope.
                        return AggregateResult::new_err(
                            DiagnosticBuilder::new(decl.ident.span)
                                .build_already_defined(&decl.ident.data, original_span),
                        );
                    }
                    if let Some(&ir::GlobalVarNode { original_span, .. }) =
                        global.vars.get(&decl.ident.data)
                    {
                        return AggregateResult::new_err(
                            DiagnosticBuilder::new(decl.ident.span)
                                .build_already_defined(&decl.ident.data, original_span),
                        );
                    }
                    global.vars.insert(decl.ident.data.clone(), global_var);
                    AggregateResult::new_ok(())
                })
        }
        ast::ExternalDeclaration::Declaration(ast::Declaration::FunctionDeclaration(fd)) => {
            let function = AstFunction {
                span,
                comments: external_declaration.comments.as_deref(),
                return_type: &fd.return_type,
                ident: &fd.ident,
                params: &fd.params,
                is_vararg: fd.is_vararg,
                body: None,
            };
            add_function(function, global)
        }
        ast::ExternalDeclaration::FunctionDefinition(fd) => {
            let function = AstFunction {
                span,
                comments: external_declaration.comments.as_deref(),
                return_type: &fd.return_type,
                ident: &fd.ident,
                params: &fd.params,
                is_vararg: fd.is_vararg,
                body: Some(&fd.body),
            };
            add_function(function, global)
        }
    }
}

fn extract_global_var_initializer(
    expr_node: &ast::ExpressionNode,
) -> AggregateResult<ir::Constant> {
    match &expr_node.data {
        ast::Expression::Literal(lit) => literal(lit).map(|expr_node| match expr_node.expr {
            ir::Expr::Constant(constant) => constant,
            _ => unreachable!("ICE: ast literals can only be mapped to ir constants"),
        }),
        // TODO: allow more constant expressions than only literals
        _ => AggregateResult::new_err(
            DiagnosticBuilder::new(expr_node.span).build_non_const_global_initializer(),
        ),
    }
}

/// Merge of [`ast::FunctionDeclaration`] and [`ast::FunctionDefinition`].
#[derive(Debug)]
struct AstFunction<'a> {
    pub span: Span,
    pub comments: Option<&'a str>,
    pub return_type: &'a ast::QualifiedTypeNode,
    pub ident: &'a ast::IdentNode,
    pub params: &'a [ast::FunctionParamNode],
    pub is_vararg: bool,
    pub body: Option<&'a ast::BlockStatementNode>,
}

fn add_function(function: AstFunction, global: &mut ir::Root) -> AggregateResult<()> {
    let return_type = CType::from_ast_type(&function.return_type.data.inner.data);
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
        .zip(AggregateResult::transpose_from(
            function
                .body
                .map(|body| build_ir_from_block(body, &mut function_scope)),
        ));

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
                        .unwrap_or(function.span),
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
            .and_then(|DeclarationType { ty, is_const }| {
                param
                    .ident
                    .as_ref()
                    .map(|ident| {
                        let item = VariableItem {
                            original_span: param.span,
                            ty: ty.clone(),
                            is_const,
                            // params will be initialized by the arguments passsed to a function call
                            initialized: true,
                        };
                        match scope.vars.declare(ident.data.clone(), item) {
                            Ok(id) => AggregateResult::new_ok(Some(id)),
                            Err(id) => {
                                let original_span = scope.vars.root_table().get(id).original_span;
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
            })
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
        // TODO: customized diagnostic message mentioning a function was expected but a variable
        // with the same name already existed in the global scope.
        return AggregateResult::new_err(
            DiagnosticBuilder::new(function.ident.span).build_already_defined(ident, original_span),
        );
    }

    let Some(original_function) = global.functions.get_mut(ident) else {
        return AggregateResult::new_ok(true);
    };

    let mut res = AggregateResult::new_ok(false);

    if &original_function.return_type != return_type {
        // TODO: customize diagnostics message
        res.add_err(
            DiagnosticBuilder::new(function.ident.span)
                .build_already_defined(ident, original_function.original_span),
        );
    }

    let param_types = function
        .params
        .iter()
        .map(|p| CType::from_ast_type(&p.type_name.data.inner.data));

    if original_function
        .params
        .iter()
        .zip(param_types)
        .any(|(p, ty)| p.ty != ty)
    {
        // TODO: customize diagnostics message
        res.add_err(
            DiagnosticBuilder::new(function.ident.span)
                .build_already_defined(ident, original_function.original_span),
        );
    }

    if original_function.is_vararg != function.is_vararg {
        // TODO: customize diagnostics message
        res.add_err(
            DiagnosticBuilder::new(function.ident.span)
                .build_already_defined(ident, original_function.original_span),
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
            // TODO: customize diagnostic to mention that function may only be _defined_ once.
            res.add_err(
                DiagnosticBuilder::new(function.ident.span)
                    .build_already_defined(ident, original_function.original_span),
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
