mod build_type;

use crate::{
    ast, cst,
    diagnostic::{AggregateResult, DiagnosticBuilder, Span},
    generated,
};
use antlr_rust::{
    parser_rule_context::{BaseParserRuleContext, ParserRuleContext},
    rule_context::CustomRuleContext,
    token::Token,
    tree::ParseTree,
    TidAble,
};
use std::{ops::Deref, rc::Rc, sync::atomic::Ordering};

pub fn lower(cst: &cst::Cst<'_>) -> AggregateResult<ast::Ast> {
    AstBuilder::new(&cst.token_stream).build_from_translation_unit(&cst.translation_unit)
}

fn tree_error<'a, Ctx>(_ectx: &BaseParserRuleContext<'a, Ctx>) -> !
where
    Ctx: CustomRuleContext<'a> + TidAble<'a>,
{
    // We should never get here since the ErrorListener should have caught the error already
    panic!("ICE: Error inside ANTLR tree");
}

fn extract_span<'a>(ctx: &impl ParserRuleContext<'a>) -> Span {
    let start: usize = ctx.start().get_start().try_into().unwrap();
    let end: usize = (ctx.stop().get_stop() + 1).try_into().unwrap();

    Span::from(start..end)
}

fn extract_span_from_token(token: &impl Token) -> Span {
    let start: usize = token.get_start().try_into().unwrap();
    let end: usize = (token.get_stop() + 1).try_into().unwrap();

    Span::from(start..end)
}

macro_rules! build_from_generic_binary_op {
    (
        $vis:vis $from:ident($from_type:tt) {
            $singular:tt => $next:ident,
            $composed:tt => let $text:pat => $expr:expr
        }
    ) => {
        $vis fn $from(&self, ctx: &cst::$from_type) -> AggregateResult<ast::ExpressionNode> {
            let data = match ctx {
                cst::$from_type::$singular(singular) => return self.$next(singular.value.as_deref().unwrap()),
                cst::$from_type::$composed(composed) => {
                    self.$from(composed.lhs.as_deref().unwrap())
                        .zip(self.$next(composed.rhs.as_deref().unwrap()))
                        .map(|(first, sec)| {
                            let op = {
                                let $text = composed.op.as_deref().unwrap();
                                $expr
                            };
                            ast::Expression::Binary(Box::new(first), op, Box::new(sec))
                        })
                }
                cst::$from_type::Error(ectx) => tree_error(ectx),
            };

            data.map(|data| {
                ast::ExpressionNode {
                    span: extract_span(ctx),
                    data,
                }
            })
        }
    };
}

macro_rules! build_from_single_binary_op {
    (
        $vis:vis $from:ident($from_type:tt) {
            $singular:tt => $next:ident,
            $composed:tt => $op:ident
        }
    ) => {
        build_from_generic_binary_op! {
            $vis $from($from_type) {
                $singular => $next,
                $composed => let ctx => {
                    ast::BinaryOperatorNode {
                        span: extract_span_from_token(ctx),
                        data: ast::BinaryOperator::$op,
                    }
                }
            }
        }
    };
}

macro_rules! build_from_multi_binary_op {
    (
        $vis:vis $from:ident($from_type:tt) {
            $singular:tt => $next:ident,
            $composed:tt => match $op_tok:ident {
                $($tok_type:ident => $op:ident),* $(,)?
            }
        }
    ) => {
        build_from_generic_binary_op! {
            $vis $from($from_type) {
                $singular => $next,
                $composed => let $op_tok => {
                    let data = match $op_tok.token_type {
                        $(generated::clexer::$tok_type => ast::BinaryOperator::$op,)*
                        _ => unreachable!(),
                    };
                    ast::BinaryOperatorNode {
                        span: extract_span_from_token($op_tok),
                        data,
                    }
                }
            }
        }
    };
}

pub struct AstBuilder<'a, 'b> {
    input: &'a cst::TokenStream<'b>,
}

impl<'a, 'b> AstBuilder<'a, 'b> {
    pub fn new(input: &'a cst::TokenStream<'b>) -> Self {
        Self { input }
    }

    fn extract_comments(&self, token_index: isize) -> Option<String> {
        let comment_tokens = self
            .input
            .get_hidden_tokens_to_left(token_index, generated::clexer::COMMENTS as isize);

        if comment_tokens.is_empty() {
            None
        } else {
            Some(join_comment_tokens(comment_tokens.into_iter()))
        }
    }

    pub fn build_from_translation_unit(
        self,
        ctx: &cst::TranslationUnit,
    ) -> AggregateResult<ast::Ast> {
        let mut res = AggregateResult::new_ok(Vec::with_capacity(ctx.content.len()));

        for statement in &ctx.content {
            use cst::ExternalDeclaration;
            match statement.as_ref() {
                ExternalDeclaration::ExternalDeclarationStatementContext(decl) => self
                    .build_from_declaration_statement(decl.value.as_deref().unwrap())
                    .map(|data| ast::ExternalDeclarationNode {
                        span: extract_span(decl),
                        data: ast::ExternalDeclaration::Declaration(data),
                        comments: None,
                    })
                    .add_to(&mut res, |r, s| r.push(s)),
                ExternalDeclaration::ExternalDeclarationFunctionDefinitionContext(func) => self
                    .build_from_function_definition(func.value.as_deref().unwrap())
                    .map(|data| ast::ExternalDeclarationNode {
                        span: extract_span(func),
                        data: ast::ExternalDeclaration::FunctionDefinition(data),
                        comments: None,
                    })
                    .add_to(&mut res, |r, s| r.push(s)),
                ExternalDeclaration::ExternalDeclarationIncludeContext(incl) => {
                    let start = extract_span(incl).start();
                    let len = incl.get_text().trim_end().len();
                    let span = (start..(start + len)).into();
                    AggregateResult::new_ok(generate_printf_from_include_stdio(span))
                        .add_to(&mut res, |r, s| r.push(s));
                    AggregateResult::new_ok(generate_scanf_from_include_stdio(span))
                        .add_to(&mut res, |r, s| r.push(s));
                }
                ExternalDeclaration::Error(ectx) => tree_error(ectx),
            }
        }

        res.map(|mut global_declarations| {
            global_declarations.shrink_to_fit();
            ast::Ast {
                global_declarations,
            }
        })
    }

    fn build_from_block_item(
        &self,
        ctx: &cst::BlockItem,
    ) -> AggregateResult<Option<ast::StatementNode>> {
        use cst::BlockItem;
        match ctx {
            BlockItem::BlockItemDeclarationContext(decl) => self
                .build_from_declaration_statement(decl.value.as_deref().unwrap())
                .map(|data| {
                    Some(ast::StatementNode {
                        span: extract_span(ctx),
                        data: ast::Statement::Declaration(data),
                        comments: self
                            .extract_comments(ctx.start().token_index.load(Ordering::Relaxed)),
                    })
                }),
            BlockItem::BlockItemStatementContext(stmt) => self
                .build_from_statement(stmt.value.as_deref().unwrap())
                .map(|v| v.ok()),
            BlockItem::Error(ectx) => tree_error(ectx),
        }
    }

    fn build_from_statement(
        &self,
        ctx: &cst::Statement,
    ) -> AggregateResult<Result<ast::StatementNode, Span>> {
        use cst::Statement;
        let data = match ctx {
            Statement::StatementExprContext(expr) => self
                .build_from_expr(expr.value.as_deref().unwrap())
                .map(ast::Statement::Expression),
            Statement::StatementSelectionContext(stmt) => {
                self.build_from_selection_statement(stmt.value.as_deref().unwrap())
            }
            Statement::StatementIterationContext(stmt) => {
                self.build_from_iteration_statement(stmt.value.as_deref().unwrap())
            }
            Statement::StatementJumpContext(stmt) => {
                self.build_from_jump_statement(stmt.value.as_deref().unwrap())
            }
            Statement::StatementBlockContext(block) => self
                .build_from_block_statement(block.value.as_deref().unwrap())
                .map(ast::Statement::BlockStatement),
            Statement::StatementEmptyContext(_) => {
                return AggregateResult::new_ok(Err(extract_span(ctx)))
            }
            Statement::Error(ectx) => tree_error(ectx),
        };

        let comments = self.extract_comments(ctx.start().token_index.load(Ordering::Relaxed));

        data.map(|data| {
            Ok(ast::StatementNode {
                span: extract_span(ctx),
                data,
                comments,
            })
        })
    }

    fn build_from_selection_statement(
        &self,
        ctx: &cst::SelectionStatement,
    ) -> AggregateResult<ast::Statement> {
        use cst::SelectionStatement;
        match ctx {
            SelectionStatement::SelectionStatementIfContext(ctx) => self
                .build_from_if_statement(ctx.value.as_deref().unwrap())
                .map(ast::Statement::If),
            SelectionStatement::SelectionStatementSwitchContext(ctx) => self
                .build_form_switch_statement(ctx.value.as_deref().unwrap())
                .map(ast::Statement::Switch),
            SelectionStatement::Error(ectx) => tree_error(ectx),
        }
    }

    fn build_from_if_statement(&self, ctx: &cst::IfStatement) -> AggregateResult<ast::IfStatement> {
        use cst::IfStatement;
        match ctx {
            IfStatement::IfStatementIfContext(ctx) => self
                .build_from_expr(ctx.value.as_deref().unwrap())
                .zip(self.build_from_statement(ctx.body.as_deref().unwrap()))
                .map(|(condition, if_body)| ast::IfStatement {
                    condition,
                    if_body: to_block_statements(if_body),
                    else_body: None,
                }),
            IfStatement::IfStatementIfElseContext(ctx) => self
                .build_from_expr(ctx.value.as_deref().unwrap())
                .zip(self.build_from_statement(ctx.body_if.as_deref().unwrap()))
                .zip(self.build_from_statement(ctx.body_else.as_deref().unwrap()))
                .map(|((condition, if_body), else_body)| ast::IfStatement {
                    condition,
                    if_body: to_block_statements(if_body),
                    else_body: Some(to_block_statements(else_body)),
                }),
            IfStatement::Error(ectx) => tree_error(ectx),
        }
    }

    fn build_form_switch_statement(
        &self,
        ctx: &cst::SwitchStatement,
    ) -> AggregateResult<ast::SwitchStatement> {
        fn build_case_body<'a, 'input, I>(
            builder: &AstBuilder,
            span: Span,
            stmt: I,
        ) -> AggregateResult<ast::BlockStatementNode>
        where
            I: Iterator<Item = &'a Rc<cst::Statement<'input>>>,
            'input: 'a,
        {
            let mut res = AggregateResult::new_ok(Vec::new());

            for block_item in stmt {
                builder
                    .build_from_statement(block_item.deref())
                    .add_to(&mut res, |v, s| {
                        if let Ok(s) = s {
                            v.push(s)
                        }
                    });
            }

            res.map(|stmts| ast::BlockStatementNode { span, stmts })
        }

        let mut cases = AggregateResult::new_ok(Vec::new());
        let mut default_case = None;
        for case in &ctx.body {
            match case.deref() {
                cst::SwitchCase::SwitchCaseExprContext(case) => {
                    let label = case.label.as_deref().unwrap();
                    self.build_from_expr(label.value.as_deref().unwrap())
                        .zip(build_case_body(self, extract_span(case), case.body.iter()))
                        .map(|(expr, body)| {
                            ast::SwitchCase::Expr(ast::SwithCaseExprNode {
                                label_span: extract_span(label),
                                expr,
                                body,
                            })
                        })
                        .add_to(&mut cases, |res, v| res.push(v))
                }
                cst::SwitchCase::SwitchCaseDefaultContext(case) => {
                    let span = extract_span_from_token(case.label.as_deref().unwrap());
                    if let Some(other_default_span) = default_case {
                        cases.add_err(
                            DiagnosticBuilder::new(span)
                                .build_multiple_defaults(other_default_span),
                        );
                    } else {
                        default_case = Some(span);
                        build_case_body(self, extract_span(case), case.body.iter())
                            .map(|body| {
                                ast::SwitchCase::Default(ast::SwitchCaseDefaultNode {
                                    label_span: span,
                                    body,
                                })
                            })
                            .add_to(&mut cases, |res, v| res.push(v))
                    }
                }
                cst::SwitchCase::Error(ectx) => tree_error(ectx),
            }
        }

        cases
            .zip(self.build_from_expr(ctx.value.as_deref().unwrap()))
            .map(|(cases, expr)| ast::SwitchStatement { expr, cases })
    }

    fn build_from_iteration_statement(
        &self,
        ctx: &cst::IterationStatement,
    ) -> AggregateResult<ast::Statement> {
        use cst::IterationStatement;
        match ctx {
            IterationStatement::IterationStatementWhileContext(ctx) => self
                .build_from_while_statement(ctx.value.as_deref().unwrap())
                .map(ast::Statement::While),
            IterationStatement::IterationStatementForContext(ctx) => self
                .build_from_for_statement(ctx.value.as_deref().unwrap())
                .map(ast::Statement::For),
            IterationStatement::Error(ectx) => tree_error(ectx),
        }
    }

    fn build_from_while_statement(
        &self,
        ctx: &cst::WhileStatement,
    ) -> AggregateResult<ast::WhileStatement> {
        self.build_from_expr(ctx.value.as_deref().unwrap())
            .zip(self.build_from_statement(ctx.body.as_deref().unwrap()))
            .map(|(condition, body)| ast::WhileStatement {
                condition,
                body: to_block_statements(body),
            })
    }

    fn build_from_for_statement(
        &self,
        ctx: &cst::ForStatement,
    ) -> AggregateResult<ast::ForStatement> {
        use cst::ForStatement;

        fn map_transpose<T, U, F>(v: Option<T>, map: F) -> AggregateResult<Option<U>>
        where
            F: FnOnce(T) -> AggregateResult<U>,
        {
            match v {
                Some(t) => map(t).map(Some),
                None => AggregateResult::new_ok(None),
            }
        }

        let (res, cond, iter, body) = match ctx {
            ForStatement::ForStatementDeclContext(ctx) => (
                map_transpose(ctx.init.as_deref(), |init| {
                    self.build_from_declaration_statement(init)
                        .map(|init| ast::StatementNode {
                            span: extract_span(ctx.init.as_deref().unwrap()),
                            data: ast::Statement::Declaration(init),
                            comments: None,
                        })
                }),
                ctx.cond.as_deref(),
                ctx.iter.as_deref(),
                ctx.body.as_deref().unwrap(),
            ),

            ForStatement::ForStatementExprContext(ctx) => (
                map_transpose(ctx.init.as_deref(), |init| {
                    self.build_from_expr(init).map(|init| ast::StatementNode {
                        span: extract_span(ctx.init.as_deref().unwrap()),
                        data: ast::Statement::Expression(init),
                        comments: None,
                    })
                }),
                ctx.cond.as_deref(),
                ctx.iter.as_deref(),
                ctx.body.as_deref().unwrap(),
            ),
            ForStatement::Error(ectx) => tree_error(ectx),
        };
        res.zip(map_transpose(cond, |v| self.build_from_expr(v)))
            .zip(map_transpose(iter, |v| self.build_from_expr(v)))
            .zip(self.build_from_statement(body))
            .map(|(((init, condition), iter), body)| ast::ForStatement {
                init: init.map(Box::new),
                condition,
                iter,
                body: to_block_statements(body),
            })
    }

    fn build_from_jump_statement(
        &self,
        ctx: &cst::JumpStatement,
    ) -> AggregateResult<ast::Statement> {
        use cst::JumpStatement;
        match ctx {
            JumpStatement::JumpStatementBreakContext(_) => {
                AggregateResult::new_ok(ast::Statement::Break)
            }
            JumpStatement::JumpStatementContinueContext(_) => {
                AggregateResult::new_ok(ast::Statement::Continue)
            }
            JumpStatement::JumpStatementReturnContext(ctx) => {
                let op_span = extract_span_from_token(ctx.op.as_deref().unwrap());
                match ctx.value.as_deref() {
                    Some(expr) => self
                        .build_from_expr(expr)
                        .map(|expr| ast::Statement::Return(op_span, Some(expr))),
                    None => AggregateResult::new_ok(ast::Statement::Return(op_span, None)),
                }
            }
            JumpStatement::Error(ectx) => tree_error(ectx),
        }
    }

    fn build_from_declaration_statement(
        &self,
        ctx: &cst::DeclarationStatement,
    ) -> AggregateResult<ast::Declaration> {
        use cst::DeclarationStatement;
        match ctx {
            DeclarationStatement::DeclarationStatementWithoutInitializerContext(decl) => self
                .build_from_type_name(decl.type_name.as_deref().unwrap())
                .zip(self.build_from_identifier(decl.ident.as_deref().unwrap()))
                .zip(self.build_from_array_declarations(&decl.array))
                .map(|((type_name, ident), is_array)| {
                    ast::Declaration::Variable(ast::VariableDeclaration {
                        type_name,
                        ident,
                        array_parts: is_array,
                        initializer: None,
                    })
                }),
            DeclarationStatement::DeclarationStatementWithInitializerContext(decl) => self
                .build_from_type_name(decl.type_name.as_deref().unwrap())
                .zip(self.build_from_identifier(decl.ident.as_deref().unwrap()))
                .zip(self.build_from_array_declarations(&decl.array))
                .zip(self.build_from_expr(decl.rhs.as_deref().unwrap()))
                .map(|(((type_name, ident), is_array), initializer)| {
                    let op_span = extract_span_from_token(decl.op.as_deref().unwrap());
                    ast::Declaration::Variable(ast::VariableDeclaration {
                        type_name,
                        ident,
                        array_parts: is_array,
                        initializer: Some((op_span, initializer)),
                    })
                }),
            DeclarationStatement::DeclarationStatementFunctionDeclarationContext(decl) => self
                .build_from_function_declaration(decl.value.as_deref().unwrap())
                .map(ast::Declaration::FunctionDeclaration),
            DeclarationStatement::Error(ectx) => tree_error(ectx),
        }
    }

    fn build_from_array_declarations(
        &self,
        ctxs: &Vec<Rc<cst::ArrayDeclaration>>,
    ) -> AggregateResult<Vec<ast::ArrayDeclarationNode>> {
        use cst::ArrayDeclaration;

        let mut res = AggregateResult::new_ok(Vec::new());

        for ctx in ctxs {
            let ctx = ctx.deref();
            let part = match ctx {
                ArrayDeclaration::ArrayDeclarationPlainContext(_) => {
                    AggregateResult::new_ok(ast::ArrayDeclaration::Unknown)
                }
                ArrayDeclaration::ArrayDeclarationExprContext(ctx) => self
                    .build_from_cond_expr(ctx.value.as_deref().unwrap())
                    .map(ast::ArrayDeclaration::Known),
                ArrayDeclaration::Error(ectx) => tree_error(ectx),
            };
            part.map(|part| ast::ArrayDeclarationNode {
                span: extract_span(ctx),
                data: part,
            })
            .add_to(&mut res, |res, p| res.push(p));
        }
        res
    }

    fn build_from_function_declaration(
        &self,
        ctx: &cst::FunctionDeclaration,
    ) -> AggregateResult<ast::FunctionDeclaration> {
        self.build_from_prototype(ctx.prototype.as_deref().unwrap())
            .map(
                |(return_type, ident, params, is_vararg)| ast::FunctionDeclaration {
                    return_type,
                    ident,
                    params,
                    is_vararg,
                },
            )
    }

    fn build_from_function_definition(
        &self,
        ctx: &cst::FunctionDefinition,
    ) -> AggregateResult<ast::FunctionDefinition> {
        self.build_from_prototype(ctx.prototype.as_deref().unwrap())
            .zip(self.build_from_block_statement(ctx.body.as_deref().unwrap()))
            .map(
                |((return_type, ident, params, is_vararg), body)| ast::FunctionDefinition {
                    prototype_span: extract_span(ctx.prototype.as_deref().unwrap()),
                    return_type,
                    ident,
                    params,
                    is_vararg,
                    body,
                },
            )
    }

    fn build_from_prototype(
        &self,
        ctx: &cst::FunctionPrototype,
    ) -> AggregateResult<(
        ast::QualifiedTypeNode,      // return type
        ast::IdentNode,              // ident
        Vec<ast::FunctionParamNode>, // params
        bool,                        // is_vararg
    )> {
        self.build_from_type_name(ctx.type_name.as_deref().unwrap())
            .zip(self.build_from_identifier(ctx.ident.as_deref().unwrap()))
            .zip({
                let mut res = AggregateResult::new_ok(Vec::new());
                for param in &ctx.params {
                    self.build_from_function_param(param)
                        .add_to(&mut res, |r, p| r.push(p));
                }
                res
            })
            .map(|((return_type, ident), params)| {
                (return_type, ident, params, ctx.varargs.is_some())
            })
    }

    fn build_from_function_param(
        &self,
        ctx: &cst::FunctionParam,
    ) -> AggregateResult<ast::FunctionParamNode> {
        self.build_from_type_name(ctx.type_name.as_deref().unwrap())
            .zip(match ctx.ident.as_deref() {
                Some(ident) => self.build_from_identifier(ident).map(Some),
                None => AggregateResult::new_ok(None),
            })
            .zip(self.build_from_array_declarations(&ctx.array))
            .map(|((type_name, ident), array_parts)| ast::FunctionParamNode {
                span: extract_span(ctx),
                type_name,
                ident,
                array_parts,
            })
    }

    fn build_from_block_statement(
        &self,
        ctx: &cst::BlockStatement,
    ) -> AggregateResult<ast::BlockStatementNode> {
        let mut res = AggregateResult::new_ok(Vec::with_capacity(ctx.content.len()));

        for block_item in &ctx.content {
            self.build_from_block_item(block_item)
                .add_to(&mut res, |res, stmt| {
                    if let Some(stmt) = stmt {
                        res.push(stmt)
                    }
                });
        }

        res.map(|mut stmts| {
            stmts.shrink_to_fit();
            ast::BlockStatementNode {
                span: extract_span(ctx),
                stmts,
            }
        })
    }

    #[allow(clippy::only_used_in_recursion)]
    fn build_from_type_name(&self, ctx: &cst::TypeName) -> AggregateResult<ast::QualifiedTypeNode> {
        use cst::TypeName as TN;

        let span = extract_span(ctx);

        let data = match ctx {
            TN::TypeNamePlainContext(plain) => {
                build_type::build_from_specifiers(span, &plain.specifiers).and_then(|primitive| {
                    let unqualified_type_node = ast::UnqualifiedTypeNode {
                        span: extract_span(plain),
                        data: ast::UnqualifiedType::PlainType(ast::PlainType::Primitive(primitive)),
                    };
                    with_qualifiers(unqualified_type_node, &plain.qualifiers)
                })
            }
            TN::TypeNamePointerContext(ctx) => self
                .build_from_type_name(ctx.inner.as_deref().unwrap())
                .and_then(|data| {
                    let unqualified_type_node = ast::UnqualifiedTypeNode {
                        span: extract_span(ctx),
                        data: ast::UnqualifiedType::PointerType(Box::new(data)),
                    };
                    with_qualifiers(unqualified_type_node, &ctx.ptr_qualifiers)
                }),
            TN::Error(ectx) => tree_error(ectx),
        };

        data.map(|data| ast::QualifiedTypeNode { span, data })
    }

    pub fn build_from_expr(&self, ctx: &cst::Expr) -> AggregateResult<ast::ExpressionNode> {
        self.build_from_assign_expr(ctx.value.as_deref().unwrap())
    }

    pub fn build_from_assign_expr(
        &self,
        ctx: &cst::AssignExpr,
    ) -> AggregateResult<ast::ExpressionNode> {
        use cst::AssignExpr;
        let data = match ctx {
            AssignExpr::AssignExprSingularContext(singular) => {
                return self.build_from_cond_expr(singular.value.as_deref().unwrap())
            }
            AssignExpr::AssignExprComposedContext(composed) => self
                .build_from_unary_expr(composed.lhs.as_deref().unwrap())
                .zip(self.build_from_assign_expr(composed.rhs.as_deref().unwrap()))
                .map(|(lhs, rhs)| {
                    let op = composed.op.as_deref().unwrap();
                    let op = ast::AssignmentOperatorNode {
                        span: extract_span_from_token(op),
                    };
                    ast::Expression::Assignment(Box::new(lhs), op, Box::new(rhs))
                }),
            AssignExpr::Error(ectx) => tree_error(ectx),
        };

        data.map(|data| ast::ExpressionNode {
            span: extract_span(ctx),
            data,
        })
    }

    pub fn build_from_cond_expr(
        &self,
        ctx: &cst::CondExpr,
    ) -> AggregateResult<ast::ExpressionNode> {
        use cst::CondExpr;
        let data = match ctx {
            CondExpr::CondExprSingularContext(singular) => {
                return self.build_from_logical_or_expr(singular.value.as_deref().unwrap())
            }
            CondExpr::CondExprTernaryContext(_composed) => AggregateResult::new_err(
                DiagnosticBuilder::new(extract_span(ctx))
                    .build_unimplemented("ternary expressions"),
            ),
            CondExpr::Error(ectx) => tree_error(ectx),
        };

        data.map(|data| ast::ExpressionNode {
            span: extract_span(ctx),
            data,
        })
    }

    build_from_single_binary_op! {
        build_from_logical_or_expr(LogicalOrExpr) {
            LogicalOrExprSingularContext => build_from_logical_and_expr,
            LogicalOrExprComposedContext => DoublePipe
        }
    }

    build_from_single_binary_op! {
        build_from_logical_and_expr(LogicalAndExpr) {
            LogicalAndExprSingularContext => build_from_bitwise_or_expr,
            LogicalAndExprComposedContext => DoubleAmpersand
        }
    }

    build_from_single_binary_op! {
        build_from_bitwise_or_expr(BitwiseOrExpr) {
            BitwiseOrExprSingularContext => build_from_bitwise_xor_expr,
            BitwiseOrExprComposedContext => Pipe
        }
    }

    build_from_single_binary_op! {
        build_from_bitwise_xor_expr(BitwiseXorExpr) {
            BitwiseXorExprSingularContext => build_from_bitwise_and_expr,
            BitwiseXorExprComposedContext => Caret
        }
    }

    build_from_single_binary_op! {
        build_from_bitwise_and_expr(BitwiseAndExpr) {
            BitwiseAndExprSingularContext => build_from_equality_expr,
            BitwiseAndExprComposedContext => Ampersand
        }
    }

    build_from_multi_binary_op! {
        build_from_equality_expr(EqualityExpr) {
            EqualityExprSingularContext => build_from_inequality_expr,
            EqualityExprComposedContext => match token {
                DOUBLE_EQUALS => DoubleEquals,
                BANG_EQUALS => BangEquals,
            }
        }
    }

    build_from_multi_binary_op! {
        build_from_inequality_expr(InequalityExpr) {
            InequalityExprSingularContext => build_from_shift_expr,
            InequalityExprComposedContext => match token {
                ANGLE_LEFT => AngleLeft,
                ANGLE_RIGHT => AngleRight,
                ANGLE_LEFT_EQUALS => AngleLeftEquals,
                ANGLE_RIGHT_EQUALS => AngleRightEquals,
            }
        }
    }

    build_from_multi_binary_op! {
        build_from_shift_expr(ShiftExpr) {
            ShiftExprSingularContext => build_from_arith_expr,
            ShiftExprComposedContext => match token {
                DOUBLE_ANGLE_LEFT => DoubleAngleLeft,
                DOUBLE_ANGLE_RIGHT => DoubleAngleRight,
            }
        }
    }

    build_from_multi_binary_op! {
        build_from_arith_expr(ArithExpr) {
            ArithExprSingularContext => build_from_term_expr,
            ArithExprComposedContext => match token {
                PLUS => Plus,
                MINUS => Minus,
            }
        }
    }

    build_from_multi_binary_op! {
        build_from_term_expr(TermExpr) {
            TermExprSingularContext => build_from_cast_expr,
            TermExprComposedContext => match token {
                STAR => Star,
                SLASH => Slash,
                PERCENT => Percent,
            }
        }
    }

    pub fn build_from_cast_expr(
        &self,
        ctx: &cst::CastExpr,
    ) -> AggregateResult<ast::ExpressionNode> {
        use cst::CastExpr;
        let data = match ctx {
            CastExpr::CastExprSingularContext(singular) => {
                return self.build_from_unary_expr(singular.value.as_deref().unwrap())
            }
            CastExpr::CastExprComposedContext(composed) => self
                .build_from_type_name(composed.type_name.as_deref().unwrap())
                .zip(self.build_from_cast_expr(composed.value.as_deref().unwrap()))
                .map(|(type_name, expr)| ast::Expression::Cast(type_name, Box::new(expr))),
            CastExpr::Error(ectx) => tree_error(ectx),
        };

        data.map(|data| ast::ExpressionNode {
            span: extract_span(ctx),
            data,
        })
    }

    pub fn build_from_unary_expr(
        &self,
        ctx: &cst::UnaryExpr,
    ) -> AggregateResult<ast::ExpressionNode> {
        use cst::UnaryExpr;
        let data = match ctx {
            UnaryExpr::UnaryExprPostfixContext(postfix) => {
                return self.build_from_postfix_expr(postfix.value.as_deref().unwrap())
            }
            UnaryExpr::UnaryExprPrefixContext(prefix) => {
                use generated::clexer as g;
                let op_token = prefix.op.as_deref().unwrap();
                let op = match op_token.token_type {
                    g::DOUBLE_PLUS => ast::UnaryOperator::DoublePlusPrefix,
                    g::DOUBLE_MINUS => ast::UnaryOperator::DoubleMinusPrefix,
                    g::BANG => ast::UnaryOperator::Bang,
                    g::PLUS => ast::UnaryOperator::Plus,
                    g::MINUS => ast::UnaryOperator::Minus,
                    g::TILDE => ast::UnaryOperator::Tilde,
                    g::AMPERSAND => ast::UnaryOperator::Ampersand,
                    g::STAR => ast::UnaryOperator::Star,
                    _ => unreachable!(),
                };
                self.build_from_cast_expr(prefix.value.as_deref().unwrap())
                    .map(|expr| {
                        ast::Expression::Unary(
                            ast::UnaryOperatorNode {
                                span: extract_span_from_token(op_token),
                                data: op,
                            },
                            Box::new(expr),
                        )
                    })
            }
            UnaryExpr::Error(ectx) => tree_error(ectx),
        };

        data.map(|data| ast::ExpressionNode {
            span: extract_span(ctx),
            data,
        })
    }

    pub fn build_from_postfix_expr(
        &self,
        ctx: &cst::PostfixExpr,
    ) -> AggregateResult<ast::ExpressionNode> {
        use cst::PostfixExpr;
        let data = match ctx {
            PostfixExpr::PostfixExprPrimaryContext(primary) => {
                return self.build_from_primary_expr(primary.value.as_deref().unwrap());
            }
            PostfixExpr::PostfixExprArraySubscriptContext(ctx) => self
                .build_from_postfix_expr(ctx.value.as_deref().unwrap())
                .zip(self.build_from_expr(ctx.rhs.as_deref().unwrap()))
                .map(|(lhs, rhs)| ast::Expression::ArraySubscript(Box::new(lhs), Box::new(rhs))),
            PostfixExpr::PostfixExprFunctionCallContext(ctx) => {
                let mut args = AggregateResult::new_ok(Vec::new());
                for arg in &ctx.args {
                    self.build_from_expr(arg.deref())
                        .add_to(&mut args, |res, v| res.push(v));
                }
                self.build_from_identifier(ctx.ident.as_deref().unwrap())
                    .zip(args)
                    .map(|(ident, args)| {
                        ast::Expression::FunctionCall(ast::FunctionCall { ident, args })
                    })
            }
            PostfixExpr::PostfixExprPostfixContext(postfix) => {
                use generated::clexer as g;
                let op_token = postfix.op.as_deref().unwrap();
                let op = match op_token.token_type {
                    g::DOUBLE_PLUS => ast::UnaryOperator::DoublePlusPostfix,
                    g::DOUBLE_MINUS => ast::UnaryOperator::DoubleMinusPostfix,
                    _ => unreachable!(),
                };
                self.build_from_postfix_expr(postfix.value.as_deref().unwrap())
                    .map(|expr| {
                        ast::Expression::Unary(
                            ast::UnaryOperatorNode {
                                span: extract_span_from_token(op_token),
                                data: op,
                            },
                            Box::new(expr),
                        )
                    })
            }
            PostfixExpr::Error(ectx) => tree_error(ectx),
        };

        data.map(|data| ast::ExpressionNode {
            span: extract_span(ctx),
            data,
        })
    }

    pub fn build_from_primary_expr(
        &self,
        ctx: &cst::PrimaryExpr,
    ) -> AggregateResult<ast::ExpressionNode> {
        use cst::PrimaryExpr;
        let data = match ctx {
            PrimaryExpr::PrimaryExprWrappedContext(wrapped) => self
                .build_from_expr(wrapped.inner.as_deref().unwrap())
                .map(|en| en.data),
            PrimaryExpr::PrimaryExprLiteralContext(literal) => self
                .build_from_literal(literal.value.as_deref().unwrap())
                .map(ast::Expression::Literal),
            PrimaryExpr::PrimaryExprIdentifierContext(ident) => self
                .build_from_identifier(ident.ident.as_deref().unwrap())
                .map(ast::Expression::Ident),
            PrimaryExpr::Error(ectx) => tree_error(ectx),
        };

        data.map(|data| ast::ExpressionNode {
            span: extract_span(ctx),
            data,
        })
    }

    pub fn build_from_literal(&self, ctx: &cst::Literal) -> AggregateResult<ast::LiteralNode> {
        use cst::Literal;
        let data = match ctx {
            Literal::LiteralIntegerContext(literal) => {
                self.build_from_integer_literal(literal.value.as_deref().unwrap())
            }
            Literal::LiteralFloatingPointContext(literal) => {
                // The rust parse f64 function allows for strictly more strings as the C grammar
                // so should always return Ok.
                // https://doc.rust-lang.org/nightly/core/primitive.f64.html#impl-FromStr-for-f64
                let value = literal
                    .value
                    .as_deref()
                    .unwrap()
                    .get_text()
                    .parse()
                    .unwrap();
                AggregateResult::new_ok(ast::Literal::Float(value))
            }
            Literal::LiteralCharContext(literal) => {
                let value = literal.value.as_deref().unwrap().get_text();

                // The grammar requires the first and last character to be a `'`, so this will
                // always be safe.
                let value = &value[1..value.len() - 1];

                let span = extract_span(ctx);

                parse_char_literal(value, span.start() + 1).map(ast::Literal::Char)
            }
            Literal::LiteralStringContext(literal) => {
                let mut merged_string = AggregateResult::new_ok(Vec::new());
                // `literal` can consist of multiple STRING_LITERAL tokens, e.g.
                // `"foo" "bar"` is one literal that is equivalent to `"foobar"`.
                for string_literal in &literal.value {
                    let span = extract_span_from_token(string_literal.as_ref());
                    let value = string_literal.get_text();
                    // The grammar requires the first and last character to be a `"`, so this will
                    // always be safe.
                    let value = &value[1..(value.len() - 1)];
                    parse_string_literal(value, span.start() + 1)
                        .add_to(&mut merged_string, |ms, mut s| ms.append(&mut s));
                }
                merged_string.map(ast::Literal::String)
            }
            Literal::Error(ectx) => tree_error(ectx),
        };

        data.map(|data| ast::LiteralNode {
            span: extract_span(ctx),
            data,
        })
    }

    pub fn build_from_integer_literal(
        &self,
        ctx: &cst::IntegerLiteral,
    ) -> AggregateResult<ast::Literal> {
        use cst::IntegerLiteral;
        // All the unwraps here should be fine since the grammar ensures that the invariant are met
        // for the various parse functions
        AggregateResult::new_ok(match ctx {
            IntegerLiteral::IntegerLiteralOctalContext(literal) => {
                let value = i128::from_str_radix(literal.value.as_deref().unwrap().get_text(), 8)
                    .expect("ICE: grammar should have made sure this is a valid octal literal");

                ast::Literal::Octal(value)
            }
            IntegerLiteral::IntegerLiteralDecimalContext(literal) => {
                let value =
                    literal.value.as_deref().unwrap().get_text().parse().expect(
                        "ICE: grammar should have made sure this is a valid decimal literal",
                    );
                ast::Literal::Dec(value)
            }
            IntegerLiteral::IntegerLiteralHexadecimalContext(literal) => {
                let literal = literal.value.as_deref().unwrap().get_text();
                // This is safe since the grammar ensures we start with `0x` or `0X`
                let literal = &literal[2..];
                let value = i128::from_str_radix(literal, 16)
                    .expect("ICE: grammar should have made sure this is a valid hex literal");
                ast::Literal::Hex(value)
            }
            IntegerLiteral::Error(ectx) => tree_error(ectx),
        })
    }

    fn build_from_identifier(&self, ctx: &cst::Identifier) -> AggregateResult<ast::IdentNode> {
        AggregateResult::new_ok(ast::IdentNode {
            span: extract_span(ctx),
            data: ctx.value.as_deref().unwrap().get_text().to_owned(),
        })
    }
}

fn join_comment_tokens<'a>(tokens: impl Iterator<Item = &'a cst::TFTok<'a>>) -> String {
    use generated::clexer as g;
    tokens
        .map(|token| match token.token_type {
            // strip the `//`
            g::SINGLELINE_COMMENT => token.text[2..].to_owned(),
            // strip the `/*` and `*/`
            g::MULTILINE_COMMENT => token.text[2..(token.text.len() - 2)].to_owned(),
            _ => panic!("expected comment token"),
        })
        .collect::<Vec<String>>()
        .join("\n")
}

/// If the stmt is a BlockStatement returns that, if it isen't the statement gets wrapped in a new
/// BlockStatement
fn to_block_statements(stmt: Result<ast::StatementNode, Span>) -> ast::BlockStatementNode {
    match stmt {
        Ok(ast::StatementNode {
            data: ast::Statement::BlockStatement(block),
            ..
        }) => block,
        Ok(stmt) => ast::BlockStatementNode {
            span: stmt.span,
            stmts: vec![stmt],
        },
        Err(span) => ast::BlockStatementNode {
            span,
            stmts: Vec::new(),
        },
    }
}

fn with_qualifiers(
    unqualified_type_node: ast::UnqualifiedTypeNode,
    qualifiers: &[Rc<cst::TypeQualifier>],
) -> AggregateResult<ast::QualifiedType> {
    use cst::TypeQualifier as TQ;
    let mut qualified_type = ast::QualifiedType {
        is_const: None,
        inner: unqualified_type_node,
    };

    let mut res = AggregateResult::new_ok(());

    for qualifier in qualifiers.iter() {
        match qualifier.deref() {
            TQ::TypeQualifierConstContext(ctx) => {
                let span = extract_span(ctx);

                if let Some(original_span) = &qualified_type.is_const {
                    res.add_rec_diagnostic(
                        DiagnosticBuilder::new(span)
                            .build_multiple_qualifiers("const", *original_span),
                    )
                } else {
                    qualified_type.is_const = Some(span);
                }
            }
            TQ::Error(ectx) => tree_error(ectx),
        }
    }
    if matches!(
        qualified_type.inner.data,
        ast::UnqualifiedType::PlainType(ast::PlainType::Primitive(ast::PrimitiveType::Void))
    ) {
        if let Some(const_span) = qualified_type.is_const {
            return AggregateResult::new_err(
                DiagnosticBuilder::new(qualified_type.inner.span).build_qualified_void(const_span),
            );
        }
    }
    res.map(|_| qualified_type)
}

/// Parses a char literal that may contain escaped characters.
///
/// `start_index` must give the byte offset of the start of `value`.
///
/// Note that `value` and `start_index` concern the inner literal, without surrounding quotes
/// (`'` or `"`).
fn parse_char_literal(value: &str, start_index: usize) -> AggregateResult<u8> {
    let literal_end = start_index + value.len();
    let mut seq = value
        .char_indices()
        .map(|(i, c)| (start_index + i, c))
        .peekable();
    let mut out = AggregateResult::new_ok(Vec::new());
    while let Some((index, current_char)) = seq.next() {
        match current_char {
            '\\' => {
                parse_escape_sequence(&mut seq, index, literal_end)
                    .add_to(&mut out, |out, b| out.push(b));
            }
            _ => {
                if let Some(out) = out.value_mut() {
                    out.extend_from_slice(current_char.encode_utf8(&mut [0; 4]).as_bytes());
                }
            }
        };
    }
    out.and_then(|v| match v[..] {
        [] => panic!("ICE: empty char literals are impossible by grammar"),
        [b] => AggregateResult::new_ok(b),
        [b, ..] => AggregateResult::new_rec(
            b,
            DiagnosticBuilder::new(start_index..literal_end).build_multi_byte_char(),
        ),
    })
}

/// Parses a string literal that may contain escaped characters.
///
/// `start_index` must give the byte offset of the start of `value`.
///
/// Note that `value` and `start_index` concern the inner literal, without surrounding quotes
/// (`'` or `"`).
fn parse_string_literal(value: &str, start_index: usize) -> AggregateResult<Vec<u8>> {
    let literal_end = start_index + value.len();
    let mut seq = value
        .char_indices()
        .map(|(i, c)| (start_index + i, c))
        .peekable();
    let mut out = AggregateResult::new_ok(Vec::new());
    while let Some((index, current_char)) = seq.next() {
        match current_char {
            '\\' => {
                let mut byte = parse_escape_sequence(&mut seq, index, literal_end);
                if let Some(0) = byte.value() {
                    let span_end = seq.peek().map_or(literal_end, |(i, _)| *i);
                    byte.add_rec_diagnostic(
                        DiagnosticBuilder::new(index..span_end).build_embedded_null_in_string(),
                    )
                }
                byte.add_to(&mut out, |out, b| out.push(b));
            }
            '\0' => out.add_err(
                DiagnosticBuilder::new(index..(index + 1)).build_embedded_null_in_string(),
            ),
            _ => {
                if let Some(out) = out.value_mut() {
                    out.extend_from_slice(current_char.encode_utf8(&mut [0; 4]).as_bytes());
                }
            }
        };
    }
    out
}

/// Parses an escape sequence following a `\` in a char or string literal.
///
/// The following requirements apply to the arguments:
/// - The `\` escape prefix must already be consumed from `seq`.
/// - The indices in `seq` must represent the byte offset of the input start (not the escape
///   start), i.e. indices that can be used to construct spans.
/// - `escape_start` must give the byte offset of the `\` that started the escape sequence.
/// - `literal_end` must give the byte offset of the exclusive end of the literal (i.e. the
///   index of the past-the-end byte). This will be used as the upper bound of a span if `seq`
///   is fully consumed.
fn parse_escape_sequence(
    seq: &mut std::iter::Peekable<impl Iterator<Item = (usize, char)>>,
    escape_start: usize,
    literal_end: usize,
) -> AggregateResult<u8> {
    let (_, escaped_char) = seq
        .next()
        .expect("ICE: empty escape sequences are impossible by grammar");
    match escaped_char {
        'x' => parse_hex_escape(seq, escape_start, literal_end),
        '\'' => AggregateResult::new_ok(b'\''),
        '"' => AggregateResult::new_ok(b'"'),
        '?' => AggregateResult::new_ok(b'?'),
        '\\' => AggregateResult::new_ok(b'\\'),
        'a' => AggregateResult::new_ok(0x07),
        'b' => AggregateResult::new_ok(0x08),
        'f' => AggregateResult::new_ok(0x0c),
        'n' => AggregateResult::new_ok(b'\n'),
        'r' => AggregateResult::new_ok(b'\r'),
        't' => AggregateResult::new_ok(b'\t'),
        'v' => AggregateResult::new_ok(0x0b),
        o1 @ '0'..='7' => {
            let mut value: u32 = o1.to_digit(8).unwrap();
            if let Some((_, o2)) = seq.next_if(|(_, c)| matches!(c, '0'..='7')) {
                value = value * 0o10 + o2.to_digit(8).unwrap();
                if let Some((_, o3)) = seq.next_if(|(_, c)| matches!(c, '0'..='7')) {
                    value = value * 0o10 + o3.to_digit(8).unwrap();
                }
            }

            let mut result = AggregateResult::new_ok(value as u8);

            if value > u8::MAX as u32 {
                result.add_rec_diagnostic(
                    DiagnosticBuilder::new(
                        escape_start..seq.peek().map_or(literal_end, |(i, _)| *i),
                    )
                    .build_escape_sequence_out_of_range("octal"),
                )
            }

            result
        }
        _ => AggregateResult::new_err(
            DiagnosticBuilder::new(escape_start..seq.peek().map_or(literal_end, |(i, _)| *i))
                .build_unknown_escape_sequence(&format!("\\{escaped_char}")),
        ),
    }
}

/// Parse the hex digits after a `\x` escape in a char or string literal.
///
/// The following requirements apply to the arguments:
/// - The `\x` escape prefix must already be consumed from `seq`.
/// - The indices in `seq` must represent the byte offset from the input start (not the escape
///   start), i.e. indices that can be used to construct spans.
/// - `escape_start` must give the byte offset of the `\` that started the escape sequence.
/// - `literal_end` must give the byte offset of the exclusive end of the literal (i.e. the
///   index of the past-the-end byte). This will be used as the upper bound of a span if `seq`
///   is fully consumed.
fn parse_hex_escape(
    seq: &mut std::iter::Peekable<impl Iterator<Item = (usize, char)>>,
    escape_start: usize,
    literal_end: usize,
) -> AggregateResult<u8> {
    let index = seq.peek().map_or(literal_end, |(i, _)| *i);
    let mut result = match seq.next_if(|(_, c)| c.is_ascii_hexdigit()) {
        Some((_, c)) => AggregateResult::new_ok(c.to_digit(16).unwrap() as u8),
        None => {
            return AggregateResult::new_err(
                DiagnosticBuilder::new(escape_start..index).build_incomplete_hex_escape_sequence(),
            );
        }
    };
    let mut overflowed = false;
    while let Some((_, c)) = seq.next_if(|(_, c)| c.is_ascii_hexdigit()) {
        if let Some(value) = result.value_mut() {
            let (r, of) = value.overflowing_mul(0x10);
            *value = r + c.to_digit(16).unwrap() as u8;
            overflowed |= of;
        }
    }
    if overflowed {
        result.add_rec_diagnostic(
            DiagnosticBuilder::new(escape_start..seq.peek().map_or(literal_end, |(i, _)| *i))
                .build_escape_sequence_out_of_range("hex"),
        )
    }
    result
}

fn generate_printf_from_include_stdio(span: Span) -> ast::ExternalDeclarationNode {
    generate_io_format_fn_from_include_stdio("printf".to_owned(), span)
}

fn generate_scanf_from_include_stdio(span: Span) -> ast::ExternalDeclarationNode {
    generate_io_format_fn_from_include_stdio("scanf".to_owned(), span)
}

fn generate_io_format_fn_from_include_stdio(
    name: String,
    span: Span,
) -> ast::ExternalDeclarationNode {
    ast::ExternalDeclarationNode {
        span,
        data: ast::ExternalDeclaration::Declaration(ast::Declaration::FunctionDeclaration(
            ast::FunctionDeclaration {
                return_type: ast::QualifiedTypeNode {
                    span,
                    // TODO: Should be void for printf
                    data: ast::QualifiedType {
                        is_const: None,
                        inner: ast::UnqualifiedTypeNode {
                            span,
                            data: ast::UnqualifiedType::PlainType(ast::PlainType::Primitive(
                                ast::PrimitiveType::SignedInt,
                            )),
                        },
                    },
                },
                ident: ast::IdentNode { span, data: name },
                params: vec![ast::FunctionParamNode {
                    span,
                    type_name: ast::QualifiedTypeNode {
                        span,
                        data: ast::QualifiedType {
                            is_const: None,
                            inner: ast::UnqualifiedTypeNode {
                                span,
                                data: ast::UnqualifiedType::PointerType(Box::new(
                                    ast::QualifiedTypeNode {
                                        span,
                                        data: ast::QualifiedType {
                                            is_const: Some(span),
                                            inner: ast::UnqualifiedTypeNode {
                                                span,
                                                data: ast::UnqualifiedType::PlainType(
                                                    ast::PlainType::Primitive(
                                                        ast::PrimitiveType::Char,
                                                    ),
                                                ),
                                            },
                                        },
                                    },
                                )),
                            },
                        },
                    },
                    ident: None,
                    array_parts: Vec::new(),
                }],
                is_vararg: true,
            },
        )),
        comments: Some("included via #include<stdio.h>".to_owned()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_escape_sequence() {
        use AggregateResult as AG;
        // hex escape sequence
        let parsed = parse_escape_sequence(&mut "\\x28".char_indices().skip(1).peekable(), 0, 4);
        assert_eq!(parsed, AG::new_ok(0x28));

        // octal escape sequence
        let parsed = parse_escape_sequence(&mut "\\137".char_indices().skip(1).peekable(), 0, 4);
        assert_eq!(parsed, AG::new_ok(0o137));

        // common escaped characters
        let mut input = r#"\n\r\?\f\\"#.char_indices().filter(|(i, _)| i % 2 != 0).peekable();
        assert_eq!(AG::new_ok(b'\n'), parse_escape_sequence(&mut input, 0, 10));
        assert_eq!(AG::new_ok(b'\r'), parse_escape_sequence(&mut input, 2, 10));
        assert_eq!(AG::new_ok(b'?'), parse_escape_sequence(&mut input, 4, 10));
        assert_eq!(AG::new_ok(0x0c), parse_escape_sequence(&mut input, 6, 10));
        assert_eq!(AG::new_ok(b'\\'), parse_escape_sequence(&mut input, 8, 10));
    }

    #[test]
    fn test_parse_hex_escape() {
        let parsed = parse_hex_escape(&mut "24".char_indices().peekable(), 0, 4);
        assert_eq!(parsed, AggregateResult::new_ok(0x24));
    }
}
