mod generated;

use antlr_rust::{
    error_listener::ErrorListener, errors::ANTLRError, recognizer::Recognizer, token::Token,
    token_factory::TokenFactory,
};
use std::{cell::RefCell, rc::Rc};

use crate::{
    ast,
    diagnostic::{self, AggregateResult, DiagnosticBuilder},
};

type LexerInput<'a> = antlr_rust::InputStream<&'a str>;
type Lexer<'a> = generated::CLexer<'a, LexerInput<'a>>;
type TokenStream<'a> = antlr_rust::common_token_stream::CommonTokenStream<'a, Lexer<'a>>;
type ParserErrorStrategy<'a> = antlr_rust::DefaultErrorStrategy<'a, generated::CParserContextType>;
type Parser<'a> = generated::CParser<'a, TokenStream<'a>, ParserErrorStrategy<'a>>;

pub fn parse(input: &str) -> AggregateResult<ast::Ast> {
    let error_listener = AggregatingErrorListener::new();

    // A seperate scope is used here to make shure the refrences to the ErrorListener are droped.
    let tu = {
        let lexer = build_lexer(input);
        let token_stream = TokenStream::new(lexer);
        let mut parser = build_parser(token_stream, error_listener.clone());

        match parser.translationUnit() {
            Ok(tu) => tu,
            Err(_) => panic!("ICE: Internal ANTLR error"),
        }
    };

    Rc::try_unwrap(error_listener.0)
        .expect("ICE: All refrences to the error_listener should be droped by now")
        .into_inner()
        .and_then(move |_| ast_builder::build_from_translation_unit(&tu))
}

fn build_lexer(input: &str) -> Lexer {
    let input = LexerInput::new(input.into());
    let mut lexer = Lexer::new(input);
    lexer.remove_error_listeners();
    // We have a ERROR_TOKEN: . ; in the grammar with makes sure the lexer never errors
    lexer.add_error_listener(Box::new(PanicErrorListener(
        "ICE: The lexer should never encounter a error",
    )));
    lexer
}

fn build_parser(token_stream: TokenStream, error_listener: AggregatingErrorListener) -> Parser {
    use antlr_rust::Parser as _;
    let mut parser = Parser::with_strategy(token_stream, ParserErrorStrategy::new());
    parser.remove_error_listeners();
    parser.add_error_listener(Box::new(error_listener));
    parser
}

struct PanicErrorListener(&'static str);

impl<'a, T: Recognizer<'a>> ErrorListener<'a, T> for PanicErrorListener {
    fn syntax_error(
        &self,
        _recognizer: &T,
        _offending_symbol: Option<&<T::TF as TokenFactory<'a>>::Inner>,
        _line: isize,
        _column: isize,
        _msg: &str,
        _error: Option<&ANTLRError>,
    ) {
        panic!("{}", self.0);
    }
}

#[derive(Clone)]
struct AggregatingErrorListener(Rc<RefCell<AggregateResult<()>>>);

impl AggregatingErrorListener {
    fn new() -> Self {
        AggregatingErrorListener(Rc::new(RefCell::new(AggregateResult::new_ok(()))))
    }
}

impl<'a, T: Recognizer<'a> + antlr_rust::Parser<'a>> ErrorListener<'a, T>
    for AggregatingErrorListener
{
    fn syntax_error(
        &self,
        recognizer: &T,
        offending_symbol: Option<&<T::TF as TokenFactory<'a>>::Inner>,
        _line: isize,
        _column: isize,
        _msg: &str,
        error: Option<&ANTLRError>,
    ) {
        let offending_symbol = offending_symbol.unwrap();

        let vocabulary = recognizer.get_vocabulary();
        let offending_symbol_name = vocabulary.get_display_name(offending_symbol.get_token_type());

        let start: usize = offending_symbol.get_start().try_into().unwrap();
        let end: usize = (offending_symbol.get_stop() + 1).try_into().unwrap();

        let db = DiagnosticBuilder::new(start..end);

        let d = match error {
            Some(error) => {
                let expected_tokens = match error {
                    ANTLRError::NoAltError(e) => e.base.get_expected_tokens(recognizer),
                    ANTLRError::InputMismatchError(e) => e.base.get_expected_tokens(recognizer),
                    ANTLRError::PredicateError(e) => e.base.get_expected_tokens(recognizer),
                    e => panic!("ICE: Unexpected ANTLRError: {}", e),
                };

                let expected_tokens = expected_tokens.to_token_string(vocabulary);

                // TODO: Find a way to iterator through the expected tokens instead of using the
                // ANTLR-provided string (and splitting it on ',').
                db.build_syntax_error(&offending_symbol_name, expected_tokens.split(",").collect())
            }
            None => db.build_syntax_error(&offending_symbol_name, vec![]),
        };

        let mut ar = self.0.as_ref().take();
        ar.add_err(d);
        self.0.as_ref().replace(ar);
    }
}

mod ast_builder {
    use super::{
        ast,
        diagnostic::{AggregateResult, DiagnosticBuilder, Span},
        generated::{self, context},
    };
    use antlr_rust::{
        parser_rule_context::{BaseParserRuleContext, ParserRuleContext},
        rule_context::CustomRuleContext,
        token::Token,
        TidAble,
    };
    use std::{ops::Deref, rc::Rc};

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

    pub fn build_from_translation_unit(
        ctx: &context::TranslationUnit,
    ) -> AggregateResult<ast::Ast> {
        let mut res = AggregateResult::new_ok(Vec::with_capacity(ctx.content.len()));

        for statement in &ctx.content {
            if let Some(statement) = build_from_statement(statement) {
                statement.add_to(&mut res, |v, s| v.push(s));
            }
        }

        res.map(|mut stmts| {
            stmts.shrink_to_fit();
            ast::Ast {
                global: ast::BlockStatement(stmts),
            }
        })
    }

    fn build_from_statement(
        ctx: &context::Statement,
    ) -> Option<AggregateResult<ast::StatementNode>> {
        use context::Statement;
        let data = match ctx {
            Statement::StatementExprContext(expr) => match expr.value.as_deref() {
                Some(value) => build_from_expr(value).map(ast::Statement::Expression),
                None => return None,
            },
            Statement::StatementDeclarationContext(decl) => {
                build_from_declaration_statement(decl.value.as_deref().unwrap())
            }
            Statement::StatementAssignmentContext(assignment) => {
                build_from_assignment_statement(assignment.value.as_deref().unwrap())
            }
            Statement::StatementBlockContext(block) => {
                build_from_block_statement(block.value.as_deref().unwrap())
            }
            Statement::Error(ectx) => tree_error(ectx),
        };

        Some(data.map(|data| ast::StatementNode {
            span: extract_span(ctx),
            data,
        }))
    }

    fn build_from_declaration_statement(
        ctx: &context::DeclarationStatement,
    ) -> AggregateResult<ast::Statement> {
        use context::DeclarationStatement;
        match ctx {
            DeclarationStatement::DeclarationStatementWithoutInitializerContext(decl) => {
                build_from_type_name(decl.type_name.as_deref().unwrap())
                    .zip(build_from_identifier(decl.ident.as_deref().unwrap()))
                    .map(|(type_name, ident)| ast::Statement::Declaration {
                        type_name,
                        ident,
                        initializer: None,
                    })
            }
            DeclarationStatement::DeclarationStatementWithInitializerContext(decl) => {
                build_from_type_name(decl.type_name.as_deref().unwrap())
                    .zip(build_from_identifier(decl.ident.as_deref().unwrap()))
                    .zip(build_from_expr(decl.rhs.as_deref().unwrap()))
                    .map(
                        |((type_name, ident), initializer)| ast::Statement::Declaration {
                            type_name,
                            ident,
                            initializer: Some(initializer),
                        },
                    )
            }
            DeclarationStatement::Error(ectx) => tree_error(ectx),
        }
    }

    fn build_from_assignment_statement(
        ctx: &context::AssignmentStatement,
    ) -> AggregateResult<ast::Statement> {
        build_from_identifier(ctx.ident.as_deref().unwrap())
            .zip(build_from_expr(ctx.rhs.as_deref().unwrap()))
            .map(|(ident, rhs)| ast::Statement::Assignment { ident, rhs })
    }

    fn build_from_block_statement(
        ctx: &context::BlockStatement,
    ) -> AggregateResult<ast::Statement> {
        let mut res = AggregateResult::new_ok(Vec::with_capacity(ctx.content.len()));

        for statement in &ctx.content {
            if let Some(statement) = build_from_statement(statement) {
                statement.add_to(&mut res, |v, s| v.push(s));
            }
        }

        res.map(|mut stmts| {
            stmts.shrink_to_fit();
            ast::Statement::BlockStatement(ast::BlockStatement(stmts))
        })
    }

    fn build_from_type_name(ctx: &context::TypeName) -> AggregateResult<ast::QualifiedTypeNode> {
        use context::TypeName as TN;
        use context::TypeSpecifier as TS;

        let span = extract_span(ctx);

        let data = match ctx {
            TN::TypeNamePlainContext(plain) => {
                if plain.specifiers.len() > 1 {
                    return AggregateResult::new_err(
                        DiagnosticBuilder::new(span)
                            .build_unimplemented("types with more than one specifier"),
                    );
                }
                let primitive = match plain.specifiers.get(0) {
                    Some(s) => match s.as_ref() {
                        TS::TypeSpecifierPrimitiveContext(primitive_type) => {
                            build_from_primitive_type(primitive_type.tp.as_deref().unwrap())
                                .map(|p| (p, extract_span(primitive_type)))
                        }
                        TS::Error(ectx) => tree_error(ectx),
                    },
                    None => AggregateResult::new_rec(
                        (ast::PrimitiveType::Int, extract_span(plain)),
                        DiagnosticBuilder::new(span).build_unspecified_type(),
                    ),
                };

                primitive.and_then(|(primitive, span)| {
                    let unqualified_type_node = ast::UnqualifiedTypeNode {
                        span,
                        data: ast::UnqualifiedType::PlainType(ast::PlainType::Primitive(primitive)),
                    };
                    with_qualifiers(unqualified_type_node, &plain.qualifiers)
                })
            }
            TN::TypeNamePointerContext(ctx) => build_from_type_name(ctx.inner.as_deref().unwrap())
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

    fn with_qualifiers(
        unqualified_type_node: ast::UnqualifiedTypeNode,
        qualifiers: &Vec<Rc<context::TypeQualifier>>,
    ) -> AggregateResult<ast::QualifiedType> {
        use context::TypeQualifier as TQ;
        let mut qualified_type = ast::QualifiedType {
            is_const: None,
            inner: unqualified_type_node,
        };

        let mut res = AggregateResult::new_ok(());

        for qualifier in qualifiers.iter() {
            // TODO if is_const is some => warning
            match qualifier.deref() {
                TQ::TypeQualifierConstContext(ctx) => {
                    let span = extract_span(ctx);

                    if let Some(original_span) = &qualified_type.is_const {
                        res.add_rec_diagnostic(
                            DiagnosticBuilder::new(span)
                                .build_duplicate_qualifier("const", original_span.clone()),
                        )
                    } else {
                        qualified_type.is_const = Some(span);
                    }
                }
                TQ::Error(ectx) => tree_error(ectx),
            }
        }
        res.map(|_| qualified_type)
    }

    fn build_from_primitive_type(
        ctx: &context::PrimitiveType,
    ) -> AggregateResult<ast::PrimitiveType> {
        use context::PrimitiveType;
        AggregateResult::new_ok(match ctx {
            PrimitiveType::PrimitiveTypeIntContext(_) => ast::PrimitiveType::Int,
            PrimitiveType::PrimitiveTypeCharContext(_) => ast::PrimitiveType::Char,
            PrimitiveType::PrimitiveTypeFloatContext(_) => ast::PrimitiveType::Float,
            PrimitiveType::Error(ectx) => tree_error(ectx),
        })
    }

    pub fn build_from_expr(ctx: &context::Expr) -> AggregateResult<ast::ExpressionNode> {
        build_from_cond_expr(ctx.value.as_deref().unwrap())
    }

    pub fn build_from_cond_expr(ctx: &context::CondExpr) -> AggregateResult<ast::ExpressionNode> {
        use context::CondExpr;
        let data = match ctx {
            CondExpr::CondExprSingularContext(singular) => {
                return build_from_logical_or_expr(singular.value.as_deref().unwrap())
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

    macro_rules! build_from_generic_binary_op {
        (
            $vis:vis $from:ident($context:tt :: $from_type:tt) {
                $singular:tt => $next:ident,
                $composed:tt => let $text:pat => $expr:expr
            }
        ) => {
            $vis fn $from(ctx: &$context::$from_type) -> AggregateResult<ast::ExpressionNode> {
                let data = match ctx {
                    $context::$from_type::$singular(singular) => return $next(singular.value.as_deref().unwrap()),
                    $context::$from_type::$composed(composed) => {
                        $from(composed.lhs.as_deref().unwrap())
                            .zip($next(composed.rhs.as_deref().unwrap()))
                            .map(|(first, sec)| {
                                let op = {
                                    let $text = composed.op.as_deref().unwrap();
                                    $expr
                                };
                                ast::Expression::Binary(Box::new(first), op, Box::new(sec))
                            })
                    }
                    $context::$from_type::Error(ectx) => tree_error(ectx),
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
            $vis:vis $from:ident($context:tt :: $from_type:tt) {
                $singular:tt => $next:ident,
                $composed:tt => $op:ident
            }
        ) => {
            build_from_generic_binary_op! {
                $vis $from($context::$from_type) {
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
            $vis:vis $from:ident($context:tt :: $from_type:tt) {
                $singular:tt => $next:ident,
                $composed:tt => match $op_tok:ident {
                    $($tok_type:ident => $op:ident),* $(,)?
                }
            }
        ) => {
            build_from_generic_binary_op! {
                $vis $from($context::$from_type) {
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

    build_from_single_binary_op! {
        build_from_logical_or_expr(context::LogicalOrExpr) {
            LogicalOrExprSingularContext => build_from_logical_and_expr,
            LogicalOrExprComposedContext => DoublePipe
        }
    }

    build_from_single_binary_op! {
        build_from_logical_and_expr(context::LogicalAndExpr) {
            LogicalAndExprSingularContext => build_from_bitwise_or_expr,
            LogicalAndExprComposedContext => DoubleAmpersand
        }
    }

    build_from_single_binary_op! {
        build_from_bitwise_or_expr(context::BitwiseOrExpr) {
            BitwiseOrExprSingularContext => build_from_bitwise_xor_expr,
            BitwiseOrExprComposedContext => Pipe
        }
    }

    build_from_single_binary_op! {
        build_from_bitwise_xor_expr(context::BitwiseXorExpr) {
            BitwiseXorExprSingularContext => build_from_bitwise_and_expr,
            BitwiseXorExprComposedContext => Caret
        }
    }

    build_from_single_binary_op! {
        build_from_bitwise_and_expr(context::BitwiseAndExpr) {
            BitwiseAndExprSingularContext => build_from_equality_expr,
            BitwiseAndExprComposedContext => Ampersand
        }
    }

    build_from_multi_binary_op! {
        build_from_equality_expr(context::EqualityExpr) {
            EqualityExprSingularContext => build_from_inequality_expr,
            EqualityExprComposedContext => match token {
                DOUBLE_EQUALS => DoubleEquals,
                BANG_EQUALS => BangEquals,
            }
        }
    }

    build_from_multi_binary_op! {
        build_from_inequality_expr(context::InequalityExpr) {
            InequalityExprSingularContext => build_from_arith_expr,
            InequalityExprComposedContext => match token {
                ANGLE_LEFT => AngleLeft,
                ANGLE_RIGHT => AngleRight,
                ANGLE_LEFT_EQUALS => AngleLeftEquals,
                ANGLE_RIGHT_EQUALS => AngleRightEquals,
            }
        }
    }

    build_from_multi_binary_op! {
        build_from_arith_expr(context::ArithExpr) {
            ArithExprSingularContext => build_from_term_expr,
            ArithExprComposedContext => match token {
                PLUS => Plus,
                MINUS => Minus,
            }
        }
    }

    build_from_multi_binary_op! {
        build_from_term_expr(context::TermExpr) {
            TermExprSingularContext => build_from_cast_expr,
            TermExprComposedContext => match token {
                STAR => Star,
                SLASH => Slash,
                PERCENT => Percent,
            }
        }
    }

    pub fn build_from_cast_expr(ctx: &context::CastExpr) -> AggregateResult<ast::ExpressionNode> {
        use context::CastExpr;
        let data = match ctx {
            CastExpr::CastExprSingularContext(singular) => {
                return build_from_unary_expr(singular.value.as_deref().unwrap())
            }
            CastExpr::CastExprComposedContext(composed) => {
                build_from_type_name(composed.type_name.as_deref().unwrap())
                    .zip(build_from_cast_expr(composed.value.as_deref().unwrap()))
                    .map(|(type_name, expr)| ast::Expression::Cast(type_name, Box::new(expr)))
            }
            CastExpr::Error(ectx) => tree_error(ectx),
        };

        data.map(|data| ast::ExpressionNode {
            span: extract_span(ctx),
            data,
        })
    }

    pub fn build_from_unary_expr(ctx: &context::UnaryExpr) -> AggregateResult<ast::ExpressionNode> {
        use context::UnaryExpr;
        let data = match ctx {
            UnaryExpr::UnaryExprPostfixContext(postfix) => {
                return build_from_postfix_expr(postfix.value.as_deref().unwrap())
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
                build_from_cast_expr(prefix.value.as_deref().unwrap()).map(|expr| {
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
        ctx: &context::PostfixExpr,
    ) -> AggregateResult<ast::ExpressionNode> {
        use context::PostfixExpr;
        let data = match ctx {
            PostfixExpr::PostfixExprPrimaryContext(primary) => {
                return build_from_primary_expr(primary.value.as_deref().unwrap());
            }
            PostfixExpr::PostfixExprPostfixContext(postfix) => {
                use generated::clexer as g;
                let op_token = postfix.op.as_deref().unwrap();
                let op = match op_token.token_type {
                    g::DOUBLE_PLUS => ast::UnaryOperator::DoublePlusPostfix,
                    g::DOUBLE_MINUS => ast::UnaryOperator::DoubleMinusPostfix,
                    _ => unreachable!(),
                };
                build_from_postfix_expr(postfix.value.as_deref().unwrap()).map(|expr| {
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
        ctx: &context::PrimaryExpr,
    ) -> AggregateResult<ast::ExpressionNode> {
        use context::PrimaryExpr;
        let data = match ctx {
            PrimaryExpr::PrimaryExprWrappedContext(wrapped) => {
                build_from_expr(wrapped.inner.as_deref().unwrap()).map(|en| en.data)
            }
            PrimaryExpr::PrimaryExprLiteralContext(literal) => {
                build_from_literal(literal.value.as_deref().unwrap()).map(ast::Expression::Literal)
            }
            PrimaryExpr::PrimaryExprIdentifierContext(ident) => {
                build_from_identifier(ident.ident.as_deref().unwrap()).map(ast::Expression::Ident)
            }
            PrimaryExpr::Error(ectx) => tree_error(ectx),
        };

        data.map(|data| ast::ExpressionNode {
            span: extract_span(ctx),
            data,
        })
    }

    pub fn build_from_literal(ctx: &context::Literal) -> AggregateResult<ast::LiteralNode> {
        use context::Literal;
        let data = match ctx {
            Literal::LiteralIntegerContext(literal) => {
                // TODO set type based on size needed for value
                build_from_integer_literal(literal.value.as_deref().unwrap()).map(|value| {
                    ast::Literal {
                        value: ast::LiteralValue::Integer(value),
                        // t: ast::UnqualifiedType::PlainType(ast::PlainType::Primitive(
                        //     ast::PrimitiveType::Int,
                        // )),
                    }
                })
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
                AggregateResult::new_ok(ast::Literal {
                    value: ast::LiteralValue::Float(value),
                    // t: ast::UnqualifiedType::PlainType(ast::PlainType::Primitive(
                    //     ast::PrimitiveType::Float,
                    // )),
                })
            }
            Literal::LiteralCharContext(literal) => {
                let value = literal.value.as_deref().unwrap().get_text();

                // The grammar requires the first and last character to be a `'` so this will
                // always be safe.
                let value = &value[1..value.len() - 1];

                let span = extract_span(ctx);

                parse_char_literal(value, span.start() + 1).map(|byte| {
                    ast::Literal {
                        value: ast::LiteralValue::Integer(byte as i128),
                        // t: ast::UnqualifiedType::PlainType(ast::PlainType::Primitive(
                        //     ast::PrimitiveType::Char,
                        // )),
                    }
                })
            }
            Literal::Error(ectx) => tree_error(ectx),
        };

        data.map(|data| ast::LiteralNode {
            span: extract_span(ctx),
            data,
        })
    }

    pub fn build_from_integer_literal(ctx: &context::IntegerLiteral) -> AggregateResult<i128> {
        use context::IntegerLiteral;
        // All the unwraps here should be fine since the grammar ensures that the invariant are met
        // for the various parse functions
        AggregateResult::new_ok(match ctx {
            IntegerLiteral::IntegerLiteralOctalContext(literal) => {
                i128::from_str_radix(literal.value.as_deref().unwrap().get_text(), 8).unwrap()
            }
            IntegerLiteral::IntegerLiteralDecimalContext(literal) => literal
                .value
                .as_deref()
                .unwrap()
                .get_text()
                .parse()
                .unwrap(),
            IntegerLiteral::IntegerLiteralHexadecimalContext(literal) => {
                let literal = literal.value.as_deref().unwrap().get_text();
                // This is safe since the grammar ensures we start with `0x` or `0X`
                let literal = &literal[2..];
                i128::from_str_radix(literal, 16).unwrap()
            }
            IntegerLiteral::Error(ectx) => tree_error(ectx),
        })
    }

    fn build_from_identifier(ctx: &context::Identifier) -> AggregateResult<ast::IdentNode> {
        AggregateResult::new_ok(ast::IdentNode {
            span: extract_span(ctx),
            data: ctx.value.as_deref().unwrap().get_text().to_owned(),
        })
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
                    DiagnosticBuilder::new(escape_start..index)
                        .build_incomplete_hex_escape_sequence(),
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

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_parse_escape_sequence() {
            use AggregateResult as AG;
            // hex escape sequence
            let parsed =
                parse_escape_sequence(&mut "\\x28".char_indices().skip(1).peekable(), 0, 4);
            assert_eq!(parsed, AG::new_ok(0x28));

            // octal escape sequence
            let parsed =
                parse_escape_sequence(&mut "\\137".char_indices().skip(1).peekable(), 0, 4);
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
}
