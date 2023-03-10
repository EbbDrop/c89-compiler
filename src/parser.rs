mod generated;

use antlr_rust::{
    error_listener::ErrorListener, errors::ANTLRError, recognizer::Recognizer, token::Token,
    token_factory::TokenFactory,
};
use std::fmt;

use super::ast;

#[derive(Debug)]
pub struct ParseError();

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for ParseError {}

#[derive(Debug)]
pub struct UnspecifiedAntlrError;

impl fmt::Display for UnspecifiedAntlrError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "unspecified ANTLR error")
    }
}

impl std::error::Error for UnspecifiedAntlrError {}

type LexerInput<'a> = antlr_rust::InputStream<&'a str>;
type Lexer<'a> = generated::MainLexer<'a, LexerInput<'a>>;
type TokenStream<'a> = antlr_rust::common_token_stream::CommonTokenStream<'a, Lexer<'a>>;
type ParserErrorStrategy<'a> = antlr_rust::BailErrorStrategy<'a, generated::MainParserContextType>;
type Parser<'a> = generated::MainParser<'a, TokenStream<'a>, ParserErrorStrategy<'a>>;

pub fn parse(input: &str) -> Result<ast::Ast, ParseError> {
    let lexer = build_lexer(input);
    let token_stream = TokenStream::new(lexer);
    let mut parser = build_parser(token_stream);

    let tu = parser.translationUnit().map_err(|_| ParseError())?;

    ast_builder::build_from_translation_unit(tu.as_ref()).map_err(|_| ParseError())
}

fn build_lexer(input: &str) -> Lexer {
    let input = LexerInput::new(input.into());
    let mut lexer = Lexer::new(input);
    lexer.remove_error_listeners();
    lexer.add_error_listener(Box::new(CustomErrorListener {}));
    lexer
}

fn build_parser(token_stream: TokenStream) -> Parser {
    use antlr_rust::Parser as _;
    let mut parser = Parser::with_strategy(token_stream, ParserErrorStrategy::new());
    parser.remove_error_listeners();
    parser.add_error_listener(Box::new(CustomErrorListener {}));
    parser
}

struct CustomErrorListener {}

impl<'a, T: Recognizer<'a>> ErrorListener<'a, T> for CustomErrorListener
where
    // NOTE: this is only required to display the token data.
    // This can probably be removed once `syntax_error` has a real implementation.
    <T::TF as TokenFactory<'a>>::Data: std::fmt::Display,
{
    fn syntax_error(
        &self,
        _recognizer: &T,
        offending_symbol: Option<&<T::TF as TokenFactory<'a>>::Inner>,
        line: isize,
        column: isize,
        msg: &str,
        _error: Option<&ANTLRError>,
    ) {
        eprintln!("Lexer error at line {}, column {}: {}", line, column, msg);
        if let Some(token) = offending_symbol {
            eprintln!("Found offending symbol: {}", token.get_text());
        }
    }
}

mod ast_builder {
    use crate::ast::Span;

    use super::{ast, generated::context, UnspecifiedAntlrError};
    use antlr_rust::{
        errors::ANTLRError,
        parser_rule_context::{BaseParserRuleContext, ParserRuleContext},
        rule_context::CustomRuleContext,
        token::Token,
    };
    use std::{ops::Deref, rc::Rc};

    fn extract_unspecified_error<'a, Ctx>(ectx: &BaseParserRuleContext<'a, Ctx>) -> ANTLRError
    where
        Ctx: CustomRuleContext<'a>,
    {
        ectx.exception
            .as_deref()
            .cloned()
            .unwrap_or(ANTLRError::FallThrough(Rc::new(UnspecifiedAntlrError {})))
    }

    fn extract_span<'a>(ctx: &impl ParserRuleContext<'a>) -> Span {
        let start = ctx.start().get_start();
        let end = ctx.stop().get_stop();

        Span {
            start: start as usize,
            length: (end + 1 - start) as usize,
        }
    }

    fn extract_span_from_token(token: &impl Token) -> Span {
        let start = token.get_start();
        let end = token.get_stop();

        Span {
            start: start as usize,
            length: (end + 1 - start) as usize,
        }
    }

    pub fn build_from_translation_unit(
        ctx: &context::TranslationUnit,
    ) -> Result<ast::Ast, ANTLRError> {
        Ok(ast::Ast {
            global: ast::BlockStatement(
                ctx.content
                    .iter()
                    .filter_map(|x| build_from_statement(x))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
        })
    }

    fn build_from_statement(
        ctx: &context::Statement,
    ) -> Option<Result<ast::StatementNode, ANTLRError>> {
        use context::Statement;
        let data = match ctx {
            Statement::StatementExprContext(expr) => {
                build_from_expr(expr.value.as_deref()?).map(ast::Statement::Expression)
            }
            Statement::StatementDeclarationContext(decl) => {
                build_from_declaration_statement(decl.value.as_deref().unwrap())
            }
            Statement::StatementAssignmentContext(assignment) => {
                build_from_assignment_statement(assignment.value.as_deref().unwrap())
            }
            Statement::StatementBlockContext(block) => {
                build_from_block_statement(block.value.as_deref().unwrap())
            }
            Statement::Error(ectx) => Err(extract_unspecified_error(ectx)),
        };

        Some(data.map(|data| ast::StatementNode {
            span: extract_span(ctx),
            data,
        }))
    }

    fn build_from_declaration_statement(
        ctx: &context::DeclarationStatement,
    ) -> Result<ast::Statement, ANTLRError> {
        use context::DeclarationStatement;
        match ctx {
            DeclarationStatement::DeclarationStatementWithoutInitializerContext(decl) => {
                Ok(ast::Statement::Declaration {
                    type_name: build_from_type_name(decl.type_name.as_deref().unwrap())?,
                    ident: build_from_identifier(decl.ident.as_deref().unwrap())?,
                    initializer: None,
                })
            }
            DeclarationStatement::DeclarationStatementWithInitializerContext(decl) => {
                Ok(ast::Statement::Declaration {
                    type_name: build_from_type_name(decl.type_name.as_deref().unwrap())?,
                    ident: build_from_identifier(decl.ident.as_deref().unwrap())?,
                    initializer: Some(build_from_expr(decl.rhs.as_deref().unwrap())?),
                })
            }
            DeclarationStatement::Error(ectx) => Err(extract_unspecified_error(ectx)),
        }
    }

    fn build_from_assignment_statement(
        ctx: &context::AssignmentStatement,
    ) -> Result<ast::Statement, ANTLRError> {
        Ok(ast::Statement::Assignment {
            ident: build_from_identifier(ctx.ident.as_deref().unwrap())?,
            rhs: build_from_expr(ctx.rhs.as_deref().unwrap())?,
        })
    }

    fn build_from_block_statement(
        ctx: &context::BlockStatement,
    ) -> Result<ast::Statement, ANTLRError> {
        Ok(ast::Statement::BlockStatement(ast::BlockStatement(
            ctx.content
                .iter()
                .filter_map(|s| build_from_statement(s))
                .collect::<Result<Vec<_>, _>>()?,
        )))
    }

    fn build_from_type_name(ctx: &context::TypeName) -> Result<ast::QualifiedTypeNode, ANTLRError> {
        use context::TypeName as TN;
        use context::TypeSpecifier as TS;
        let data = match ctx {
            TN::TypeNamePlainContext(plain) => {
                if plain.specifiers.len() > 1 {
                    unimplemented!("only one specifier supported for now");
                }
                let s = plain
                    .specifiers
                    .get(0)
                    .expect("a type must contain at least one specifier");
                match s.as_ref() {
                    TS::TypeSpecifierPrimitiveContext(primitive_type) => {
                        let unqualified_type_node = ast::UnqualifiedTypeNode {
                            span: extract_span(primitive_type),
                            data: ast::UnqualifiedType::PlainType(ast::PlainType::Primitive(
                                build_from_primitive_type(primitive_type.tp.as_deref().unwrap())?,
                            )),
                        };
                        with_qualifiers(unqualified_type_node, &plain.qualifiers)?
                    }
                    TS::Error(ectx) => return Err(extract_unspecified_error(ectx)),
                }
            }
            TN::TypeNamePointerContext(ctx) => {
                let unqualified_type_node = ast::UnqualifiedTypeNode {
                    span: extract_span(ctx),
                    data: ast::UnqualifiedType::PointerType(Box::new(build_from_type_name(
                        ctx.inner.as_deref().unwrap(),
                    )?)),
                };
                with_qualifiers(unqualified_type_node, &ctx.ptr_qualifiers)?
            }
            TN::Error(ectx) => return Err(extract_unspecified_error(ectx)),
        };

        Ok(ast::QualifiedTypeNode {
            span: extract_span(ctx),
            data,
        })
    }

    fn with_qualifiers(
        unqualified_type_node: ast::UnqualifiedTypeNode,
        qualifiers: &Vec<Rc<context::TypeQualifier>>,
    ) -> Result<ast::QualifiedType, ANTLRError> {
        use context::TypeQualifier as TQ;
        let mut qualified_type = ast::QualifiedType {
            is_const: None,
            inner: unqualified_type_node,
        };
        for qualifier in qualifiers.iter() {
            // TODO if is_const is some => warning
            match qualifier.deref() {
                TQ::TypeQualifierConstContext(ctx) => {
                    qualified_type.is_const = Some(extract_span(ctx));
                }
                TQ::Error(ectx) => return Err(extract_unspecified_error(ectx)),
            }
        }
        Ok(qualified_type)
    }

    fn build_from_primitive_type(
        ctx: &context::PrimitiveType,
    ) -> Result<ast::PrimitiveType, ANTLRError> {
        use context::PrimitiveType;
        match ctx {
            PrimitiveType::PrimitiveTypeIntContext(_) => Ok(ast::PrimitiveType::Int),
            PrimitiveType::PrimitiveTypeCharContext(_) => Ok(ast::PrimitiveType::Char),
            PrimitiveType::PrimitiveTypeFloatContext(_) => Ok(ast::PrimitiveType::Float),
            PrimitiveType::Error(ectx) => Err(extract_unspecified_error(ectx)),
        }
    }

    pub fn build_from_expr(ctx: &context::Expr) -> Result<ast::ExpressionNode, ANTLRError> {
        build_from_cond_expr(ctx.value.as_deref().unwrap())
    }

    pub fn build_from_cond_expr(
        ctx: &context::CondExpr,
    ) -> Result<ast::ExpressionNode, ANTLRError> {
        use context::CondExpr;
        let _data = match ctx {
            CondExpr::CondExprSingularContext(singular) => {
                return build_from_logical_or_expr(singular.value.as_deref().unwrap())
            }
            CondExpr::CondExprTernaryContext(_composed) => todo!("Ternary is not yet suported"),
            CondExpr::Error(ectx) => return Err(extract_unspecified_error(ectx)),
        };

        #[allow(unreachable_code)]
        Ok(ast::ExpressionNode {
            span: extract_span(ctx),
            data: _data,
        })
    }

    macro_rules! build_from_generic_binary_op {
        (
            $vis:vis $from:ident($context:tt :: $from_type:tt) {
                $singular:tt => $next:ident,
                $composed:tt => let $text:pat => $expr:expr
            }
        ) => {
            $vis fn $from(ctx: &$context::$from_type) -> Result<ast::ExpressionNode, ANTLRError> {
                let data = match ctx {
                    $context::$from_type::$singular(singular) => return $next(singular.value.as_deref().unwrap()),
                    $context::$from_type::$composed(composed) => ast::Expression::Binary(
                        Box::new($from(composed.lhs.as_deref().unwrap())?),
                        {
                            let $text = composed.op.as_deref().unwrap();
                            $expr
                        },
                        Box::new($next(composed.rhs.as_deref().unwrap())?),
                    ),
                    $context::$from_type::Error(ectx) => return Err(extract_unspecified_error(ectx)),
                };

                Ok(ast::ExpressionNode {
                    span: extract_span(ctx),
                    data,
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
                $composed:tt => match $op_text:ident {
                    $($pat:pat => $op:ident),* $(,)?
                }
            }
        ) => {
            build_from_generic_binary_op! {
                $vis $from($context::$from_type) {
                    $singular => $next,
                    $composed => let $op_text => {
                        let data = match $op_text.get_text() {
                            $($pat => ast::BinaryOperator::$op,)*
                            _ => unreachable!(),
                        };
                        ast::BinaryOperatorNode {
                            span: extract_span_from_token($op_text),
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
            EqualityExprComposedContext => match text {
                "==" => DoubleEquals,
                "!=" => BangEquals,
            }
        }
    }

    build_from_multi_binary_op! {
        build_from_inequality_expr(context::InequalityExpr) {
            InequalityExprSingularContext => build_from_arith_expr,
            InequalityExprComposedContext => match text {
                ">" => AngleLeft,
                "<" => AngleRight,
                ">=" => AngleLeftEquals,
                "<=" => AngleRightEquals,
            }
        }
    }

    build_from_multi_binary_op! {
        build_from_arith_expr(context::ArithExpr) {
            ArithExprSingularContext => build_from_term_expr,
            ArithExprComposedContext => match text {
                "+" => Plus,
                "-" => Minus,
            }
        }
    }

    build_from_multi_binary_op! {
        build_from_term_expr(context::TermExpr) {
            TermExprSingularContext => build_from_cast_expr,
            TermExprComposedContext => match text {
                "*" => Star,
                "/" => Slash,
                "%" => Percent,
            }
        }
    }

    pub fn build_from_cast_expr(
        ctx: &context::CastExpr,
    ) -> Result<ast::ExpressionNode, ANTLRError> {
        use context::CastExpr;
        let data = match ctx {
            CastExpr::CastExprSingularContext(singular) => {
                return build_from_unary_expr(singular.value.as_deref().unwrap())
            }
            CastExpr::CastExprComposedContext(composed) => ast::Expression::Cast(
                build_from_type_name(composed.type_name.as_deref().unwrap())?,
                Box::new(build_from_cast_expr(composed.value.as_deref().unwrap())?),
            ),
            CastExpr::Error(ectx) => return Err(extract_unspecified_error(ectx)),
        };

        Ok(ast::ExpressionNode {
            span: extract_span(ctx),
            data,
        })
    }

    pub fn build_from_unary_expr(
        ctx: &context::UnaryExpr,
    ) -> Result<ast::ExpressionNode, ANTLRError> {
        use context::UnaryExpr;
        let data = match ctx {
            UnaryExpr::UnaryExprPostfixContext(postfix) => {
                return build_from_postfix_expr(postfix.value.as_deref().unwrap())
            }
            UnaryExpr::UnaryExprPrefixContext(prefix) => {
                let op_token = prefix.op.as_deref().unwrap();
                let op = match op_token.get_text() {
                    "++" => ast::UnaryOperator::DoublePlusPrefix,
                    "--" => ast::UnaryOperator::DoubleMinusPrefix,
                    "!" => ast::UnaryOperator::Bang,
                    "+" => ast::UnaryOperator::Plus,
                    "-" => ast::UnaryOperator::Minus,
                    "~" => ast::UnaryOperator::Tilde,
                    "&" => ast::UnaryOperator::Ampersand,
                    "*" => ast::UnaryOperator::Star,
                    _ => unreachable!(),
                };
                ast::Expression::Unary(
                    ast::UnaryOperatorNode {
                        span: extract_span_from_token(op_token),
                        data: op,
                    },
                    Box::new(build_from_cast_expr(prefix.value.as_deref().unwrap())?),
                )
            }
            UnaryExpr::Error(ectx) => return Err(extract_unspecified_error(ectx)),
        };

        Ok(ast::ExpressionNode {
            span: extract_span(ctx),
            data,
        })
    }

    pub fn build_from_postfix_expr(
        ctx: &context::PostfixExpr,
    ) -> Result<ast::ExpressionNode, ANTLRError> {
        use context::PostfixExpr;
        let data = match ctx {
            PostfixExpr::PostfixExprPrimaryContext(primary) => {
                return build_from_primary_expr(primary.value.as_deref().unwrap())
            }
            PostfixExpr::PostfixExprPostfixContext(postfix) => {
                let op_token = postfix.op.as_deref().unwrap();
                let op = match op_token.get_text() {
                    "++" => ast::UnaryOperator::DoublePlusPostfix,
                    "--" => ast::UnaryOperator::DoubleMinusPostfix,
                    _ => unreachable!(),
                };
                ast::Expression::Unary(
                    ast::UnaryOperatorNode {
                        span: extract_span_from_token(op_token),
                        data: op,
                    },
                    Box::new(build_from_postfix_expr(postfix.value.as_deref().unwrap())?),
                )
            }
            PostfixExpr::Error(ectx) => return Err(extract_unspecified_error(ectx)),
        };

        Ok(ast::ExpressionNode {
            span: extract_span(ctx),
            data,
        })
    }

    pub fn build_from_primary_expr(
        ctx: &context::PrimaryExpr,
    ) -> Result<ast::ExpressionNode, ANTLRError> {
        use context::PrimaryExpr;
        let data = match ctx {
            PrimaryExpr::PrimaryExprWrappedContext(wrapped) => {
                return build_from_expr(wrapped.inner.as_deref().unwrap())
            }
            PrimaryExpr::PrimaryExprLiteralContext(literal) => {
                ast::Expression::Literal(build_from_literal(literal.value.as_deref().unwrap())?)
            }
            PrimaryExpr::PrimaryExprIdentifierContext(ident) => {
                ast::Expression::Ident(build_from_identifier(ident.ident.as_deref().unwrap())?)
            }
            PrimaryExpr::Error(ectx) => return Err(extract_unspecified_error(ectx)),
        };

        Ok(ast::ExpressionNode {
            span: extract_span(ctx),
            data,
        })
    }

    pub fn build_from_literal(ctx: &context::Literal) -> Result<ast::LiteralNode, ANTLRError> {
        use context::Literal;
        let data = match ctx {
            Literal::LiteralIntegerContext(literal) => {
                // TODO set type based on size needed for value
                let value = build_from_integer_literal(literal.value.as_deref().unwrap())?;
                ast::Literal {
                    value: ast::LiteralValue::Integer(value),
                    // t: ast::UnqualifiedType::PlainType(ast::PlainType::Primitive(
                    //     ast::PrimitiveType::Int,
                    // )),
                }
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
                ast::Literal {
                    value: ast::LiteralValue::Float(value),
                    // t: ast::UnqualifiedType::PlainType(ast::PlainType::Primitive(
                    //     ast::PrimitiveType::Float,
                    // )),
                }
            }
            Literal::LiteralCharContext(literal) => {
                let value = literal.value.as_deref().unwrap().get_text();

                // The grammar requires the first and last character to be a `'` so this will
                // always be safe.
                let value = &value[1..value.len() - 1];

                let value = parse_string_literal(value);
                let value = if value.len() == 1 {
                    value.as_bytes()[0] as u8 as i128
                } else {
                    // Technically multi byte chars are allowed in the C standard but are
                    // implementation defined. `g++ -ansi -pedantic` gives a warning
                    // TODO give real error/warning
                    return Err(ANTLRError::FallThrough(Rc::new(UnspecifiedAntlrError {})));
                };

                ast::Literal {
                    value: ast::LiteralValue::Integer(value),
                    // t: ast::UnqualifiedType::PlainType(ast::PlainType::Primitive(
                    //     ast::PrimitiveType::Char,
                    // )),
                }
            }
            Literal::Error(ectx) => return Err(extract_unspecified_error(ectx)),
        };

        Ok(ast::LiteralNode {
            span: extract_span(ctx),
            data,
        })
    }

    pub fn build_from_integer_literal(ctx: &context::IntegerLiteral) -> Result<i128, ANTLRError> {
        use context::IntegerLiteral;
        // All the unwraps here should be fine since the grammar ensures that the invariant are met
        // for the various parse functions
        match ctx {
            IntegerLiteral::IntegerLiteralOctalContext(literal) => {
                Ok(i128::from_str_radix(literal.value.as_deref().unwrap().get_text(), 8).unwrap())
            }
            IntegerLiteral::IntegerLiteralDecimalContext(literal) => Ok(literal
                .value
                .as_deref()
                .unwrap()
                .get_text()
                .parse()
                .unwrap()),
            IntegerLiteral::IntegerLiteralHexadecimalLContext(literal) => {
                let literal = literal.value.as_deref().unwrap().get_text();
                // This is safe since the grammar ensures we start with `0x` or `0X`
                let literal = &literal[2..];
                Ok(i128::from_str_radix(literal, 16).unwrap())
            }
            IntegerLiteral::Error(ectx) => Err(extract_unspecified_error(ectx)),
        }
    }

    fn build_from_identifier(ctx: &context::Identifier) -> Result<ast::IdentNode, ANTLRError> {
        Ok(ast::IdentNode {
            span: extract_span(ctx),
            data: ctx.value.as_deref().unwrap().get_text().to_owned(),
        })
    }

    fn parse_string_literal(value: &str) -> String {
        if value.len() == 1 {
            return value.to_owned();
        }
        todo!("Parsing strings/escaped chars is not yet implemented")
    }
}
