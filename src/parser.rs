mod generated;

use antlr_rust::{
    error_listener::ErrorListener, errors::ANTLRError, recognizer::Recognizer, token::Token,
    token_factory::TokenFactory,
};
use std::fmt;

use super::ast;

#[derive(Debug)]
pub struct ParseError(ANTLRError);

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

    let tu = parser.translationUnit().map_err(ParseError)?;

    ast_builder::build_from_translation_unit(tu.as_ref()).map_err(ParseError)
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
    use super::{ast, generated::context, UnspecifiedAntlrError};
    use antlr_rust::{
        errors::ANTLRError, parser_rule_context::BaseParserRuleContext,
        rule_context::CustomRuleContext, token::Token,
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

    pub fn build_from_translation_unit(
        ctx: &context::TranslationUnit,
    ) -> Result<ast::Ast, ANTLRError> {
        Ok(ast::Ast::BlockStatement(ast::BlockStatement(
            ctx.content
                .iter()
                .filter_map(|x| build_from_statement(x))
                .collect::<Result<Vec<_>, _>>()?,
        )))
    }

    fn build_from_statement(
        ctx: &context::Statement,
    ) -> Option<Result<ast::Statement, ANTLRError>> {
        use context::Statement;
        match ctx {
            Statement::StatementExprContext(expr) => expr
                .value
                .as_deref()
                .map(|e| build_from_expr(e).map(ast::Statement::Expression)),
            Statement::StatementDeclarationContext(decl) => Some(build_from_declaration_statement(
                decl.value.as_deref().unwrap(),
            )),
            Statement::StatementAssignmentContext(assignment) => Some(
                build_from_assignment_statement(assignment.value.as_deref().unwrap()),
            ),
            Statement::StatementBlockContext(block) => {
                Some(build_from_block_statement(block.value.as_deref().unwrap()))
            }
            Statement::Error(ectx) => Some(Err(extract_unspecified_error(ectx))),
        }
    }

    fn build_from_declaration_statement(
        ctx: &context::DeclarationStatement,
    ) -> Result<ast::Statement, ANTLRError> {
        use context::DeclarationStatement;
        match ctx {
            DeclarationStatement::DeclarationStatementWithoutInitializerContext(decl) => {
                Ok(ast::Statement::Declaration {
                    type_name: build_from_type_name(decl.type_name.as_deref().unwrap())?,
                    ident: decl.ident.as_ref().unwrap().get_text().to_owned(),
                    initializer: None,
                })
            }
            DeclarationStatement::DeclarationStatementWithInitializerContext(decl) => {
                Ok(ast::Statement::Declaration {
                    type_name: build_from_type_name(decl.type_name.as_deref().unwrap())?,
                    ident: decl.ident.as_ref().unwrap().get_text().to_owned(),
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
            ident: ctx.ident.as_ref().unwrap().get_text().to_owned(),
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

    fn build_from_type_name(ctx: &context::TypeName) -> Result<ast::QualifiedType, ANTLRError> {
        use context::TypeName as TN;
        use context::TypeSpecifier as TS;
        match ctx {
            TN::TypeNamePlainContext(plain) => {
                if plain.specifiers.len() > 1 {
                    unimplemented!("only one specifier supported for now");
                }
                let s = plain
                    .specifiers
                    .get(0)
                    .expect("a type must contain at least one specifier");
                match s.as_ref() {
                    TS::TypeSpecifierPrimitiveContext(primitive_type) => with_qualifiers(
                        ast::UnqualifiedType::PlainType(ast::PlainType::Primitive(
                            build_from_primitive_type(primitive_type.tp.as_deref().unwrap())?,
                        )),
                        &plain.qualifiers,
                    ),
                    TS::Error(ectx) => Err(extract_unspecified_error(ectx)),
                }
            }
            TN::TypeNamePointerContext(ctx) => with_qualifiers(
                ast::UnqualifiedType::PointerType(Box::new(build_from_type_name(
                    ctx.inner.as_deref().unwrap(),
                )?)),
                &ctx.ptr_qualifiers,
            ),
            TN::Error(ectx) => Err(extract_unspecified_error(ectx)),
        }
    }

    fn with_qualifiers(
        unqualified_type: ast::UnqualifiedType,
        qualifiers: &Vec<Rc<context::TypeQualifier>>,
    ) -> Result<ast::QualifiedType, ANTLRError> {
        use context::TypeQualifier as TQ;
        let mut qualified_type = ast::QualifiedType {
            is_const: false,
            inner: unqualified_type,
        };
        for qualifier in qualifiers.iter() {
            match qualifier.deref() {
                TQ::TypeQualifierConstContext(_) => {
                    qualified_type.is_const = true;
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

    pub fn build_from_expr(ctx: &context::Expr) -> Result<ast::Expression, ANTLRError> {
        build_from_cond_expr(ctx.value.as_deref().unwrap())
    }

    pub fn build_from_cond_expr(ctx: &context::CondExpr) -> Result<ast::Expression, ANTLRError> {
        use context::CondExpr;
        match ctx {
            CondExpr::CondExprSingularContext(singular) => {
                build_from_logical_or_expr(singular.value.as_deref().unwrap())
            }
            CondExpr::CondExprTernaryContext(_composed) => todo!("Ternary is not yet suported"),
            CondExpr::Error(ectx) => Err(extract_unspecified_error(ectx)),
        }
    }

    macro_rules! build_from_generic_binary_op {
        (
            $vis:vis $from:ident($context:tt :: $from_type:tt) {
                $singular:tt => $next:ident,
                $composed:tt => let $text:pat => $expr:expr
            }
        ) => {
            $vis fn $from(ctx: &$context::$from_type) -> Result<ast::Expression, ANTLRError> {
                match ctx {
                    $context::$from_type::$singular(singular) => $next(singular.value.as_deref().unwrap()),
                    $context::$from_type::$composed(composed) => Ok(ast::Expression::Binary(
                        Box::new($from(composed.lhs.as_deref().unwrap())?),
                        {
                            let $text = composed.op.as_deref().unwrap();
                            $expr
                        },
                        Box::new($next(composed.rhs.as_deref().unwrap())?),
                    )),
                    $context::$from_type::Error(ectx) => Err(extract_unspecified_error(ectx)),
                }
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
                    $composed => let _ => ast::BinaryOperator::$op
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
                        match $op_text.get_text() {
                            $($pat => ast::BinaryOperator::$op,)*
                            _ => unreachable!(),
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

    pub fn build_from_cast_expr(ctx: &context::CastExpr) -> Result<ast::Expression, ANTLRError> {
        use context::CastExpr;
        match ctx {
            CastExpr::CastExprSingularContext(singular) => {
                build_from_unary_expr(singular.value.as_deref().unwrap())
            }
            CastExpr::CastExprComposedContext(composed) => Ok(ast::Expression::Cast(
                build_from_type_name(composed.type_name.as_deref().unwrap())?,
                Box::new(build_from_cast_expr(composed.value.as_deref().unwrap())?),
            )),
            CastExpr::Error(ectx) => Err(extract_unspecified_error(ectx)),
        }
    }

    pub fn build_from_unary_expr(ctx: &context::UnaryExpr) -> Result<ast::Expression, ANTLRError> {
        use context::UnaryExpr;
        match ctx {
            UnaryExpr::UnaryExprPostfixContext(postfix) => {
                build_from_postfix_expr(postfix.value.as_deref().unwrap())
            }
            UnaryExpr::UnaryExprPrefixContext(prefix) => Ok(ast::Expression::Unary(
                match prefix.op.as_ref().unwrap().get_text() {
                    "++" => ast::UnaryOperator::DoublePlusPrefix,
                    "--" => ast::UnaryOperator::DoubleMinusPrefix,
                    "!" => ast::UnaryOperator::Bang,
                    "+" => ast::UnaryOperator::Plus,
                    "-" => ast::UnaryOperator::Minus,
                    "~" => ast::UnaryOperator::Tilde,
                    "&" => ast::UnaryOperator::Ampersand,
                    "*" => ast::UnaryOperator::Star,
                    _ => unreachable!(),
                },
                Box::new(build_from_cast_expr(prefix.value.as_deref().unwrap())?),
            )),
            UnaryExpr::Error(ectx) => Err(extract_unspecified_error(ectx)),
        }
    }

    pub fn build_from_postfix_expr(
        ctx: &context::PostfixExpr,
    ) -> Result<ast::Expression, ANTLRError> {
        use context::PostfixExpr;
        match ctx {
            PostfixExpr::PostfixExprPrimaryContext(primary) => {
                build_from_primary_expr(primary.value.as_deref().unwrap())
            }
            PostfixExpr::PostfixExprPostfixContext(postfix) => Ok(ast::Expression::Unary(
                match postfix.op.as_ref().unwrap().get_text() {
                    "++" => ast::UnaryOperator::DoublePlusPostfix,
                    "--" => ast::UnaryOperator::DoubleMinusPostfix,
                    _ => unreachable!(),
                },
                Box::new(build_from_postfix_expr(postfix.value.as_deref().unwrap())?),
            )),
            PostfixExpr::Error(ectx) => Err(extract_unspecified_error(ectx)),
        }
    }

    pub fn build_from_primary_expr(
        ctx: &context::PrimaryExpr,
    ) -> Result<ast::Expression, ANTLRError> {
        use context::PrimaryExpr;
        match ctx {
            PrimaryExpr::PrimaryExprWrappedContext(wrapped) => {
                build_from_expr(wrapped.inner.as_deref().unwrap())
            }
            PrimaryExpr::PrimaryExprLiteralContext(literal) => Ok(ast::Expression::Literal(
                literal.value.as_deref().unwrap().get_text(),
            )),
            PrimaryExpr::PrimaryExprIdentContext(ident) => Ok(ast::Expression::Ident(
                ident.ident.as_deref().unwrap().get_text().to_owned(),
            )),
            PrimaryExpr::Error(ectx) => Err(extract_unspecified_error(ectx)),
        }
    }

    // pub fn build_from_integer_literal(
    //     ctx: &context::IntegerLiteral,
    // ) -> Result<ast::Expression, ANTLRError> {
    //     todo!()
    // }
    // pub fn build_from_literal(ctx: &context::Literal) -> Result<ast::Expression, ANTLRError> {
    //     todo!()
    // }
}
