use crate::{
    cst,
    diagnostic::{AggregateResult, DiagnosticBuilder},
    generated::{
        clexer::CLexer,
        cparser::{CParser, CParserContextType},
    },
};
use antlr_rust::{
    error_listener::ErrorListener, errors::ANTLRError, recognizer::Recognizer, token::Token,
    token_factory::TokenFactory,
};
use std::{cell::RefCell, rc::Rc};

type LexerInput<'a> = antlr_rust::InputStream<&'a str>;
type Lexer<'a> = CLexer<'a, LexerInput<'a>>;
type TokenStream<'a> = antlr_rust::common_token_stream::CommonTokenStream<'a, Lexer<'a>>;
type ParserErrorStrategy<'a> = antlr_rust::DefaultErrorStrategy<'a, CParserContextType>;
type Parser<'a> = CParser<'a, TokenStream<'a>, ParserErrorStrategy<'a>>;

pub fn parse(input: &str) -> AggregateResult<cst::TranslationUnit> {
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
        .expect("ICE: All references to the error_listener should be dropped by now")
        .into_inner()
        .and(AggregateResult::new_ok(Rc::try_unwrap(tu).expect(
            "ICE: All references to the translation unit (cst) should be dropped by now",
        )))
}

fn build_lexer(input: &str) -> Lexer {
    let input = LexerInput::new(input);
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
                db.build_syntax_error(&offending_symbol_name, expected_tokens.split(',').collect())
            }
            None => db.build_syntax_error(&offending_symbol_name, vec![]),
        };

        let mut ar = self.0.as_ref().take();
        ar.add_err(d);
        self.0.as_ref().replace(ar);
    }
}
