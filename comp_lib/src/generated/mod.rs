#[rustfmt::skip]
#[allow(clippy::all)]
mod cparserlistener;

#[rustfmt::skip]
#[allow(clippy::all)]
pub mod clexer;

#[rustfmt::skip]
#[allow(clippy::all)]
#[allow(unused_parens)]
pub mod cparser;

pub type LexerInput<D> = antlr_rust::InputStream<D>;
pub type Lexer<'a, D> = clexer::CLexer<'a, LexerInput<D>>;
pub type TokenStream<'a, D> = antlr_rust::common_token_stream::CommonTokenStream<'a, Lexer<'a, D>>;
