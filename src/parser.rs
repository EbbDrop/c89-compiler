mod generated;

use antlr_rust::{common_token_stream::CommonTokenStream, InputStream};
use generated::mainLexer as Lexer;
use generated::mainParser as Parser;

pub fn parse() {
    let lexer = Lexer::new(InputStream::new("3 + 5".into()));
    let mut parser = Parser::new(CommonTokenStream::new(lexer));
    parser.full_expr().unwrap();
}
