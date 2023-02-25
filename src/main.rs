mod ast;
mod parser;

fn main() {
    dbg!(parser::parse("2 + 3"));
}
