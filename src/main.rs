#![feature(box_into_inner)]

mod ast;
mod parser;

fn main() {
    match parser::parse("2 + 3") {
        Ok(ast) => {
            dbg!(ast);
        }
        Err(err) => {
            eprintln!("{:#?}", err);
        }
    }
}
