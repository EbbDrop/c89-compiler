use generators::dot::to_dot;

mod ast;
mod generators;
mod parser;

fn main() {
    match parser::parse("2 + 3") {
        Ok(ref ast) => {
            println!("{}", to_dot(ast));
        }
        Err(err) => {
            eprintln!("{:#?}", err);
        }
    }
}
