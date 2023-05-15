mod dot_tree;
mod inspect_ast;
mod inspect_ir;

use crate::{ast, ir};
use dot_tree::DotTree;
use std::fmt::Write;

trait ToDot {
    fn to_dot(&self) -> DotTree;
}

pub fn inspect_ast(ast: &ast::Ast) -> String {
    ast.to_dot().to_string()
}

pub fn inspect_ir(ir: &ir::Root) -> String {
    ir.to_dot().to_string()
}

fn escape_string_literal(string_literal: &[u8]) -> String {
    let mut escaped = String::from("\"");
    for c in String::from_utf8_lossy(string_literal).chars() {
        match c {
            '\\' | '"' => write!(escaped, "\\{c}").unwrap(),
            '\t' => escaped += "\\t",
            '\n' => escaped += "\\n",
            '\r' => escaped += "\\r",
            '\x07' => escaped += "\\a",
            '\x08' => escaped += "\\b",
            '\x0c' => escaped += "\\f",
            '\x0b' => escaped += "\\v",
            ' ' => escaped += " ",
            c if c.is_ascii_graphic() => escaped.write_char(c).unwrap(),
            c if c.is_ascii() => write!(escaped, "\\x{:02X}", c as u8).unwrap(),
            c => escaped.write_char(c).unwrap(),
        }
        if escaped.chars().count() >= 10 {
            escaped += "…";
            return escaped;
        }
    }
    escaped += "\"";
    escaped
}
