use crate::cli;
use codespan_reporting::files::SimpleFile;
use comp_lib::{diagnostic::AggregateResult, inspectors, passes};

pub struct CompileOpts {
    pub output_format: cli::OutputFormat,
    pub const_fold: bool,
}

pub fn compile(source: &SimpleFile<String, String>, opts: CompileOpts) -> AggregateResult<Vec<u8>> {
    let cst = passes::parse::parse(source.source());
    // IDEA: antlr tree string
    let mut ast = cst.and_then(|cst| passes::lower_cst::lower(&cst));

    if opts.const_fold {
        if let Some(ast) = ast.value_mut() {
            passes::const_fold::const_fold(ast);
        }
    }

    match opts.output_format {
        cli::OutputFormat::AstDot => {
            return ast.map(|ast| inspectors::dot::inspect_ast(&ast).into_bytes());
        }
        cli::OutputFormat::AstRustDbg => {
            return ast.map(|ast| format!("{ast:#?}\n").into_bytes());
        }
        _ => {}
    }

    let ir = ast.and_then(|ast| passes::lower_ast::build_ir_from_ast(&ast));

    assert!(opts.output_format == cli::OutputFormat::IrRustDbg);
    ir.map(|ir| format!("{ir:#?}\n").into_bytes())
}
