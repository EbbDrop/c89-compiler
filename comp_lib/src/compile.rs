use std::collections::HashSet;

use crate::{
    codegen,
    diagnostic::{AggregateResult, Code},
    inspectors, passes,
};

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
pub enum OutputFormat {
    AntlrTree,
    AstDot,
    AstRustDbg,
    IrDot,
    IrRustDbg,
    SymbolTableAscii,
    #[default]
    LlvmIr,
}

pub struct CompileOpts {
    output_format: OutputFormat,
    const_fold: bool,
    analyze_control_flow: bool,
    upgrade_to_err: HashSet<Code>,
}

pub struct CompileOptsBuilder {
    output_format: OutputFormat,
    const_fold: bool,
    analyze_control_flow: bool,
    upgrade_to_err: HashSet<Code>,
}

impl Default for CompileOptsBuilder {
    fn default() -> Self {
        Self {
            output_format: OutputFormat::default(),
            const_fold: true,
            analyze_control_flow: true,
            upgrade_to_err: HashSet::default(),
        }
    }
}

impl CompileOptsBuilder {
    /// Output llvm, with const_fold on and not upgrading any diagnostics.
    pub fn new() -> Self {
        Self::default()
    }

    pub fn output_format(mut self, format: OutputFormat) -> Self {
        self.output_format = format;
        self
    }

    /// Set const folding
    pub fn const_fold(mut self, const_fold: bool) -> Self {
        self.const_fold = const_fold;
        self
    }

    pub fn analyze_control_flow(mut self, analyze_control_flow: bool) -> Self {
        self.analyze_control_flow = analyze_control_flow;
        self
    }

    /// upgrade diagnostics to be inline with the exercises
    /// at the moement these are the codes:
    /// - [`Code::UndeclaredIdent`]
    /// - [`Code::IncompatibleAssign`]
    /// - [`Code::MultiByteChar`]
    pub fn for_assignments(mut self) -> Self {
        self.upgrade_to_err.insert(Code::UnspecifiedType);
        self.upgrade_to_err.insert(Code::IncompatibleAssign);
        self.upgrade_to_err.insert(Code::IncompatibleReturn);
        self.upgrade_to_err.insert(Code::IncompatibleArg);
        self.upgrade_to_err.insert(Code::MultiByteChar);
        self
    }

    pub fn with_code_to_upgrade(mut self, code: Code) -> Self {
        self.upgrade_to_err.insert(code);
        self
    }

    pub fn build(self) -> CompileOpts {
        CompileOpts {
            output_format: self.output_format,
            const_fold: self.const_fold,
            analyze_control_flow: self.analyze_control_flow,
            upgrade_to_err: self.upgrade_to_err,
        }
    }
}

pub fn compile(source: &str, source_name: &str, opts: &CompileOpts) -> AggregateResult<Vec<u8>> {
    let mut res = run_compile(source, source_name, opts);
    res.upgrade_diagnostics(|d| opts.upgrade_to_err.contains(d.code()));
    res
}

fn run_compile(source: &str, source_name: &str, opts: &CompileOpts) -> AggregateResult<Vec<u8>> {
    if opts.output_format == OutputFormat::AntlrTree {
        let antlr_tree = passes::parse::parse_to_antlr_tree(source);
        return antlr_tree.map(String::into_bytes);
    }

    let cst = passes::parse::parse_to_cst(source);

    let mut ast = cst.and_then(|cst| passes::lower_cst::lower(&cst));

    if opts.const_fold {
        if let Some(ast) = ast.value_mut() {
            passes::const_fold::const_fold(ast);
        }
    }

    match opts.output_format {
        OutputFormat::AstDot => {
            return ast.map(|ast| inspectors::dot::inspect_ast(&ast).into_bytes());
        }
        OutputFormat::AstRustDbg => {
            return ast.map(|ast| format!("{ast:#?}\n").into_bytes());
        }
        _ => {}
    }

    let mut res = ast.and_then(|ast| passes::lower_ast::build_ir_from_ast(&ast));

    if opts.analyze_control_flow {
        if let Some(ir) = res.value_mut() {
            let extra_diags = passes::dead_code_removal::remove_dead_code(ir);
            for diag in extra_diags {
                res.add_rec_diagnostic(diag);
            }
        }
    }

    match opts.output_format {
        OutputFormat::IrDot => return res.map(|ir| inspectors::dot::inspect_ir(&ir).into_bytes()),
        OutputFormat::IrRustDbg => return res.map(|ir| format!("{ir:#?}\n").into_bytes()),
        OutputFormat::SymbolTableAscii => {
            return res.map(|ir| {
                let mut s = String::new();
                for (name, funcion) in ir.functions {
                    s += &format!("function {}:\n", name);
                    s += &format!("{:^8}|{:^10}\n", "id", "type");
                    s += &format!("{:-^8}|{:-^10}\n", "", "");
                    // TODO: relies on unspecified order
                    for (i, entry) in funcion.table.items().enumerate() {
                        s += &format!("{:^8}|{:^10}\n", i, entry.ty);
                    }
                    s.push('\n');
                }
                s.into_bytes()
            });
        }
        _ => {}
    }

    let llvm_ir = res.and_then(|ir| codegen::llvm::build_from_ir(&ir, source_name, source));

    assert_eq!(opts.output_format, OutputFormat::LlvmIr);
    llvm_ir.map(|s| format!("{s}\n").into_bytes())
}
