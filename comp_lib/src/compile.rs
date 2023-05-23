use std::collections::HashSet;

pub use crate::settings::Target;
use crate::{
    codegen,
    diagnostic::{AggregateResult, Code},
    inspectors, passes,
    settings::Settings,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum OutputFormat {
    AntlrTree,
    AstDot,
    AstRustDbg,
    IrDot,
    IrRustDbg,
    SymbolTableAscii,
    LlvmIr,
    MipsDbg,
    MipsAsm,
}

impl std::fmt::Display for OutputFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            OutputFormat::AntlrTree => "antlr tree",
            OutputFormat::AstDot => "ast dot",
            OutputFormat::AstRustDbg => "ast rust dbg",
            OutputFormat::IrDot => "ir dot",
            OutputFormat::IrRustDbg => "ir rust dbg",
            OutputFormat::SymbolTableAscii => "symbol table",
            OutputFormat::LlvmIr => "llvm ir",
            OutputFormat::MipsDbg => "mips dbg",
            OutputFormat::MipsAsm => "mips assembly",
        };
        write!(f, "{name}")
    }
}

#[derive(Debug, Clone)]
pub struct CompileOpts {
    output_format: OutputFormat,
    settings: Settings,
    const_fold: bool,
    analyze_control_flow: bool,
    upgrade_to_err: HashSet<Code>,
}

#[derive(Debug, Clone)]
pub struct CompileOptsBuilder {
    output_format: Option<OutputFormat>,
    target: Target,
    const_fold: bool,
    analyze_control_flow: bool,
    upgrade_to_err: HashSet<Code>,
}

#[derive(Debug, Clone)]
pub enum CompileOptsErr {
    IncompatibleFormatAndTarget(OutputFormat, Target),
}

impl std::fmt::Display for CompileOptsErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileOptsErr::IncompatibleFormatAndTarget(format, target) => {
                write!(f, "Can't use the {format} format with the {target} target.")
            }
        }
    }
}

impl std::error::Error for CompileOptsErr {}

impl Default for CompileOptsBuilder {
    fn default() -> Self {
        Self {
            output_format: None,
            target: Target::X86_64,
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
        self.output_format = Some(format);
        self
    }

    // Set the target
    pub fn target(mut self, target: Target) -> Self {
        self.target = target;
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

    pub fn build(self) -> Result<CompileOpts, CompileOptsErr> {
        let output_format = match self.output_format {
            Some(format) => match (&self.target, &format) {
                (Target::X86_64, OutputFormat::MipsAsm) => {
                    return Err(CompileOptsErr::IncompatibleFormatAndTarget(
                        format,
                        self.target,
                    ))
                }
                _ => format,
            },
            None => match &self.target {
                Target::X86_64 => OutputFormat::LlvmIr,
                Target::Mips => OutputFormat::MipsAsm,
            },
        };
        let settings = Settings {
            target: self.target,
        };
        Ok(CompileOpts {
            output_format,
            settings,
            const_fold: self.const_fold,
            analyze_control_flow: self.analyze_control_flow,
            upgrade_to_err: self.upgrade_to_err,
        })
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

    let mut res = ast.and_then(|ast| passes::lower_ast::build_ir_from_ast(&ast, &opts.settings));

    if opts.analyze_control_flow {
        if let Some(ir) = res.value_mut() {
            let extra_diags = passes::dead_code_removal::remove_dead_code(ir);
            for diag in extra_diags {
                res.add_rec_diagnostic(diag);
            }
        }
    }

    match opts.output_format {
        OutputFormat::IrDot => res.map(|ir| inspectors::dot::inspect_ir(&ir).into_bytes()),
        OutputFormat::IrRustDbg => res.map(|ir| format!("{ir:#?}\n").into_bytes()),
        OutputFormat::SymbolTableAscii => res.map(|ir| {
            let mut s = String::new();
            for (name, funcion) in ir.functions {
                s += &format!("function {}:\n", name);
                s += &format!(
                    "{:^8}|{:^10}|{:^14}|{:^6}\n",
                    "id", "type", "needs address", "init"
                );
                s += &format!("{:-^8}|{:-^10}|{:-^14}|{:-^6}\n", "", "", "", "");
                for (i, entry) in funcion.table.iter() {
                    s += &format!(
                        "{:^8}|{:<10}|{:^14}|{:^6}\n",
                        i.to_string(),
                        entry.ty.to_string(),
                        entry.needs_address,
                        entry.initialized
                    );
                }
                s.push('\n');
            }
            s.into_bytes()
        }),
        OutputFormat::LlvmIr => {
            let llvm_ir = res.and_then(|ir| {
                codegen::llvm::build_from_ir(&ir, &opts.settings, source_name, source)
            });

            llvm_ir.map(|s| format!("{s}\n").into_bytes())
        }
        OutputFormat::MipsDbg | OutputFormat::MipsAsm => {
            let mips_ir = res.and_then(|ir| {
                codegen::mips::build_from_ir(&ir, &opts.settings, source_name, source)
            });

            let config = if opts.output_format == OutputFormat::MipsDbg {
                mips_ir::MipsOutputConfig {
                    use_register_names: true,
                    allow_virtuals: true,
                    show_block_arguments: true,
                    show_all_blocks: false,
                }
            } else {
                //TODO also do the other algos in this case
                mips_ir::MipsOutputConfig {
                    use_register_names: true,
                    allow_virtuals: false,
                    show_block_arguments: false,
                    show_all_blocks: false,
                }
            };

            mips_ir.map(|mir| {
                let mut output = String::new();

                mips_ir::MipsOutputter::new(&mut output)
                    .with_config(config)
                    .write_root(&mir)
                    .unwrap();

                output.into_bytes()
            })
        }
        _ => unreachable!(
            "Format {:?} should have been handled before",
            opts.output_format
        ),
    }
}
