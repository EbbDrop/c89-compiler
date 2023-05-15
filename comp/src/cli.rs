use crate::util::PathOrStd;

use comp_lib::compile::{self, CompileOpts, CompileOptsBuilder, CompileOptsErr};

use anyhow::{bail, Context};
use clap::{Parser, ValueEnum};
use codespan_reporting::files::SimpleFile;

use std::{fs::File, io::Read};

#[derive(Debug, Copy, Clone, PartialEq, Eq, ValueEnum)]
pub enum OutputFormat {
    AntlrTree,
    AstDot,
    AstRustDbg,
    IrDot,
    IrRustDbg,
    SymbolTableAscii,
    LlvmIr,
    MipsAsm,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, ValueEnum)]
pub enum Target {
    X86_64,
    Mips,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum SkippablePasses {
    ConstFold,
    ControlFlowAnalysis,
}

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    /// The input file, use `-` for std in.
    #[arg(default_value = "-")]
    input_path: PathOrStd,

    /// The compile target. Defaults to x86-64
    #[arg(short = 't', long, value_name = "TARGET", value_enum)]
    target: Option<Target>,

    /// The output format. Default based on the target, llvm-ir for X86_64 and mips-asm for MIPS.
    #[arg(short = 'e', long, value_name = "FORMAT", value_enum)]
    emit: Option<OutputFormat>,

    /// Zero or more passes to skip
    #[arg(long = "skip", value_name = "PASS", value_enum)]
    skips: Vec<SkippablePasses>,

    /// The output file, use `-` for std out.
    #[arg(short = 'o', long = "output", default_value = "-")]
    output_path: PathOrStd,
}

pub fn open_input_source(args: &Args) -> anyhow::Result<SimpleFile<String, String>> {
    match &args.input_path {
        PathOrStd::Path(path) => {
            if !path.exists() {
                bail!("Input file `{}` doesn't exist", path.display());
            }
            let mut handle = File::open(path)
                .with_context(|| format!("Failed to open input file `{}`", path.display()))?;
            let mut s = String::new();
            handle
                .read_to_string(&mut s)
                .with_context(|| format!("Failed to read from input file `{}`", path.display()))?;

            Ok(SimpleFile::new(
                path.file_name().unwrap().to_string_lossy().into_owned(),
                s,
            ))
        }
        PathOrStd::StdStream => {
            let mut handle = std::io::stdin().lock();
            let mut s = String::new();
            handle
                .read_to_string(&mut s)
                .context("Failed to read from stdin")?;

            Ok(SimpleFile::new("stdin stream".to_owned(), s))
        }
    }
}

pub fn extract_compile_opts(args: &Args) -> Result<CompileOpts, CompileOptsErr> {
    let opts = CompileOptsBuilder::new();

    let opts = if let Some(format) = args.emit {
        let format = match format {
            OutputFormat::AntlrTree => compile::OutputFormat::AntlrTree,
            OutputFormat::AstDot => compile::OutputFormat::AstDot,
            OutputFormat::AstRustDbg => compile::OutputFormat::AstRustDbg,
            OutputFormat::IrDot => compile::OutputFormat::IrDot,
            OutputFormat::IrRustDbg => compile::OutputFormat::IrRustDbg,
            OutputFormat::SymbolTableAscii => compile::OutputFormat::SymbolTableAscii,
            OutputFormat::LlvmIr => compile::OutputFormat::LlvmIr,
            OutputFormat::MipsAsm => compile::OutputFormat::MipsAsm,
        };
        opts.output_format(format)
    } else {
        opts
    };

    let opts = if let Some(target) = args.target {
        let target = match target {
            Target::X86_64 => compile::Target::X86_64,
            Target::Mips => compile::Target::Mips,
        };
        opts.target(target)
    } else {
        opts
    };

    opts.for_assignments()
        .const_fold(!args.skips.contains(&SkippablePasses::ConstFold))
        .analyze_control_flow(!args.skips.contains(&SkippablePasses::ControlFlowAnalysis))
        .build()
}

pub fn open_output(args: &Args) -> anyhow::Result<Box<dyn std::io::Write>> {
    match &args.output_path {
        PathOrStd::Path(path) => std::fs::OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open(path)
            .map(|f| Box::new(f) as Box<dyn std::io::Write>)
            .with_context(|| format!("Failed to open output file `{}`", path.display())),
        PathOrStd::StdStream => Ok(Box::new(std::io::stdout().lock())),
    }
}
