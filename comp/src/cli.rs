use crate::{compile::CompileOpts, util::PathOrStd};
use anyhow::{bail, Context};
use clap::{Parser, ValueEnum};
use codespan_reporting::files::SimpleFile;
use std::{fs::File, io::Read};

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, ValueEnum)]
pub enum OutputFormat {
    AstDot,
    AstRustDbg,
    IrRustDbg,
    #[default]
    LlvmIr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum SkippablePasses {
    ConstFold,
}

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    /// The input file, use `-` for std in.
    #[arg(default_value = "-")]
    input_path: PathOrStd,

    /// The output format
    #[arg(short = 'e', long, value_name = "FORMAT", value_enum, default_value_t)]
    emit: OutputFormat,

    /// Zero or more passes to skip
    #[arg(long = "skip", value_name = "PASS", value_enum)]
    skips: Vec<SkippablePasses>,

    /// The output file, use `-` for std out.
    #[arg(short = 'o', long = "output", default_value = "-")]
    output_path: PathOrStd,
}

impl Args {
    pub fn output_format(&self) -> OutputFormat {
        self.emit
    }
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

pub fn extract_compile_opts(args: &Args) -> CompileOpts {
    CompileOpts {
        output_format: args.output_format(),
        const_fold: !args.skips.contains(&SkippablePasses::ConstFold),
    }
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
