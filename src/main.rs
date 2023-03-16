use std::{
    fs::{File, OpenOptions},
    io::{self, Read, Write},
    path::PathBuf,
};

use anyhow::{bail, Context, Result};
use clap::{Parser, ValueEnum};
use codespan_reporting::{
    diagnostic::{Label, Severity},
    files::SimpleFile,
    term,
};
use comp::{
    diagnostic::{AggregateResult, Code, DiagnosticKind},
    generators::dot::to_dot,
    parser, passes,
};

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum OutputFormat {
    #[default]
    Dot,
    RustDbg,
}

#[derive(Debug, Clone)]
enum PathOrStd {
    Path(PathBuf),
    StdStream,
}

impl From<&std::ffi::OsStr> for PathOrStd {
    fn from(value: &std::ffi::OsStr) -> Self {
        if value == "-" {
            Self::StdStream
        } else {
            Self::Path(value.into())
        }
    }
}

fn eprint_aggregate<'files, T, F>(aggregate: &AggregateResult<T>, files: &'files F)
where
    F: codespan_reporting::files::Files<'files, FileId = ()>,
{
    let mut writer = term::termcolor::StandardStream::stderr(term::termcolor::ColorChoice::Auto);
    let config = term::Config {
        chars: term::Chars {
            single_primary_caret: '─',
            single_secondary_caret: '─',
            multi_primary_caret_start: '╯',
            multi_secondary_caret_start: '╯',
            multi_primary_caret_end: '╯',
            multi_secondary_caret_end: '╯',
            ..term::Chars::box_drawing()
        },

        ..Default::default()
    };

    for (t, d) in aggregate.diagnostics() {
        let severity = match t {
            DiagnosticKind::Rec => Severity::Warning,
            DiagnosticKind::Err => Severity::Error,
        };

        let mut labels = Vec::with_capacity(1 + d.additional_spans_len());

        labels.push({
            let mut l = Label::primary((), *d.main_span());
            if let Some(m) = d.main_span_message() {
                l = l.with_message(m);
            }
            l
        });

        for (span, message) in d.additional_spans() {
            let mut l = Label::secondary((), *span);
            if let Some(m) = message {
                l = l.with_message(m);
            }
            labels.push(l);
        }

        let mut diagnostic = codespan_reporting::diagnostic::Diagnostic::new(severity)
            .with_message(d.message())
            .with_labels(labels);

        if d.code() != &Code::Unspecified {
            diagnostic = diagnostic.with_code(d.code().to_string())
        }

        term::emit(&mut writer, &config, files, &diagnostic).unwrap();
    }
}

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The input file, use `-` for std in.
    #[arg(default_value = "-")]
    input_path: PathOrStd,

    /// The output format
    #[arg(short = 'e', long, value_name = "FORMAT", value_enum, default_value_t)]
    emit: OutputFormat,

    /// The output file, use `-` for std out.
    #[arg(short = 'o', long = "output", default_value = "-")]
    output_path: PathOrStd,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let source = match args.input_path {
        PathOrStd::Path(path) => {
            if !path.exists() {
                bail!("Input file `{}` doesn't exist", path.display());
            }
            let mut handle = File::open(&path)
                .with_context(|| format!("Failed to open input file `{}`", path.display()))?;
            let mut s = String::new();
            handle
                .read_to_string(&mut s)
                .with_context(|| format!("Failed to read from input file `{}`", path.display()))?;

            SimpleFile::new(path.file_name().unwrap().to_string_lossy().into_owned(), s)
        }
        PathOrStd::StdStream => {
            let mut handle = io::stdin().lock();
            let mut s = String::new();
            handle
                .read_to_string(&mut s)
                .context("Failed to read from stdin")?;

            SimpleFile::new("stdin stream".to_owned(), s)
        }
    };

    let mut parse_result = parser::parse(source.source());
    if !parse_result.is_ok() {
        eprint_aggregate(&parse_result, &source);
    }
    let ast = match parse_result.value_mut() {
        Some(ast) => ast,
        None => bail!("couldn't compile due to the previous errors"),
    };

    passes::const_fold::const_fold(ast);

    let output = match args.emit {
        OutputFormat::Dot => to_dot(ast).into_bytes(),
        OutputFormat::RustDbg => format!("{:#?}\n", ast).into_bytes(),
    };

    match args.output_path {
        PathOrStd::Path(path) => {
            let mut handle = OpenOptions::new()
                .write(true)
                .create(true)
                .open(&path)
                .with_context(|| format!("Failed to open output file `{}`", path.display()))?;
            handle
                .write_all(&output)
                .with_context(|| format!("Failed to write to output file `{}`", path.display()))?;
        }
        PathOrStd::StdStream => {
            let mut handle = io::stdout().lock();
            handle
                .write_all(&output)
                .context("Failed to write to stdout")?;
        }
    }

    Ok(())
}
