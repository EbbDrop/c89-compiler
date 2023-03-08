use std::{
    fs::{File, OpenOptions},
    io::{self, Read, Write},
    path::PathBuf,
};

use anyhow::{bail, Context, Result};
use clap::{Parser, ValueEnum};
use comp::{generators::dot::to_dot, parser};

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

    let input_string = match args.input_path {
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
            s
        }
        PathOrStd::StdStream => {
            let mut handle = io::stdin().lock();
            let mut s = String::new();
            handle
                .read_to_string(&mut s)
                .context("Failed to read from stdin")?;
            s
        }
    };

    let ast = parser::parse(&input_string)?;

    let output = match args.emit {
        OutputFormat::Dot => to_dot(&ast).into_bytes(),
        OutputFormat::RustDbg => format!("{:#?}\n", &ast).into_bytes(),
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
