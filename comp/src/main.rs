mod cli;
mod report;
mod util;

use anyhow::{bail, Context, Result};
use clap::Parser;

use comp_lib::compile;
use compile::compile;
use std::io::Write;

fn main() -> Result<()> {
    let args = cli::Args::parse();

    let source = cli::open_input_source(&args)?;
    let source_name = source.name().clone();

    let compile_opts = cli::extract_compile_opts(&args);
    let res = compile(source.source(), &source_name, &compile_opts);

    if !res.is_ok() {
        report::eprint_aggregate(&res, &source);
    }

    let Some(output) = res.into_value() else {
        bail!("couldn't compile due to the previous errors");
    };

    cli::open_output(&args)?
        .write_all(&output)
        .with_context(|| "Failed to write to output".to_string())?;

    Ok(())
}
