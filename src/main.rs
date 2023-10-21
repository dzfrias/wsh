mod cli;

use std::fs;

use anyhow::{Context, Result};
use clap::Parser as CliParser;
use shwasi_parser::Parser;
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

use crate::cli::Cli;

#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

fn main() -> Result<()> {
    // Heap profiler setup
    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::new_heap();

    // Logging setup
    let fmt_layer = fmt::layer();
    let filter_layer = EnvFilter::try_from_default_env()
        .or_else(|_| EnvFilter::try_new("info"))
        .expect("\"info\" should be a valid directive");
    tracing_subscriber::registry()
        .with(filter_layer)
        .with(fmt_layer)
        .init();

    let args = Cli::parse();
    let input = fs::read(args.input).context("error reading input file")?;

    let parser = Parser::new(&input);
    let module = parser.read_module()?;

    println!("{}", module.codes[0].body);

    Ok(())
}
