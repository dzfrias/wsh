mod cli;

use std::fs;

use anyhow::{Context, Result};
use clap::Parser as CliParser;
use shwasi_engine::{Instance, Store};
use shwasi_parser::Parser;
use tracing_subscriber::{filter::LevelFilter, fmt, prelude::*, EnvFilter};

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
    let filter_layer = EnvFilter::builder()
        .with_default_directive(LevelFilter::WARN.into())
        .from_env()
        .context("error reading logging directives")?;
    tracing_subscriber::registry()
        .with(filter_layer)
        .with(fmt_layer)
        .init();

    let args = Cli::parse();
    let input = fs::read(args.input).context("error reading input file")?;

    let parser = Parser::new(&input);
    let module = parser.read_module()?;
    let mut store = Store::default();
    let instance = Instance::instantiate(&mut store, module)?;
    let fib = instance.get_func::<u32, u32>(&store, "fib")?;
    let result = fib.call(&mut store, 39)?;
    dbg!(result);

    Ok(())
}
