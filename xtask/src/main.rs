mod cli;
mod specgen;

use anyhow::Result;
use clap::Parser;

use crate::cli::{Cli, Task};

fn main() -> Result<()> {
    let args = Cli::parse();

    match args.task {
        Task::Specgen(specgen) => {
            specgen::specgen(&specgen)?;
        }
    }

    Ok(())
}
