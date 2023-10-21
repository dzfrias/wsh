use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};

#[derive(Debug, Parser)]
pub struct Cli {
    #[clap(subcommand)]
    pub task: Task,
}

#[derive(Debug, Subcommand)]
pub enum Task {
    /// Generate spec tests for the parser.
    Specgen(Specgen),
}

#[derive(Debug, Args)]
pub struct Specgen {
    #[arg(value_name = "PATH")]
    pub spectests: PathBuf,
}
