use std::path::PathBuf;

use clap::{Parser, ValueEnum};

#[derive(Debug, Parser)]
pub struct Cli {
    /// The input file to generate WebAssembly from.
    pub input: PathBuf,

    /// This flag changes the out file of the WebAssembly generated.
    #[arg(short, value_name = "PATH")]
    pub out: Option<PathBuf>,
    /// This flag controls when the output is colorized.
    #[arg(long, default_value = "auto", value_name = "WHEN")]
    pub color: Color,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum Color {
    Never,
    Auto,
    Always,
}
