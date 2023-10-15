use std::path::PathBuf;

use clap::Parser;

#[derive(Debug, Parser)]
pub struct Cli {
    /// The input file to generate WASM from.
    pub input: PathBuf,
}
