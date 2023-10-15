mod cli;

use std::{
    fs::{self, File},
    io::Write,
};

use anyhow::{Context, Result};
use clap::Parser;
use dumbwasm::parse;

use crate::cli::Cli;

fn main() -> Result<()> {
    let args = Cli::parse();
    let input = fs::read_to_string(args.input).context("error reading input file to string")?;

    let out = parse(&input);
    let mut out_file = File::create("out.wasm").context("error creating out file")?;
    out_file
        .write_all(&out)
        .context("error writing buffer to out file")?;

    Ok(())
}
