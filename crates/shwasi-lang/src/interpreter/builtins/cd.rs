use std::{env, io, path::PathBuf};

use anyhow::{Context, Result};
use clap::Parser;
use filedescriptor::{AsRawFileDescriptor, IntoRawFileDescriptor};

use crate::Shell;

#[derive(Debug, Parser)]
struct Args {
    dir: Option<PathBuf>,
}

pub fn cd(
    _shell: &mut Shell,
    args: Vec<String>,
    _stdout: &mut (impl io::Write + AsRawFileDescriptor),
    _stdin: Option<impl io::Read + IntoRawFileDescriptor>,
) -> Result<()> {
    let args = Args::try_parse_from(args)?;
    let path = args
        .dir
        .map_or_else(dirs::home_dir, Some)
        .context("no home directory")?;

    env::set_current_dir(path).context("error moving to directory")?;

    Ok(())
}
