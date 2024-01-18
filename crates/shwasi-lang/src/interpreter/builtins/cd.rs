use std::{env, path::PathBuf};

use anyhow::{Context, Result};
use clap::Parser;

use crate::{interpreter::builtins::IoStreams, Shell};

#[derive(Debug, Parser)]
struct Args {
    dir: Option<PathBuf>,
}

pub fn cd(_shell: &mut Shell, args: Vec<String>, _io_streams: &mut IoStreams) -> Result<()> {
    let args = Args::try_parse_from(args)?;
    let path = args
        .dir
        .map_or_else(dirs::home_dir, Some)
        .context("no home directory")?;

    env::set_current_dir(path).context("error moving to directory")?;

    Ok(())
}
