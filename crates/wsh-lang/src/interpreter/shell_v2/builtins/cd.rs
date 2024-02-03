use std::{env, ffi::OsString, path::PathBuf};

use anyhow::Context;
use clap::Parser;

use crate::shell_v2::{pipeline::Stdio, Shell};

#[derive(Debug, Parser)]
struct Args {
    dir: Option<PathBuf>,
}

pub fn cd<I, S>(_shell: &mut Shell, _stdio: Stdio, args: I) -> anyhow::Result<()>
where
    I: IntoIterator<Item = S>,
    S: Into<OsString> + Clone,
{
    let args = Args::try_parse_from(args)?;
    let dir = args
        .dir
        .or_else(dirs::home_dir)
        .context("home directory not found")?;
    env::set_current_dir(dir).context("error moving to directory")?;
    Ok(())
}
