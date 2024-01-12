use std::{env, io, path::PathBuf};

use anyhow::{Context, Result};
use filedescriptor::{AsRawFileDescriptor, IntoRawFileDescriptor};

use crate::{interpreter::builtins::Args, Shell};

pub fn cd(
    _shell: &mut Shell,
    args: Args,
    _stdout: &mut (impl io::Write + AsRawFileDescriptor),
    _stdin: Option<impl io::Read + IntoRawFileDescriptor>,
) -> Result<()> {
    let path = args
        .positional
        .first()
        .map(PathBuf::from)
        .map_or_else(dirs::home_dir, Some)
        .context("no home directory")?;

    env::set_current_dir(path).context("error moving to directory")?;

    Ok(())
}
