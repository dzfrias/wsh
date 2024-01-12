use std::{io, path::PathBuf};

use anyhow::{ensure, Context, Result};
use filedescriptor::{AsRawFileDescriptor, IntoRawFileDescriptor};

use crate::{interpreter::builtins::Args, Shell};

pub fn allow(
    shell: &mut Shell,
    args: Args,
    _stdout: &mut (impl io::Write + AsRawFileDescriptor),
    _stdin: Option<impl io::Read + IntoRawFileDescriptor>,
) -> Result<()> {
    let path = args
        .positional
        .first()
        .map_or_else(|| PathBuf::from("."), PathBuf::from);
    ensure!(path.is_dir(), "expected directory to allow");
    shell
        .env
        .allow_dir(path)
        .context("error giving directory ambient authority")?;

    Ok(())
}
