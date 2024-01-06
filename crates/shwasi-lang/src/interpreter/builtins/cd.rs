use std::{env, io, path::PathBuf};

use anyhow::{Context, Result};
use filedescriptor::AsRawFileDescriptor;

use crate::Shell;

pub fn cd<I, S>(
    _shell: &mut Shell,
    args: I,
    _stdout: &mut (impl io::Write + AsRawFileDescriptor),
) -> Result<()>
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    let path = args
        .into_iter()
        .next()
        .map(|arg| PathBuf::from(arg.as_ref()))
        .map_or_else(dirs::home_dir, Some)
        .context("no home directory")?;

    env::set_current_dir(path).context("error moving to directory")?;

    Ok(())
}
