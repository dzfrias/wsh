use std::{io, path::PathBuf};

use anyhow::{ensure, Result};
use filedescriptor::{AsRawFileDescriptor, IntoRawFileDescriptor};

use crate::{interpreter::builtins::Args, Shell};

pub fn allow(
    shell: &mut Shell,
    args: Args,
    _stdout: &mut (impl io::Write + AsRawFileDescriptor),
    _stdin: Option<impl io::Read + IntoRawFileDescriptor>,
) -> Result<()> {
    if let Some(env_pass) = args.get_argv("env", Some("e")) {
        for env_var in env_pass {
            shell.env.allow_env(env_var);
        }
        return Ok(());
    }

    let path = args
        .positional
        .first()
        .map_or_else(|| PathBuf::from("."), PathBuf::from);
    ensure!(path.is_dir(), "expected directory to allow");
    shell.env.allow_dir(path);

    Ok(())
}
