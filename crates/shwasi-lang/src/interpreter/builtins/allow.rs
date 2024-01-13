use std::{io, path::PathBuf};

use anyhow::{ensure, Result};
use filedescriptor::{AsRawFileDescriptor, IntoRawFileDescriptor};

use crate::{
    interpreter::builtins::{Args, ArgsValidator, Positionals},
    Shell,
};

pub fn allow(
    shell: &mut Shell,
    args: Args,
    _stdout: &mut (impl io::Write + AsRawFileDescriptor),
    _stdin: Option<impl io::Read + IntoRawFileDescriptor>,
) -> Result<()> {
    ArgsValidator::default()
        .positionals(Positionals::Any)
        .multi("env")
        .multi("e")
        .validate(&args)?;

    if let Some(env_pass) = args.get_argv("env", Some("e")) {
        for env_var in env_pass {
            shell.env.allow_env(env_var);
        }
        if args.positional.is_empty() {
            return Ok(());
        }
    }

    if args.positional.is_empty() {
        shell.env.allow_dir(".");
        return Ok(());
    }
    for dir in &args.positional {
        let path = PathBuf::from(dir);
        ensure!(path.is_dir(), "expected directory to allow");
        shell.env.allow_dir(path);
    }

    Ok(())
}
