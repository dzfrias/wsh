use std::{ffi::OsString, path::PathBuf};

use anyhow::{ensure, Context, Result};
use clap::Parser;

use crate::shell_v2::{pipeline::Stdio, Shell};

#[derive(Debug, Parser)]
struct Args {
    dirs: Vec<PathBuf>,

    #[arg(short, long = "env")]
    env_vars: Vec<String>,
}

pub fn allow<I, S>(shell: &mut Shell, _stdio: Stdio, args: I) -> Result<()>
where
    I: IntoIterator<Item = S>,
    S: Into<OsString> + Clone,
{
    let args = Args::try_parse_from(args)?;

    let has_env_vars = args.env_vars.is_empty();
    for env_var in args.env_vars {
        shell.env.allow_env(env_var);
    }

    if args.dirs.is_empty() && !has_env_vars {
        shell
            .env
            .allow_dir(".")
            .context("error canonicalizing cwd")?;
        return Ok(());
    }
    for path in &args.dirs {
        ensure!(path.is_dir(), "expected directory to allow");
        shell
            .env
            .allow_dir(path)
            .context("error canonicalizing provided path")?;
    }

    Ok(())
}
