use std::{ffi::OsString, path::PathBuf};

use anyhow::{Context, Result};
use clap::Parser;

use crate::shell_v2::{env::Location, pipeline::Stdio, Shell};

#[derive(Debug, Parser)]
struct Args {
    dirs: Vec<PathBuf>,

    #[arg(short, long = "virtual")]
    virt: bool,
    #[arg(short, long = "env")]
    env_vars: Vec<String>,
}

pub fn allow<I, S>(
    shell: &mut Shell,
    _stdio: Stdio,
    args: I,
    _env: &[(String, String)],
) -> Result<()>
where
    I: IntoIterator<Item = S>,
    S: Into<OsString> + Clone,
{
    let args = Args::try_parse_from(args)?;

    let has_env_vars = !args.env_vars.is_empty();
    for env_var in args.env_vars {
        shell.env.allow_env(env_var);
    }

    let location = if args.virt {
        Location::Memory
    } else {
        Location::Disk
    };
    if args.dirs.is_empty() && !has_env_vars {
        shell
            .env
            .allow_dir(".", location)
            .context("error allowing cwd")?;
        return Ok(());
    }
    for path in &args.dirs {
        shell
            .env
            .allow_dir(path, location)
            .context("error allowing directory")?;
    }

    Ok(())
}
