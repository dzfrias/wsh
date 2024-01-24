use std::path::PathBuf;

use anyhow::{ensure, Result};
use clap::Parser;

use crate::{
    interpreter::{builtins::IoStreams, env::Location},
    Shell,
};

#[derive(Debug, Parser)]
struct Args {
    dirs: Vec<PathBuf>,

    #[arg(short, long = "env")]
    env_vars: Vec<String>,
    #[arg(short, long = "virtual")]
    virt: bool,
}

pub fn allow(shell: &mut Shell, args: Vec<String>, _io_streams: &mut IoStreams) -> Result<()> {
    let args = Args::try_parse_from(args)?;
    for env_var in args.env_vars {
        shell.env.allow_env(env_var);
    }

    let loc = if args.virt {
        Location::Memory
    } else {
        Location::Disk
    };
    if args.dirs.is_empty() {
        shell.env.allow_dir(".", loc);
        return Ok(());
    }
    for path in &args.dirs {
        ensure!(path.is_dir(), "expected directory to allow");
        shell.env.allow_dir(path, loc);
    }

    Ok(())
}
