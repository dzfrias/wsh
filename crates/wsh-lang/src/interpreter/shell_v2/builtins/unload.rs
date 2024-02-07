use std::{ffi::OsString, io::Write};

use clap::Parser;

use crate::shell_v2::{builtins::force_writeln, pipeline::Stdio, Shell};

#[derive(Debug, Parser)]
struct Args;

pub fn unload<I, S>(
    shell: &mut Shell,
    mut stdio: Stdio,
    args: I,
    _env: &[(String, String)],
) -> anyhow::Result<()>
where
    I: IntoIterator<Item = S>,
    S: Into<OsString> + Clone,
{
    Args::try_parse_from(args)?;
    let unloaded = shell.env.unload_modules();
    force_writeln!(stdio.stdout, "unloaded {unloaded} modules");
    Ok(())
}
