use std::{ffi::OsString, io::Write};

use clap::Parser;

use crate::shell_v2::{
    builtins::{force_write, force_writeln},
    pipeline::Stdio,
    Shell,
};

#[derive(Debug, Parser)]
struct Args {
    args: Vec<String>,
}

pub fn error<I, S>(
    _shell: &mut Shell,
    mut stdio: Stdio,
    args: I,
    _env: &[(String, String)],
) -> anyhow::Result<()>
where
    I: IntoIterator<Item = S>,
    S: Into<OsString> + Clone,
{
    let args = Args::try_parse_from(args)?;
    let Some((last, rest)) = args.args.split_last() else {
        return Ok(());
    };
    for arg in rest {
        force_write!(stdio.stderr, "{arg} ");
    }
    force_writeln!(stdio.stderr, "{last}");
    Ok(())
}
