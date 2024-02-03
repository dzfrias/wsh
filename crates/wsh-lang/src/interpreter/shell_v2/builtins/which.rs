use std::{ffi::OsString, io::Write};

use clap::Parser;

use anyhow::Result;
use which::which as which_bin;

use crate::shell_v2::{
    builtins::{force_writeln, Builtin},
    pipeline::Stdio,
    Shell,
};

#[derive(Debug, Parser)]
struct Args {
    cmds: Vec<String>,
}

pub fn which<I, S>(_shell: &mut Shell, mut stdio: Stdio, args: I) -> Result<()>
where
    I: IntoIterator<Item = S>,
    S: Into<OsString> + Clone,
{
    let args = Args::try_parse_from(args)?;
    for arg in args.cmds {
        let arg = arg.as_ref();
        if Builtin::from_name(arg).is_some() {
            force_writeln!(stdio.stdout, "{arg}: shell builtin");
            continue;
        }
        if let Ok(path) = which_bin(arg) {
            force_writeln!(stdio.stdout, "{}", path.display());
            continue;
        }
        writeln!(stdio.stdout, "{arg} not found").expect("error writing to stdout");
    }

    Ok(())
}
