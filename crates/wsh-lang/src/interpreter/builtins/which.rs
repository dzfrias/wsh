use std::io::Write;

use clap::Parser;

use crate::{
    interpreter::builtins::{Builtin, IoStreams},
    Shell,
};
use anyhow::Result;
use which::which as which_bin;

#[derive(Debug, Parser)]
struct Args {
    cmds: Vec<String>,
}

pub fn which(shell: &mut Shell, args: Vec<String>, io_streams: &mut IoStreams) -> Result<()> {
    let args = Args::try_parse_from(args)?;
    for arg in args.cmds {
        let arg = arg.as_ref();
        // TODO: print alias itself
        if shell.env.get_alias(arg).is_some() {
            writeln!(io_streams.stdout, "{arg}: alias").expect("error writing to stdout");
            continue;
        }
        if Builtin::from_name(arg).is_some() {
            writeln!(io_streams.stdout, "{arg}: shell builtin").expect("error writing to stdout");
            continue;
        }
        if shell.env.get_module_func(arg).is_some() {
            writeln!(io_streams.stdout, "{arg}: wasm function").expect("error writing to stdout");
            continue;
        }
        if let Ok(path) = which_bin(arg) {
            writeln!(io_streams.stdout, "{}", path.display()).expect("error writing to stdout");
            continue;
        }
        writeln!(io_streams.stdout, "{arg} not found").expect("error writing to stdout");
    }

    Ok(())
}
