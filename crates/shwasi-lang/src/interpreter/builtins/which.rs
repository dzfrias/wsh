use std::io;

use filedescriptor::{AsRawFileDescriptor, IntoRawFileDescriptor};

use crate::{
    interpreter::builtins::{Args, ArgsValidator, Builtin, Positionals},
    Shell,
};
use anyhow::Result;
use which::which as which_bin;

pub fn which(
    shell: &mut Shell,
    args: Args,
    stdout: &mut (impl io::Write + AsRawFileDescriptor),
    _stdin: Option<impl io::Read + IntoRawFileDescriptor>,
) -> Result<()> {
    ArgsValidator::default()
        .positionals(Positionals::Min(1))
        .validate(&args)?;

    for arg in args.positional {
        let arg = arg.as_ref();
        // TODO: print alias itself
        if shell.env.get_alias(arg).is_some() {
            writeln!(stdout, "{arg}: alias").expect("error writing to stdout");
            continue;
        }
        if Builtin::from_name(arg).is_some() {
            writeln!(stdout, "{arg}: shell builtin").expect("error writing to stdout");
            continue;
        }
        if shell.env.get_module_func(arg).is_some() {
            writeln!(stdout, "{arg}: wasm function").expect("error writing to stdout");
            continue;
        }
        if let Ok(path) = which_bin(arg) {
            writeln!(stdout, "{}", path.display()).expect("error writing to stdout");
            continue;
        }
        writeln!(stdout, "{arg} not found").expect("error writing to stdout");
    }

    Ok(())
}
