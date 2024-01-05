use std::io;

use filedescriptor::IntoRawFileDescriptor;

use crate::{interpreter::builtins::Builtin, Shell, ShellResult};
use which::which as which_bin;

pub fn which<I, S>(
    shell: &mut Shell,
    args: I,
    mut stdout: impl io::Write + IntoRawFileDescriptor,
) -> ShellResult<i32>
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    let mut last_found = true;
    for arg in args {
        let arg = arg.as_ref();
        last_found = true;
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
        last_found = false;
    }

    Ok(if last_found { 0 } else { 1 })
}
