use std::io;

use filedescriptor::IntoRawFileDescriptor;

use crate::{Shell, ShellResult};

pub fn unload<I, S>(
    shell: &mut Shell,
    args: I,
    mut stdout: impl io::Write + IntoRawFileDescriptor,
) -> ShellResult<i32>
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    if args.into_iter().next().is_some() {
        writeln!(stdout, "unload: expected no arguments").expect("error writing to stdout");
        return Ok(1);
    }

    let unloaded = shell.env.unload_modules();
    writeln!(stdout, "unloaded {unloaded} modules").expect("error writing to stdout");
    Ok(0)
}
