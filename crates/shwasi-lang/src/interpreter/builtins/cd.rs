use std::{env, io, path::PathBuf};

use filedescriptor::IntoRawFileDescriptor;

use crate::{Shell, ShellResult};

pub fn cd<I, S>(
    _shell: &mut Shell,
    args: I,
    mut out: impl io::Write + IntoRawFileDescriptor,
) -> ShellResult<i32>
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    let Some(path) = args
        .into_iter()
        .next()
        .map(|arg| PathBuf::from(arg.as_ref()))
        .map_or_else(dirs::home_dir, Some)
    else {
        writeln!(out, "cd: no home directory").expect("error writing to stdout");
        return Ok(1);
    };

    if let Err(err) = env::set_current_dir(&path) {
        writeln!(out, "cd: error moving to {}: {err}", path.display())
            .expect("error writing to stdout");
        return Ok(1);
    }

    Ok(0)
}
