use std::{env, ffi::OsStr, fs, io, path::PathBuf};

use filedescriptor::{FileDescriptor, FromRawFileDescriptor, IntoRawFileDescriptor};

use crate::{Shell, ShellError, ShellResult};

#[derive(Debug)]
pub enum Builtin {
    Cd,
    Source,
}

impl Builtin {
    pub fn from_name(name: impl AsRef<OsStr>) -> Option<Self> {
        match name.as_ref().to_str()? {
            "cd" => Some(Self::Cd),
            "source" => Some(Self::Source),
            _ => None,
        }
    }

    pub fn run<I, S>(
        &self,
        shell: &mut Shell,
        args: I,
        stdout: impl io::Write + IntoRawFileDescriptor,
    ) -> ShellResult<i32>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        match self {
            Self::Cd => cd(args, stdout),
            Self::Source => source(shell, args, stdout),
        }
    }
}

fn cd<I, S>(args: I, mut out: impl io::Write + IntoRawFileDescriptor) -> ShellResult<i32>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let Some(path) = args
        .into_iter()
        .next()
        .map(|arg| PathBuf::from(arg.as_ref()))
        .map_or_else(dirs::home_dir, Some)
    else {
        writeln!(out, "cd: no home directory").map_err(ShellError::BuiltinWriteError)?;
        return Ok(1);
    };

    if let Err(err) = env::set_current_dir(&path) {
        writeln!(out, "cd: error moving to {}: {err}", path.display())
            .map_err(ShellError::BuiltinWriteError)?;
        return Ok(1);
    }

    Ok(0)
}

fn source<I, S>(
    shell: &mut Shell,
    args: I,
    mut stdout: impl io::Write + IntoRawFileDescriptor,
) -> ShellResult<i32>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let args = args.into_iter().collect::<Vec<_>>();
    // TOOD: support passing args to scripts
    let Some((file, _args)) = args.split_first() else {
        writeln!(stdout, "source: no file provided").map_err(ShellError::BuiltinWriteError)?;
        return Ok(1);
    };
    let contents = match fs::read_to_string(file.as_ref()) {
        Ok(contents) => contents,
        Err(err) => {
            writeln!(stdout, "source: error reading file: {err}")
                .map_err(ShellError::BuiltinWriteError)?;
            return Ok(1);
        }
    };
    let fd = stdout.into_raw_file_descriptor();
    // SAFETY: `fd` is a valid file descriptor on account of it being from an
    // `IntoRawFileDescriptor`. We later close it, so we don't leak it.
    unsafe { shell.stdout(fd) };
    shell.run(&contents, &file.as_ref().to_string_lossy())?;
    // We need to be careful that the file descriptor is closed, so we don't leak it. This can
    // cause hangs in the shell if used `source` is used with a pipe. This is because the shell
    // will never close the `stdout` it's given, so we must manually close it here.
    let _ = unsafe { FileDescriptor::from_raw_file_descriptor(fd) };

    Ok(1)
}
