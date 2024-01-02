use std::{env, ffi::OsStr, fs, io, path::PathBuf};

use filedescriptor::IntoRawFileDescriptor;

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
    ) -> ShellResult<()>
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

fn cd<I, S>(args: I, mut out: impl io::Write + IntoRawFileDescriptor) -> ShellResult<()>
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
        writeln!(out, "cd: no home directory").map_err(ShellError::CommandFailed)?;
        return Ok(());
    };

    if let Err(err) = env::set_current_dir(&path) {
        writeln!(out, "cd: error moving to {}: {err}", path.display())
            .map_err(ShellError::CommandFailed)?;
    }

    Ok(())
}

fn source<I, S>(
    shell: &mut Shell,
    args: I,
    mut stdout: impl io::Write + IntoRawFileDescriptor,
) -> ShellResult<()>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let args = args.into_iter().collect::<Vec<_>>();
    // TOOD: support passing args to scripts
    let Some((file, _args)) = args.split_first() else {
        writeln!(stdout, "source: no file provided").map_err(ShellError::CommandFailed)?;
        return Ok(());
    };
    let contents = match fs::read_to_string(file.as_ref()) {
        Ok(contents) => contents,
        Err(err) => {
            writeln!(stdout, "source: error reading file: {err}")
                .map_err(ShellError::CommandFailed)?;
            return Ok(());
        }
    };
    let fd = stdout.into_raw_file_descriptor();
    // SAFETY: it is okay to use `fd` here because builtins should take owned file
    // IntoRawFileDescriptor's
    unsafe { shell.stdout(fd) };
    shell.run(&contents)?;

    Ok(())
}
