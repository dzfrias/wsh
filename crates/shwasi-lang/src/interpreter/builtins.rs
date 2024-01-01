use std::{env, ffi::OsStr, io, path::PathBuf};

use crate::{RuntimeError, RuntimeResult};

#[derive(Debug)]
pub enum Builtin {
    Cd,
}

impl Builtin {
    pub fn from_name(name: impl AsRef<OsStr>) -> Option<Self> {
        match name.as_ref().to_str() {
            Some("cd") => Some(Self::Cd),
            _ => None,
        }
    }

    pub fn run<I, S>(&self, args: I, stdout: impl io::Write) -> RuntimeResult<()>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        match self {
            Self::Cd => cd(args, stdout),
        }
    }
}

fn cd<I, S>(args: I, mut out: impl io::Write) -> RuntimeResult<()>
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
        writeln!(out, "cd: no home directory").map_err(RuntimeError::CommandFailed)?;
        return Ok(());
    };

    if let Err(err) = env::set_current_dir(&path) {
        writeln!(out, "cd: error moving to {}: {err}", path.display())
            .map_err(RuntimeError::CommandFailed)?;
    }

    Ok(())
}
