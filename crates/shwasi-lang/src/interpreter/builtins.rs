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

    pub fn run<I, S>(&self, args: I) -> RuntimeResult<()>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        match self {
            Self::Cd => cd(args),
        }
    }
}

fn cd<I, S>(args: I) -> RuntimeResult<()>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let path = args
        .into_iter()
        .next()
        .map(|arg| PathBuf::from(arg.as_ref()))
        .map_or_else(dirs::home_dir, Some)
        .ok_or_else(|| {
            RuntimeError::CommandFailed(io::Error::new(
                io::ErrorKind::Other,
                "home directory not found",
            ))
        })?;

    env::set_current_dir(path).map_err(RuntimeError::CommandFailed)?;

    Ok(())
}
