mod cd;
mod load;
mod source;
mod unload;
mod which;

use std::{ffi::OsStr, io};

use filedescriptor::IntoRawFileDescriptor;

use self::which::which;
use crate::{Shell, ShellResult};
use cd::cd;
use load::load;
use source::source;
use unload::unload;

#[derive(Debug)]
pub enum Builtin {
    Cd,
    Source,
    Load,
    Unload,
    Which,
}

impl Builtin {
    pub fn from_name(name: impl AsRef<OsStr>) -> Option<Self> {
        match name.as_ref().to_str()? {
            "cd" => Some(Self::Cd),
            "source" => Some(Self::Source),
            "load" => Some(Self::Load),
            "unload" => Some(Self::Unload),
            "which" => Some(Self::Which),
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
        S: AsRef<str>,
    {
        match self {
            Self::Cd => cd(shell, args, stdout),
            Self::Source => source(shell, args, stdout),
            Self::Load => load(shell, args, stdout),
            Self::Unload => unload(shell, args, stdout),
            Self::Which => which(shell, args, stdout),
        }
    }
}
