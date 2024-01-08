mod cd;
mod load;
mod source;
mod unload;
mod which;

use std::io;

use filedescriptor::AsRawFileDescriptor;

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
    pub fn from_name(name: impl AsRef<str>) -> Option<Self> {
        match name.as_ref() {
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
        mut stdout: impl io::Write + AsRawFileDescriptor,
        mut stderr: impl io::Write + AsRawFileDescriptor,
    ) -> ShellResult<i32>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        let result = match self {
            Self::Cd => cd(shell, args, &mut stdout),
            Self::Source => source(shell, args, &mut stdout, &mut stderr),
            Self::Load => load(shell, args, &mut stdout),
            Self::Unload => unload(shell, args, &mut stdout),
            Self::Which => which(shell, args, &mut stdout),
        };
        if let Err(err) = result {
            writeln!(stderr, "{}: {err:#}", self.name()).expect("error writing to stderr");
        };

        Ok(0)
    }

    pub fn name(&self) -> &'static str {
        match self {
            Self::Cd => "cd",
            Self::Source => "source",
            Self::Load => "load",
            Self::Unload => "unload",
            Self::Which => "which",
        }
    }
}
