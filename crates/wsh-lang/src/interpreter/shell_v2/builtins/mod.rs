mod allow;
mod cd;
mod error;
mod load;
mod memfs;
mod source;
mod unload;
mod which;

use std::iter;

use crate::shell_v2::{pipeline::Stdio, Shell};

macro_rules! force_writeln {
    ($f:expr, $($arg:tt)*) => {
        writeln!($f, $($arg)*).expect("error writing to stdio stream!")
    };
    ($f:expr) => {
        writeln!($f).expect("error writing to stdio stream!")
    };
}
macro_rules! force_write {
    ($f:expr, $($arg:tt)*) => {
        write!($f, $($arg)*).expect("error writing to stdio stream!")
    };
}
use anyhow::Context;
use force_write;
use force_writeln;

#[derive(Debug, Clone, Copy)]
pub enum Builtin {
    Cd,
    Which,
    Source,
    Allow,
    Load,
    Unload,
    MemFs,
    Error,
}

impl Builtin {
    pub fn from_name(name: &str) -> Option<Self> {
        Some(match name {
            "cd" => Self::Cd,
            "which" => Self::Which,
            "source" => Self::Source,
            "allow" => Self::Allow,
            "load" => Self::Load,
            "unload" => Self::Unload,
            "memfs" => Self::MemFs,
            "_error" => Self::Error,
            _ => return None,
        })
    }

    pub fn name(self) -> &'static str {
        match self {
            Builtin::Cd => "cd",
            Builtin::Which => "which",
            Builtin::Source => "source",
            Builtin::Allow => "allow",
            Builtin::Load => "load",
            Builtin::Unload => "unload",
            Builtin::MemFs => "memfs",
            Builtin::Error => "_error",
        }
    }

    pub fn run(
        self,
        shell: &mut Shell,
        stdio: Stdio,
        args: &[String],
        env: &[(String, String)],
    ) -> anyhow::Result<()> {
        let args = iter::once(self.name()).chain(args.iter().map(|s| s.as_str()));
        match self {
            Builtin::Cd => cd::cd(shell, stdio, args, env),
            Builtin::Which => which::which(shell, stdio, args, env),
            Builtin::Source => source::source(shell, stdio, args, env),
            Builtin::Allow => allow::allow(shell, stdio, args, env),
            Builtin::Load => load::load(shell, stdio, args, env),
            Builtin::Unload => unload::unload(shell, stdio, args, env),
            Builtin::MemFs => memfs::memfs(shell, stdio, args, env),
            Builtin::Error => error::error(shell, stdio, args, env),
        }
        .context(self.name())
    }
}
