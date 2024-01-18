mod allow;
mod cd;
mod load;
mod memfs;
mod source;
mod unload;
mod which;

use std::{fs::File, io::Write};

use crate::{Shell, ShellResult};
use allow::allow;
use cd::cd;
use load::load;
use memfs::memfs;
use source::source;
use unload::unload;
use which::which;

#[derive(Debug)]
pub enum Builtin {
    Cd,
    Source,
    Load,
    Unload,
    Which,
    Allow,
    Memfs,
}

impl Builtin {
    pub fn from_name(name: impl AsRef<str>) -> Option<Self> {
        match name.as_ref() {
            "cd" => Some(Self::Cd),
            "source" => Some(Self::Source),
            "load" => Some(Self::Load),
            "unload" => Some(Self::Unload),
            "which" => Some(Self::Which),
            "allow" => Some(Self::Allow),
            "memfs" => Some(Self::Memfs),
            _ => None,
        }
    }

    pub fn run(
        &self,
        shell: &mut Shell,
        mut builtin_args: Vec<String>,
        mut io_streams: IoStreams,
        env: &[(String, String)],
    ) -> ShellResult<i32> {
        let mut args = vec![self.name().to_owned()];
        args.append(&mut builtin_args);

        let result = match self {
            Self::Cd => cd(shell, args, &mut io_streams),
            Self::Source => source(shell, args, &mut io_streams, env),
            Self::Load => load(shell, args, &mut io_streams),
            Self::Unload => unload(shell, args, &mut io_streams),
            Self::Which => which(shell, args, &mut io_streams),
            Self::Allow => allow(shell, args, &mut io_streams),
            Self::Memfs => memfs(shell, args, &mut io_streams),
        };
        if let Err(err) = result {
            writeln!(io_streams.stderr, "{}: {err:#}", self.name())
                .expect("error writing to stderr");
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
            Self::Allow => "allow",
            Self::Memfs => "memfs",
        }
    }
}

#[derive(Debug)]
pub struct IoStreams {
    pub stdout: File,
    pub stderr: File,
    pub stdin: Option<File>,
    pub to_tty: bool,
}
