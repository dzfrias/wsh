mod allow;
mod cd;
mod load;
mod source;
mod unload;
mod which;

use std::{collections::HashMap, io};

use filedescriptor::{AsRawFileDescriptor, IntoRawFileDescriptor};

use crate::{Shell, ShellResult};
use allow::allow;
use cd::cd;
use load::load;
use source::source;
use unload::unload;
use which::which;

#[derive(Debug)]
struct Args {
    positional: Vec<String>,
    argv: HashMap<String, Vec<String>>,
}

impl Args {
    pub fn is_empty(&self) -> bool {
        self.positional.is_empty() && self.argv.is_empty()
    }

    pub fn get_argv1(&self, name: impl AsRef<str>) -> Option<&str> {
        self.argv
            .get(name.as_ref())
            .and_then(|v| v.first())
            .map(|s| s.as_str())
    }

    pub fn get_argv(
        &self,
        name: impl AsRef<str>,
        or: Option<impl AsRef<str>>,
    ) -> Option<&[String]> {
        self.argv
            .get(name.as_ref())
            .or_else(|| or.and_then(|or| self.argv.get(or.as_ref())))
            .map(|v| v.as_slice())
    }
}

#[derive(Debug)]
pub enum Builtin {
    Cd,
    Source,
    Load,
    Unload,
    Which,
    Allow,
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
            _ => None,
        }
    }

    pub fn run<I, S>(
        &self,
        shell: &mut Shell,
        args: I,
        mut stdout: impl io::Write + AsRawFileDescriptor,
        mut stderr: impl io::Write + AsRawFileDescriptor,
        stdin: Option<impl io::Read + IntoRawFileDescriptor>,
        env: &[(String, String)],
    ) -> ShellResult<i32>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        let (positional, argv) = argmap::parse(args.into_iter().map(|s| s.as_ref().to_owned()));
        let args = Args { positional, argv };
        let result = match self {
            Self::Cd => cd(shell, args, &mut stdout, stdin),
            Self::Source => source(shell, args, &mut stdout, &mut stderr, stdin, env),
            Self::Load => load(shell, args, &mut stdout, stdin),
            Self::Unload => unload(shell, args, &mut stdout, stdin),
            Self::Which => which(shell, args, &mut stdout, stdin),
            Self::Allow => allow(shell, args, &mut stdout, stdin),
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
            Self::Allow => "allow",
        }
    }
}
