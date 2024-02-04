mod cd;
mod source;
mod which;

use std::{io::Write, iter};

use crate::shell_v2::{pipeline::Stdio, Shell};

macro_rules! force_writeln {
    ($f:expr, $($arg:tt)*) => {
        writeln!($f, $($arg)*).expect("error writing to stdio stream!")
    };
}
use force_writeln;

#[derive(Debug, Clone, Copy)]
pub enum Builtin {
    Cd,
    Which,
    Source,
}

impl Builtin {
    pub fn from_name(name: &str) -> Option<Self> {
        Some(match name {
            "cd" => Self::Cd,
            "which" => Self::Which,
            "source" => Self::Source,
            _ => return None,
        })
    }

    pub fn name(self) -> &'static str {
        match self {
            Builtin::Cd => "cd",
            Builtin::Which => "which",
            Builtin::Source => "source",
        }
    }

    pub fn run(self, shell: &mut Shell, mut stdio: Stdio, args: &[String]) -> i32 {
        let mut stderr = match stdio.stderr.try_clone() {
            Ok(stderr) => stderr,
            Err(err) => {
                force_writeln!(stdio.stderr, "wsh: could not clone stderr: {err:#}");
                return 1;
            }
        };
        let args = iter::once(self.name()).chain(args.iter().map(|s| s.as_str()));
        if let Err(err) = match self {
            Builtin::Cd => cd::cd(shell, stdio, args),
            Builtin::Which => which::which(shell, stdio, args),
            Builtin::Source => source::source(shell, stdio, args),
        } {
            force_writeln!(stderr, "{}: {err:#}", self.name());
            1
        } else {
            0
        }
    }
}
