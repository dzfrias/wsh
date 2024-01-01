use std::{env, ffi::OsStr, fs, io, path::PathBuf};

use filedescriptor::IntoRawFileDescriptor;

use crate::{Interpreter, Lexer, Parser, RuntimeError, RuntimeResult};

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
        shell: &mut Interpreter,
        args: I,
        stdout: impl io::Write + IntoRawFileDescriptor,
    ) -> RuntimeResult<()>
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

fn cd<I, S>(args: I, mut out: impl io::Write + IntoRawFileDescriptor) -> RuntimeResult<()>
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

fn source<I, S>(
    shell: &mut Interpreter,
    args: I,
    mut stdout: impl io::Write + IntoRawFileDescriptor,
) -> RuntimeResult<()>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let args = args.into_iter().collect::<Vec<_>>();
    // TOOD: support passing args to scripts
    let Some((file, _args)) = args.split_first() else {
        writeln!(stdout, "source: no file provided").map_err(RuntimeError::CommandFailed)?;
        return Ok(());
    };
    let fd = stdout.into_raw_file_descriptor();
    let contents = fs::read_to_string(file.as_ref()).expect("TODO: error");
    // TODO: this process should be done in the shell. Errors should be unified under a ShellError,
    // so the exported API can be simplified.
    let tokens = Lexer::new(&contents).lex();
    let ast = Parser::new(&tokens).parse().expect("TODO: error");
    shell.stdout(fd);
    shell.dup_stdout(false);
    shell.run(ast).expect("TODO: error");

    Ok(())
}
