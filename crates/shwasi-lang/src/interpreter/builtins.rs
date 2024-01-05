use std::{env, ffi::OsStr, fs, io, path::PathBuf};

use filedescriptor::{FileDescriptor, FromRawFileDescriptor, IntoRawFileDescriptor};
use shwasi_engine::Instance;

use crate::{Shell, ShellResult};

#[derive(Debug)]
pub enum Builtin {
    Cd,
    Source,
    Load,
    Unload,
}

impl Builtin {
    pub fn from_name(name: impl AsRef<OsStr>) -> Option<Self> {
        match name.as_ref().to_str()? {
            "cd" => Some(Self::Cd),
            "source" => Some(Self::Source),
            "load" => Some(Self::Load),
            "unload" => Some(Self::Unload),
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
        S: AsRef<OsStr>,
    {
        match self {
            Self::Cd => cd(args, stdout),
            Self::Source => source(shell, args, stdout),
            Self::Load => load(shell, args, stdout),
            Self::Unload => unload(shell, args, stdout),
        }
    }
}

fn cd<I, S>(args: I, mut out: impl io::Write + IntoRawFileDescriptor) -> ShellResult<i32>
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
        writeln!(out, "cd: no home directory").expect("error writing to stdout");
        return Ok(1);
    };

    if let Err(err) = env::set_current_dir(&path) {
        writeln!(out, "cd: error moving to {}: {err}", path.display())
            .expect("error writing to stdout");
        return Ok(1);
    }

    Ok(0)
}

fn source<I, S>(
    shell: &mut Shell,
    args: I,
    mut stdout: impl io::Write + IntoRawFileDescriptor,
) -> ShellResult<i32>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let args = args.into_iter().collect::<Vec<_>>();
    // TOOD: support passing args to scripts
    let Some((file, _args)) = args.split_first() else {
        writeln!(stdout, "source: no file provided").expect("error writing to stdout");
        return Ok(1);
    };
    let contents = match fs::read_to_string(file.as_ref()) {
        Ok(contents) => contents,
        Err(err) => {
            writeln!(stdout, "source: error reading file: {err}").expect("error writing to stdout");
            return Ok(1);
        }
    };
    let fd = stdout.into_raw_file_descriptor();
    // SAFETY: `fd` is a valid file descriptor on account of it being from an
    // `IntoRawFileDescriptor`. We later close it, so we don't leak it.
    unsafe { shell.stdout(fd) };
    let _ = shell.run(&contents, &file.as_ref().to_string_lossy());
    // We need to be careful that the file descriptor is closed, so we don't leak it. This can
    // cause hangs in the shell if used `source` is used with a pipe. This is because the shell
    // will never close the `stdout` it's given, so we must manually close it here.
    let _ = unsafe { FileDescriptor::from_raw_file_descriptor(fd) };

    Ok(0)
}

fn load<I, S>(
    shell: &mut Shell,
    args: I,
    mut stdout: impl io::Write + IntoRawFileDescriptor,
) -> ShellResult<i32>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let args = args.into_iter().collect::<Vec<_>>();
    let Some((file, _)) = args.split_first() else {
        writeln!(stdout, "load: no file provided").expect("error writing to stdout");
        return Ok(1);
    };

    let contents = match fs::read(file.as_ref()) {
        Ok(contents) => contents,
        Err(err) => {
            writeln!(stdout, "load: error reading input file: {err}")
                .expect("error writing to stdout");
            return Ok(1);
        }
    };
    let module = match shwasi_parser::Parser::new(&contents).read_module() {
        Ok(contents) => contents,
        Err(err) => {
            writeln!(stdout, "load: bad wasm module: {err}").expect("error writing to stdout");
            return Ok(1);
        }
    };
    let instance = match Instance::instantiate(shell.env.store_mut(), module) {
        Ok(instance) => instance,
        Err(err) => {
            writeln!(stdout, "load: error instantiating wasm module: {err}")
                .expect("error writing to stdout");
            return Ok(1);
        }
    };
    shell.env.register_module(instance);

    Ok(0)
}

fn unload<I, S>(
    shell: &mut Shell,
    args: I,
    mut stdout: impl io::Write + IntoRawFileDescriptor,
) -> ShellResult<i32>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    if args.into_iter().next().is_some() {
        writeln!(stdout, "unload: expected no arguments").expect("error writing to stdout");
        return Ok(1);
    }

    let unloaded = shell.env.unload_modules();
    writeln!(stdout, "unloaded {unloaded} modules").expect("error writing to stdout");
    Ok(0)
}
