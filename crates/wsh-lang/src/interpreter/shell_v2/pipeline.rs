//! A wsh-purposed command running utility. The design of this module is heavily inspired by the
//! great [duct](https://docs.rs/duct/0.13.7/duct/index.html) crate. Many subtle nuances of process
//! management are extremely well-documented and clear, making modules like this possible. We
//! don't use the duct itself to support things like built-in functions, that aren't actually
//! commands but are stil constructs in the shell.

#[cfg(unix)]
use std::os::unix::process::ExitStatusExt;
#[cfg(windows)]
use std::os::windows::process::ExitStatusExt;
use std::{fs::File, io, mem, process};

use filedescriptor::{AsRawFileDescriptor, FromRawFileDescriptor, IntoRawFileDescriptor};

/// A command pipeline generic over `T`.
///
/// Since this pipeline is meant to be used with closures (for support for built-ins and other
/// custom commands), they may take in a mutable reference to `T` for arbitrary data to work with
/// while executing.
pub struct Pipeline<T> {
    inner: Command<T>,
}

impl<T> Pipeline<T> {
    pub fn new(cmd: impl Into<Command<T>>) -> Self {
        Self { inner: cmd.into() }
    }

    pub fn pipe(&mut self, cmd: impl Into<Command<T>>) {
        let inner = self.take_inner();
        self.inner = Command::Pipe(Box::new(Pipe {
            lhs: inner,
            rhs: cmd.into(),
        }));
    }

    pub fn spawn(&mut self, data: &mut T, stdio: Stdio) -> io::Result<Handle<T>> {
        self.inner.spawn(data, stdio)
    }

    fn take_inner(&mut self) -> Command<T> {
        mem::replace(
            &mut self.inner,
            Command::Basic(BasicCommand {
                cmd: String::new(),
                merge_stderr: false,
                args: vec![],
            }),
        )
    }
}

pub type PipelineClosure<T> = Box<dyn FnOnce(&mut T, Stdio) -> i32>;

pub enum Command<T> {
    Basic(BasicCommand),
    Closure(Option<PipelineClosure<T>>),
    Pipe(Box<Pipe<T>>),
}

impl<T> From<BasicCommand> for Command<T> {
    fn from(value: BasicCommand) -> Self {
        Self::Basic(value)
    }
}

impl<T, F> From<F> for Command<T>
where
    F: FnOnce(&mut T, Stdio) -> i32 + 'static,
{
    fn from(value: F) -> Self {
        Self::Closure(Some(Box::new(value)))
    }
}

impl<T> Command<T> {
    pub fn spawn(&mut self, data: &mut T, stdio: Stdio) -> io::Result<Handle<T>> {
        let handle = match self {
            Command::Basic(c) => HandleInner::Command(c.spawn(stdio)?),
            Command::Closure(c) => HandleInner::Closure(ClosureHandle {
                closure: c.take(),
                stdio,
            }),
            Command::Pipe(p) => HandleInner::Pipe(p.spawn(data, stdio)?),
        };
        Ok(Handle(handle))
    }
}

#[derive(Debug)]
pub struct BasicCommand {
    cmd: String,
    args: Vec<String>,
    merge_stderr: bool,
}

impl BasicCommand {
    pub fn new(cmd: &str) -> Self {
        Self {
            cmd: cmd.to_owned(),
            args: vec![],
            merge_stderr: false,
        }
    }

    pub fn args<I, S>(mut self, args: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        self.args = args.into_iter().map(|s| s.as_ref().to_owned()).collect();
        self
    }

    pub fn merge_stderr(mut self, yes: bool) -> Self {
        self.merge_stderr = yes;
        self
    }

    pub fn spawn(&mut self, stdio: Stdio) -> io::Result<CommandHandle> {
        let stderr = if self.merge_stderr {
            stdio.stdout.try_clone()?
        } else {
            stdio.stderr
        };

        process::Command::new(&self.cmd)
            .args(&self.args)
            .stdout(stdio.stdout)
            .stderr(stderr)
            .stdin(stdio.stdin)
            .spawn()
            .map(|child| CommandHandle { inner: child })
    }
}

pub struct Pipe<T> {
    lhs: Command<T>,
    rhs: Command<T>,
}

impl<T> Pipe<T> {
    pub fn spawn(&mut self, data: &mut T, stdio: Stdio) -> io::Result<PipeHandle<T>> {
        let (reader, writer) = os_pipe::pipe().unwrap();
        let writer = unsafe { File::from_raw_file_descriptor(writer.into_raw_file_descriptor()) };
        let mut lhs = self.lhs.spawn(
            data,
            Stdio::new(writer, stdio.stderr.try_clone()?, stdio.stdin),
        )?;
        let reader = unsafe { File::from_raw_file_descriptor(reader.into_raw_file_descriptor()) };
        let rhs_res = self
            .rhs
            .spawn(data, Stdio::new(stdio.stdout, stdio.stderr, reader));
        match rhs_res {
            Ok(rhs) => Ok(PipeHandle {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }),
            Err(err) => {
                lhs.kill()?;
                lhs.wait(data)?;
                Err(err)
            }
        }
    }
}

#[derive(Debug)]
pub struct Stdio {
    stdout: File,
    stderr: File,
    stdin: File,
}

impl Stdio {
    pub fn new(stdout: File, stderr: File, stdin: File) -> Self {
        Self {
            stdout,
            stderr,
            stdin,
        }
    }

    pub fn try_clone(&self) -> io::Result<Self> {
        Ok(Self {
            stdout: self.stdout.try_clone()?,
            stderr: self.stderr.try_clone()?,
            stdin: self.stdin.try_clone()?,
        })
    }

    pub fn inherit() -> io::Result<Self> {
        unsafe {
            let stdout = File::from_raw_file_descriptor(io::stdout().as_raw_file_descriptor());
            let stdout_clone = stdout.try_clone()?;
            let stderr = File::from_raw_file_descriptor(io::stderr().as_raw_file_descriptor());
            let stderr_clone = stderr.try_clone()?;
            let stdin = File::from_raw_file_descriptor(io::stdin().as_raw_file_descriptor());
            let stdin_clone = stdin.try_clone()?;
            // We `forget` here to avoid closing the file descriptors
            mem::forget(stdout);
            mem::forget(stderr);
            mem::forget(stdin);
            Ok(Self {
                stdout: stdout_clone,
                stderr: stderr_clone,
                stdin: stdin_clone,
            })
        }
    }
}

pub struct Handle<T>(HandleInner<T>);

impl<T> Handle<T> {
    pub fn wait(&mut self, data: &mut T) -> io::Result<process::ExitStatus> {
        match &mut self.0 {
            HandleInner::Command(c) => c.wait(),
            HandleInner::Closure(c) => c.wait(data),
            HandleInner::Pipe(p) => p.wait(data),
        }
    }

    pub fn kill(&mut self) -> io::Result<()> {
        match &mut self.0 {
            HandleInner::Command(c) => c.kill(),
            HandleInner::Closure(c) => c.kill(),
            HandleInner::Pipe(p) => p.kill(),
        }
    }
}

pub enum HandleInner<T> {
    Command(CommandHandle),
    Pipe(PipeHandle<T>),
    Closure(ClosureHandle<T>),
}

#[derive(Debug)]
pub struct CommandHandle {
    inner: process::Child,
}

impl CommandHandle {
    pub fn wait(&mut self) -> io::Result<process::ExitStatus> {
        self.inner.wait()
    }

    pub fn kill(&mut self) -> io::Result<()> {
        self.inner.kill()
    }
}

pub struct ClosureHandle<T> {
    closure: Option<PipelineClosure<T>>,
    stdio: Stdio,
}

impl<T> ClosureHandle<T> {
    pub fn wait(&mut self, data: &mut T) -> io::Result<process::ExitStatus> {
        #[allow(clippy::unnecessary_cast)]
        let result = (self.closure.take().unwrap())(data, self.stdio.try_clone()?) as i32;
        Ok(process::ExitStatus::from_raw(result))
    }

    pub fn kill(&mut self) -> io::Result<()> {
        // TODO: should kill do something?
        Ok(())
    }
}

pub struct PipeHandle<T> {
    lhs: Box<Handle<T>>,
    rhs: Box<Handle<T>>,
}

impl<T> PipeHandle<T> {
    pub fn wait(&mut self, data: &mut T) -> io::Result<process::ExitStatus> {
        let lhs_res = self.lhs.wait(data);
        let rhs_res = self.rhs.wait(data);
        lhs_res?;
        let rhs = rhs_res?;
        // TODO: pipe status precedence: https://docs.rs/duct/latest/src/duct/lib.rs.html#1338
        Ok(rhs)
    }

    pub fn kill(&mut self) -> io::Result<()> {
        let lhs_res = self.lhs.kill();
        let rhs_res = self.rhs.kill();
        lhs_res.and(rhs_res)
    }
}

#[cfg(test)]
mod tests {
    use std::io::{Read, Write};

    use super::*;

    #[test]
    fn basic_commands() {
        let cmd = BasicCommand::new("echo").args(Some("hello"));
        let mut pipeline = Pipeline::new(cmd);
        pipeline.pipe(BasicCommand::new("wc").args(Some("-l")));
        let mut handle = pipeline.spawn(&mut (), Stdio::inherit().unwrap()).unwrap();
        handle.wait(&mut ()).unwrap();
        assert!(false)
    }

    #[test]
    fn pipe_closure() {
        let cmd = BasicCommand::new("echo").args(Some("hello"));
        let mut pipeline = Pipeline::new(cmd);
        pipeline.pipe(|_: &mut (), mut stdio: Stdio| {
            let mut stdin = String::new();
            stdio.stdin.read_to_string(&mut stdin).unwrap();
            writeln!(stdio.stdout, "got: {stdin}").unwrap();
            0
        });
        let mut handle = pipeline.spawn(&mut (), Stdio::inherit().unwrap()).unwrap();
        handle.wait(&mut ()).unwrap();
        assert!(false)
    }
}
