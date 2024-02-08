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

use command_fds::{CommandFdExt, FdMapping};
use filedescriptor::{AsRawFileDescriptor, FromRawFileDescriptor, IntoRawFileDescriptor};

/// A command pipeline generic over `T`.
///
/// Since this pipeline is meant to be used with closures (for support for built-ins and other
/// custom commands), they may take in a mutable reference to `T` for arbitrary data to work with
/// while executing.
pub struct Pipeline<'a, T> {
    inner: Command<'a, T>,
}

impl<'a, T> Pipeline<'a, T> {
    /// Create a new pipeline, starting from a command.
    pub fn new(cmd: impl Into<Command<'a, T>>) -> Self {
        Self { inner: cmd.into() }
    }

    /// Set up a pipe between the last commmand and the provided one.
    pub fn pipe(&mut self, cmd: impl Into<Command<'a, T>>) {
        let inner = self.take_inner();
        self.inner = Command::Pipe(Box::new(Pipe {
            lhs: inner,
            rhs: cmd.into(),
        }));
    }

    pub fn extend(&mut self, pipeline: Pipeline<'a, T>) {
        self.pipe(pipeline.inner);
    }

    pub fn append_args(&mut self, args: &[String]) {
        match &mut self.inner {
            Command::Basic(basic) => basic.args.extend_from_slice(args),
            Command::Pipe(pipe) => match &mut pipe.rhs {
                Command::Basic(basic) => basic.args.extend_from_slice(args),
                Command::Closure(c) => c.args.extend_from_slice(args),
                Command::Pipe(_) => unreachable!(),
            },
            Command::Closure(c) => c.args.extend_from_slice(args),
        }
    }

    /// Run the pipeline, returning a handle to the new process.
    pub fn spawn(self, data: &mut T, stdio: Stdio) -> io::Result<Handle<'a, T>> {
        self.inner.spawn(data, stdio)
    }

    fn take_inner(&mut self) -> Command<'a, T> {
        mem::replace(
            &mut self.inner,
            Command::Basic(BasicCommand {
                cmd: String::new(),
                merge_stderr: false,
                env: &[],
                args: vec![],
                pass_fds: vec![],
            }),
        )
    }
}

pub struct Closure<'a, T> {
    inner: PipelineClosure<T>,
    args: Vec<String>,
    env: &'a [(String, String)],
}

impl<'a, T> Closure<'a, T> {
    pub fn wrap<F>(f: F, args: Vec<String>, env: &'a [(String, String)]) -> Self
    where
        F: FnOnce(&mut T, Stdio, &[String], &[(String, String)]) -> i32 + 'static,
    {
        Self {
            inner: Box::new(f),
            args,
            env,
        }
    }
}

type PipelineClosure<T> = Box<dyn FnOnce(&mut T, Stdio, &[String], &[(String, String)]) -> i32>;

pub enum Command<'a, T> {
    Basic(BasicCommand<'a>),
    Closure(Closure<'a, T>),
    Pipe(Box<Pipe<'a, T>>),
}

impl<'a, T> From<BasicCommand<'a>> for Command<'a, T> {
    fn from(value: BasicCommand<'a>) -> Self {
        Self::Basic(value)
    }
}

impl<'a, T> From<Closure<'a, T>> for Command<'a, T> {
    fn from(value: Closure<'a, T>) -> Self {
        Self::Closure(value)
    }
}

impl<'a, T> Command<'a, T> {
    pub fn spawn(self, data: &mut T, stdio: Stdio) -> io::Result<Handle<'a, T>> {
        let handle = match self {
            Command::Basic(c) => HandleInner::Command(c.spawn(stdio)?),
            Command::Closure(c) => HandleInner::Closure(ClosureHandle {
                closure: Some(c.inner),
                args: c.args,
                env: c.env,
                stdio: Some(stdio),
                killed: false,
            }),
            Command::Pipe(p) => HandleInner::Pipe(p.spawn(data, stdio)?),
        };
        Ok(Handle(handle))
    }
}

#[derive(Debug)]
pub struct BasicCommand<'a> {
    cmd: String,
    args: Vec<String>,
    env: &'a [(String, String)],
    pass_fds: Vec<File>,
    merge_stderr: bool,
}

impl<'a> BasicCommand<'a> {
    pub fn new(cmd: &str) -> Self {
        Self {
            cmd: cmd.to_owned(),
            args: vec![],
            env: &[],
            pass_fds: vec![],
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

    pub fn env(mut self, env: &'a [(String, String)]) -> Self {
        self.env = env;
        self
    }

    pub fn pass_fds(mut self, fds: Vec<File>) -> Self {
        self.pass_fds = fds;
        self
    }

    pub fn merge_stderr(mut self, yes: bool) -> Self {
        self.merge_stderr = yes;
        self
    }

    pub fn spawn(self, stdio: Stdio) -> io::Result<CommandHandle> {
        let stderr = if self.merge_stderr {
            stdio.stdout.try_clone()?
        } else {
            stdio.stderr
        };

        let mut cmd = process::Command::new(&self.cmd);
        cmd.args(&self.args)
            .envs(self.env.iter().map(|(k, v)| (k, v)))
            .stdout(stdio.stdout)
            .stderr(stderr)
            .fd_mappings(
                self.pass_fds
                    .into_iter()
                    .map(|file| {
                        let raw = file.as_raw_file_descriptor();
                        FdMapping {
                            // SAFETY: `file` is a valid file descriptor, as it's already an open
                            // file
                            parent_fd: unsafe {
                                std::os::fd::OwnedFd::from_raw_file_descriptor(
                                    file.into_raw_file_descriptor(),
                                )
                            },
                            child_fd: raw,
                        }
                    })
                    .collect(),
            )
            .expect("BUG: fd mapping collision");
        if let Some(stdin) = stdio.stdin {
            cmd.stdin(stdin);
        }
        cmd.spawn().map(|child| CommandHandle { inner: child })
    }
}

pub struct Pipe<'a, T> {
    lhs: Command<'a, T>,
    rhs: Command<'a, T>,
}

impl<'a, T> Pipe<'a, T> {
    pub fn spawn(self, data: &mut T, stdio: Stdio) -> io::Result<PipeHandle<'a, T>> {
        let (reader, writer) = os_pipe::pipe()?;
        // SAFETY: `writer` is a valid raw file descriptor, and can be written to
        let writer = unsafe { File::from_raw_file_descriptor(writer.into_raw_file_descriptor()) };
        let mut lhs = self.lhs.spawn(
            data,
            Stdio::new(writer, stdio.stderr.try_clone()?, stdio.stdin),
        )?;
        // SAFETY: `reader` is a valid raw file descriptor, and can be read from
        let reader = unsafe { File::from_raw_file_descriptor(reader.into_raw_file_descriptor()) };
        let rhs_res = self
            .rhs
            .spawn(data, Stdio::new(stdio.stdout, stdio.stderr, Some(reader)));
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
    pub stdout: File,
    pub stderr: File,
    pub stdin: Option<File>,
}

impl Stdio {
    pub fn new(stdout: File, stderr: File, stdin: Option<File>) -> Self {
        Self {
            stdout,
            stderr,
            stdin,
        }
    }
}

pub struct Handle<'a, T>(HandleInner<'a, T>);

impl<'a, T> Handle<'a, T> {
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

enum HandleInner<'a, T> {
    Command(CommandHandle),
    Pipe(PipeHandle<'a, T>),
    Closure(ClosureHandle<'a, T>),
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

pub struct ClosureHandle<'a, T> {
    closure: Option<PipelineClosure<T>>,
    args: Vec<String>,
    env: &'a [(String, String)],
    stdio: Option<Stdio>,
    killed: bool,
}

impl<T> ClosureHandle<'_, T> {
    pub fn wait(&mut self, data: &mut T) -> io::Result<process::ExitStatus> {
        // If killed, we simply prevent the closure from executing
        if self.killed {
            return Ok(process::ExitStatus::from_raw(0));
        }
        // Avoiding a deadlock! We need `stdio` to be dropped in case it's a pipe end
        let stdio = self
            .stdio
            .take()
            .expect("cannot wait on closure more than once!");
        #[allow(clippy::unnecessary_cast)]
        let result = (self
            .closure
            .take()
            .expect("cannot wait on closure more than once!"))(
            data, stdio, &self.args, self.env
        ) as i32;
        Ok(process::ExitStatus::from_raw(result))
    }

    pub fn kill(&mut self) -> io::Result<()> {
        self.killed = true;
        Ok(())
    }
}

pub struct PipeHandle<'a, T> {
    lhs: Box<Handle<'a, T>>,
    rhs: Box<Handle<'a, T>>,
}

impl<T> PipeHandle<'_, T> {
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
    use filedescriptor::AsRawFileDescriptor;
    use std::io::{Read, Write};

    use super::*;

    fn inherit_stdio() -> Stdio {
        let stdout =
            unsafe { File::from_raw_file_descriptor(io::stdout().as_raw_file_descriptor()) };
        let stdout_clone = stdout.try_clone().unwrap();
        mem::forget(stdout);
        let stderr =
            unsafe { File::from_raw_file_descriptor(io::stderr().as_raw_file_descriptor()) };
        let stderr_clone = stderr.try_clone().unwrap();
        mem::forget(stderr);
        Stdio {
            stdout: stdout_clone,
            stderr: stderr_clone,
            stdin: None,
        }
    }

    #[test]
    fn basic_commands() {
        let cmd = BasicCommand::new("echo").args(Some("hello"));
        let mut pipeline = Pipeline::new(cmd);
        pipeline.pipe(BasicCommand::new("wc").args(Some("-l")));
        let mut handle = pipeline.spawn(&mut (), inherit_stdio()).unwrap();
        handle.wait(&mut ()).unwrap();
        assert!(false)
    }

    #[test]
    fn pipe_closure() {
        let cmd = BasicCommand::new("echo").args(Some("hello"));
        let mut pipeline = Pipeline::new(cmd);
        pipeline.pipe(Closure::wrap(
            |_: &mut (), mut stdio: Stdio, _: &[String], _: &[(String, String)]| {
                let mut stdin = String::new();
                stdio.stdin.unwrap().read_to_string(&mut stdin).unwrap();
                writeln!(stdio.stdout, "got: {stdin}").unwrap();
                0
            },
            vec![],
            &[],
        ));
        let mut handle = pipeline.spawn(&mut (), inherit_stdio()).unwrap();
        handle.wait(&mut ()).unwrap();
        assert!(false)
    }
}
