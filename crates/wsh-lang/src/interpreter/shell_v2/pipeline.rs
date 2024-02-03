//! A wsh-purposed command running utility. The design of this module is heavily inspired by the
//! great [duct](https://docs.rs/duct/0.13.7/duct/index.html) crate. Many subtle nuances of process
//! management are extremely well-documented and clear, making modules like this possible. We
//! don't use the duct itself to support things like built-in functions, that aren't actually
//! commands but are stil constructs in the shell.

use std::{io, mem, process};

#[derive(Debug)]
pub struct Pipeline {
    inner: CommandInner,
}

impl Pipeline {
    pub fn new(cmd: Command) -> Self {
        Self {
            inner: CommandInner::Basic(cmd),
        }
    }

    pub fn pipe(&mut self, cmd: Command) {
        let inner = mem::replace(
            &mut self.inner,
            CommandInner::Basic(Command {
                cmd: String::new(),
                args: vec![],
            }),
        );
        self.inner = CommandInner::Pipe(Box::new(Pipe {
            lhs: inner,
            rhs: CommandInner::Basic(cmd),
        }))
    }

    pub fn spawn(&mut self, stdio: Stdio) -> io::Result<Handle> {
        self.inner.spawn(stdio)
    }
}

#[derive(Debug)]
enum CommandInner {
    Basic(Command),
    Pipe(Box<Pipe>),
}

impl CommandInner {
    pub fn spawn(&mut self, stdio: Stdio) -> io::Result<Handle> {
        let handle = match self {
            CommandInner::Basic(c) => HandleInner::Command(c.spawn(stdio)?),
            CommandInner::Pipe(p) => HandleInner::Pipe(p.spawn(stdio)?),
        };
        Ok(Handle(handle))
    }
}

#[derive(Debug)]
pub struct Command {
    cmd: String,
    args: Vec<String>,
}

impl Command {
    pub fn new(cmd: &str) -> Self {
        Self {
            cmd: cmd.to_owned(),
            args: vec![],
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

    pub fn spawn(&mut self, stdio: Stdio) -> io::Result<CommandHandle> {
        process::Command::new(&self.cmd)
            .args(&self.args)
            .stdout(stdio.stdout)
            .stderr(stdio.stderr)
            .stdin(stdio.stdin)
            .spawn()
            .map(|child| CommandHandle { inner: child })
    }
}

#[derive(Debug)]
struct Pipe {
    lhs: CommandInner,
    rhs: CommandInner,
}

impl Pipe {
    pub fn spawn(&mut self, stdio: Stdio) -> io::Result<PipeHandle> {
        let (reader, writer) = os_pipe::pipe().unwrap();
        // TODO: handle stderr
        let mut lhs = self
            .lhs
            .spawn(Stdio::new(writer, process::Stdio::inherit(), stdio.stdin))?;
        let rhs_res = self
            .rhs
            .spawn(Stdio::new(stdio.stdout, stdio.stderr, reader));
        match rhs_res {
            Ok(rhs) => Ok(PipeHandle {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }),
            Err(err) => {
                lhs.kill()?;
                lhs.wait()?;
                Err(err)
            }
        }
    }
}

#[derive(Debug)]
pub struct Stdio {
    stdout: process::Stdio,
    stderr: process::Stdio,
    stdin: process::Stdio,
}

impl Stdio {
    pub fn new(
        stdout: impl Into<process::Stdio>,
        stderr: impl Into<process::Stdio>,
        stdin: impl Into<process::Stdio>,
    ) -> Self {
        Self {
            stdout: stdout.into(),
            stderr: stderr.into(),
            stdin: stdin.into(),
        }
    }
}

impl Default for Stdio {
    fn default() -> Self {
        Self {
            stdout: process::Stdio::inherit(),
            stderr: process::Stdio::inherit(),
            stdin: process::Stdio::inherit(),
        }
    }
}

pub struct Handle(HandleInner);

impl Handle {
    pub fn wait(&mut self) -> io::Result<process::ExitStatus> {
        match &mut self.0 {
            HandleInner::Command(c) => c.wait(),
            HandleInner::Pipe(p) => p.wait(),
        }
    }

    pub fn kill(&mut self) -> io::Result<()> {
        match &mut self.0 {
            HandleInner::Command(c) => c.kill(),
            HandleInner::Pipe(p) => p.kill(),
        }
    }
}

pub enum HandleInner {
    Command(CommandHandle),
    Pipe(PipeHandle),
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

pub struct PipeHandle {
    lhs: Box<Handle>,
    rhs: Box<Handle>,
}

impl PipeHandle {
    pub fn wait(&mut self) -> io::Result<process::ExitStatus> {
        let lhs_res = self.lhs.wait();
        let rhs_res = self.rhs.wait();
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
    use super::*;

    #[test]
    fn basic_commands() {
        let cmd = Command::new("echo").args(Some("hello"));
        let mut pipeline = Pipeline::new(cmd);
        pipeline.pipe(Command::new("wc").args(Some("-l")));
        let mut handle = pipeline.spawn(Stdio::default()).unwrap();
        handle.wait().unwrap();
        assert!(false)
    }
}
