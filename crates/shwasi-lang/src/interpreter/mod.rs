mod builtins;
mod env;
mod error;
mod memfs;
mod value;

use crate::{
    ast::{
        AliasAssign, Assign, Def, DefArg, EnvSet, Export, If, InfixOp, Pipeline, PipelineEnd,
        PipelineEndKind, PrefixOp, While,
    },
    interpreter::{
        builtins::{Builtin, IoStreams},
        env::Env,
    },
    parser::ast::{Command, Expr, InfixExpr, PrefixExpr, Stmt},
    Ident, Lexer, Parser,
};
pub use error::*;
use filedescriptor::{
    AsRawFileDescriptor, FileDescriptor, FromRawFileDescriptor, IntoRawFileDescriptor,
    RawFileDescriptor,
};
use shwasi_engine::{HostFunc, Instance, Store};
use shwasi_parser::ValType;
use smol_str::SmolStr;
#[cfg(unix)]
use std::os::unix::process::ExitStatusExt;
#[cfg(windows)]
use std::os::windows::process::ExitStatusExt;
use std::{
    borrow::Cow,
    fs::{File, OpenOptions},
    io::{self, Read, Write},
    mem::ManuallyDrop,
    process,
};
pub use value::*;

/// Macro to propagate control flow results during execution.
macro_rules! control_flow {
    ($expr:expr) => {
        match $expr {
            ControlFlow::Break => return Ok(ControlFlow::Break),
            ControlFlow::Continue => return Ok(ControlFlow::Continue),
            ControlFlow::Return => return Ok(ControlFlow::Return),
            ControlFlow::Value(_) => {}
        }
    };
}

/// The executor of shwasi programs.
pub struct Shell {
    env: Env,
    stdout: RawFileDescriptor,
    stderr: RawFileDescriptor,
    last_status: i32,
}

// TODO: right now, there's quite a bit of unsafe code in the implemenation. This is mostly a
// result of raw file descriptors being used pretty much everywhere. Perhaps better usages of
// OwnedFd and BorrowedFd would reduce the risk of a fd leak or use-after-close bug.
impl Shell {
    pub fn new() -> Self {
        Self {
            env: Env::new(),
            stdout: io::stdout().as_raw_file_descriptor(),
            stderr: io::stderr().as_raw_file_descriptor(),
            last_status: 0,
        }
    }

    /// Runs the shell on the given source code.
    #[allow(clippy::result_unit_err)]
    pub fn run(&mut self, src: &str, name: &str) -> ShellResult<Option<Value>> {
        let buf = Lexer::new(src).lex();
        let mut program = match Parser::new(&buf).parse() {
            Ok(program) => program,
            Err(err) => {
                // SAFETY: `stderr` must be a valid file descriptor
                let mut stderr = unsafe { OpenFileDescriptor::new(self.stderr) };
                err.write_to(src, name, &mut stderr);
                self.set_status(1);
                return Err(());
            }
        };
        self.env.add_funcs(std::mem::take(&mut program.defs));

        let mut result = Value::Null;
        for stmt in program {
            result = match self.eval_stmt(&stmt) {
                Ok(ControlFlow::Value(result)) => result,
                Ok(_) => panic!("BUG: should not have top-level control flow execution!"),
                Err(_) => continue,
            }
        }

        self.env.mem_fs.gc_step();

        // Reset stdout to the original stdout. This is necessary for operations like `source`,
        // where the user may have redirected stdout somewhere else temporarily.
        unsafe { self.stdout(io::stdout().as_raw_file_descriptor()) };
        unsafe { self.stderr(io::stderr().as_raw_file_descriptor()) };

        Ok((!result.is_null()).then_some(result))
    }

    /// Sets the stdout file descriptor to `stdout`.
    ///
    /// # Safety
    /// The provided file descriptor must be valid. Additionally, the shell **does not** close the
    /// provided fd, so that **must** be handled after the fd has been fully used up by the calling
    /// code.
    pub(self) unsafe fn stdout(&mut self, stdout: RawFileDescriptor) {
        self.stdout = stdout;
        self.env.wasi_stdout(stdout);
    }

    /// Sets the stderer file descriptor to `stderr`.
    ///
    /// # Safety
    /// The provided file descriptor must be valid. Additionally, the shell **does not** close the
    /// provided fd, so that **must** be handled after the fd has been fully used up by the calling
    /// code.
    pub(self) unsafe fn stderr(&mut self, stderr: RawFileDescriptor) {
        self.stderr = stderr;
        self.env.wasi_stderr(stderr);
    }

    #[allow(clippy::result_unit_err)]
    pub fn load(&mut self, src: &str, name: &str, export: &str) -> ShellResult<()> {
        let buf = Lexer::new(src).lex();
        let mut program = match Parser::new(&buf).parse() {
            Ok(program) => program,
            Err(err) => {
                let mut stderr = unsafe { OpenFileDescriptor::new(self.stderr) };
                err.write_to(src, name, &mut stderr);
                self.set_status(1);
                return Err(());
            }
        };
        let def_names = program
            .defs
            .iter()
            .map(|def| def.name.clone())
            .collect::<Vec<_>>();
        self.env.add_funcs(std::mem::take(&mut program.defs));
        for def_name in &def_names {
            let def = self.env.get_func(def_name).unwrap();
            // TODO: this is quite ugly and not idiomatic. Just abuses raw pointers to get around
            // ownership rules so the closure can have a mutable access to `self`. This should be
            // safe, because Shell owns the store, so it's impossible for the closure to execute if
            // `self` doesn't exist. Either way, this is abusing an escape hatch. The only other
            // way I know how to do this is to wrap `Shell` in an `Rc<RefCell<..>>`, which doesn't
            // use `unsafe`, but pretty much ruins all the existing shell code and would require a
            // huge rewrite for such a small ask.
            let s = self as *mut Shell;
            self.env.store_mut().define(
                export,
                def_name,
                HostFunc::wrap(move |_: Instance, _: &mut Store| unsafe {
                    let mut stderr =
                        File::from_raw_file_descriptor(std::io::stderr().as_raw_file_descriptor());
                    let _ = (*s).run_func(&[], &def, &mut stderr).is_err();
                    stderr.into_raw_file_descriptor();
                }),
            );
        }

        Ok(())
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> ShellResult<ControlFlow> {
        match stmt {
            Stmt::Pipeline(pipeline) => Ok(ControlFlow::Value(
                self.eval_pipeline(pipeline, /*capture =*/ false)?,
            )),
            Stmt::Expr(expr) => Ok(ControlFlow::Value(self.eval_expr(expr)?)),
            Stmt::AliasAssign(assign) => {
                self.eval_alias_assign(assign)?;
                Ok(ControlFlow::Value(Value::Null))
            }
            Stmt::Assign(assign) => {
                self.eval_assign(assign)?;
                Ok(ControlFlow::Value(Value::Null))
            }
            Stmt::Export(export) => {
                self.eval_export(export)?;
                Ok(ControlFlow::Value(Value::Null))
            }
            Stmt::If(if_) => self.eval_if(if_),
            Stmt::While(while_) => self.eval_while(while_),
            Stmt::Break => Ok(ControlFlow::Break),
            Stmt::Continue => Ok(ControlFlow::Continue),
            Stmt::Return => Ok(ControlFlow::Return),
        }
    }

    fn eval_pipeline(&mut self, pipeline: &Pipeline, capture: bool) -> ShellResult<Value> {
        if capture && pipeline.write.is_none() {
            let out = self.run_pipeline_capture(pipeline)?;
            return Ok(Value::String(out.into()));
        }

        match pipeline.write.as_deref() {
            // `>` redirect
            Some(PipelineEnd {
                expr,
                kind: PipelineEndKind::Write,
            }) => {
                let mut opts = OpenOptions::new();
                self.run_pipeline_redirect(expr, pipeline, opts.write(true).create(true))?;
            }
            // `>>` redirect
            Some(PipelineEnd {
                expr,
                kind: PipelineEndKind::Append,
            }) => {
                let mut opts = OpenOptions::new();
                self.run_pipeline_redirect(expr, pipeline, opts.append(true).create(true))?;
            }
            // No redirect case
            None => {
                // We have to do a litle bit of a hack to get Rust to accept this code on Windows.
                // For some reason, `AsRawFileDescriptor` is not implemented for
                // `RawFileDescriptor` on Windows (which is caused by the missing corresponding
                // trait implementation in std). I'm not sure if that's intentional or not. This
                // makes it impossible to use `FileDescriptor::dup`.
                //
                // Either way, we can get around this by turning the `RawFileDescriptor` into a
                // `FileDescriptor`, calling `try_clone` (a wrapper of `dup`).
                let stdout_fd = unsafe { OpenFileDescriptor::new(self.stdout) };
                // We duplicate the stdout fd because `run_pipeline` will close whatever's passed
                // in, which we don't want because we need to use the file descriptor multiple
                // times.
                let stdout_dup = self.dup_fd(stdout_fd)?;
                let stderr_fd = unsafe { OpenFileDescriptor::new(self.stderr) };
                let stderr_dup = self.dup_fd(stderr_fd)?;
                // SAFETY: `stdout_dup` is valid because it's just been dupliated from our stdout,
                // which must've been valid as a result of the error handling
                unsafe {
                    self.run_pipeline(
                        pipeline,
                        stdout_dup.into_raw_file_descriptor(),
                        stderr_dup.into_raw_file_descriptor(),
                        None,
                    )?;
                }
            }
        }

        Ok(if !capture {
            Value::Null
        } else {
            // This branch is reachable if `capture` is true, but the user is also redirecting to
            // a file. This is pretty much nonsensical, but we'll just return an empty string just
            // in case.
            Value::String("".into())
        })
    }

    /// Runs a pipeline, redircting into a file that will be opened with the given options (`opts`).
    fn run_pipeline_redirect(
        &mut self,
        expr: &Expr,
        pipeline: &Pipeline,
        opts: &mut OpenOptions,
    ) -> ShellResult<()> {
        let path = self.eval_expr(expr)?.to_string();
        // Open the file using the given path
        let file = opts
            .open(path)
            .map_err(|err| {
                self.set_status(1);
                ShellError::RedirectError(err)
            })
            .map_err(|err| {
                self.print_err(err);
            })?;
        let stderr_fd = unsafe { OpenFileDescriptor::new(self.stderr) };
        let stderr_dup = self.dup_fd(stderr_fd)?;
        // SAEFTY: `file` is a valid raw fd because it has opened successfully at this pointt
        unsafe {
            self.run_pipeline(
                pipeline,
                file.into_raw_file_descriptor(),
                stderr_dup.into_raw_file_descriptor(),
                None,
            )?;
        }
        Ok(())
    }

    /// Runs a pipeline, capturing its output into a `String`.
    fn run_pipeline_capture(&mut self, pipeline: &Pipeline) -> ShellResult<String> {
        // The stdout of the pipeline will be written to the `write_end`
        let (mut read_end, write_end) =
            os_pipe::pipe()
                .map_err(ShellError::PipeError)
                .map_err(|err| {
                    self.print_err(err);
                })?;
        // We create a thread to do reads here so that we always make progress on io. Deadlocks
        // might occur when pipes gets completely filled, and this thread makes sure that doesn't
        // happen.
        let handle = std::thread::spawn(move || -> io::Result<Vec<u8>> {
            let mut output = vec![];
            read_end.read_to_end(&mut output)?;
            Ok(output)
        });
        let stderr_fd = unsafe { OpenFileDescriptor::new(self.stderr) };
        let stderr_dup = self.dup_fd(stderr_fd)?;
        // SAFETY: `write_end` is a newly created pipe, so this shold be fine. `run_pipeline`
        // should also drop `write_end` so we won't hang when reading from `read_end`.
        unsafe {
            self.run_pipeline(
                pipeline,
                write_end.into_raw_file_descriptor(),
                stderr_dup.into_raw_file_descriptor(),
                None,
            )?;
        }
        // This should not block as long as `run_pipeline` closes `write_end`
        let out = handle
            .join()
            .unwrap()
            .map_err(ShellError::PipeError)
            .map_err(|err| self.print_err(err))?;
        let mut out = String::from_utf8_lossy(&out).to_string();
        while out.ends_with('\n') || out.ends_with('\r') {
            out.truncate(out.len() - 1);
        }
        Ok(out)
    }

    /// Execute a pipeline, writing results to `default_stdout` and reading stdin from
    /// `default_stdin`.
    ///
    /// Any file descriptors can be passed for io streams, so different output ends (pipes, files,
    /// etc.) can be written to easily.
    ///
    /// # Safety
    /// This method can be called safely as along as `default_stdout` is a valid file descriptor.
    /// Since this method eventually closes the provided fd, calling code must make sure that the
    /// file descriptor is not closed again.
    unsafe fn run_pipeline(
        &mut self,
        pipeline: &Pipeline,
        default_stdout: RawFileDescriptor,
        stderr: RawFileDescriptor,
        default_stdin: Option<os_pipe::PipeReader>,
    ) -> ShellResult<()> {
        let old_stdout = self.stdout;
        let old_stderr = self.stderr;
        let mut last_result = 0;
        let mut stdin = default_stdin;
        let mut iter = pipeline.commands.iter().peekable();
        while let Some(cmd) = iter.next() {
            let stdout;
            let pipes = if iter.peek().is_some() {
                let (out, in_) = os_pipe::pipe()
                    .map_err(ShellError::PipeError)
                    .map_err(|err| {
                        self.print_err(err);
                    })?;
                stdout = in_
                    .try_clone()
                    .map_err(ShellError::DupError)
                    .map_err(|err| {
                        self.print_err(err);
                    })?
                    .into_raw_file_descriptor();
                Some((out, in_))
            } else {
                // Last command in pipeline
                stdout = default_stdout;
                None
            };

            // We set `stdout` to the new stdout, but we need to make sure that we reset it back to
            // the old stdout.
            // This allows us to write directly to the pipe
            self.stdout(stdout);
            let stderr = if !cmd.merge_stderr {
                let stderr = unsafe { OpenFileDescriptor::new(stderr) };
                self.dup_fd(stderr)?.into_raw_file_descriptor()
            } else {
                // If the command should merge stderr, simply make the stderr fd the same as the stdout
                // fd
                stdout
            };
            self.stderr(stderr);
            let stderr = unsafe { OpenFileDescriptor::new(stderr) };
            let result = self.run_command(
                cmd,
                &pipeline.env,
                stdout,
                stderr.into_raw_file_descriptor(),
                stdin.take(),
                pipes.is_none(),
            );
            self.stdout(old_stdout);
            self.stderr(old_stderr);
            if let Some((out, _)) = pipes {
                stdin = Some(out);
            }

            let Ok(result) = result else {
                last_result = self.last_status;
                continue;
            };

            #[cfg(unix)]
            {
                last_result = result;
            }
            #[cfg(windows)]
            {
                last_result = result as i32;
            }
        }

        self.set_status(last_result);

        Ok(())
    }

    /// Evaluates a command, returning the exit status of the command.
    ///
    /// # Safety
    /// This command is safe as long as the provided `stdout` is valid. This will close `stdout`.
    unsafe fn run_command(
        &mut self,
        cmd: &Command,
        env: &[EnvSet],
        stdout: RawFileDescriptor,
        stderr: RawFileDescriptor,
        stdin: Option<os_pipe::PipeReader>,
        to_tty: bool,
    ) -> ShellResult<i32> {
        // Priority one is aliases for commands
        // TODO: avoid clone here?
        if let Some(mut alias) = self.env.get_alias(cmd.name.as_str()).cloned() {
            alias
                .commands
                .last_mut()
                .unwrap()
                .args
                .extend_from_slice(&cmd.args);
            // Recursive alias found
            if alias
                .commands
                .iter()
                .any(|alias_cmd| alias_cmd.name == cmd.name)
            {
                // Remove the alias so that we don't recurse infinitely
                let pipeline = self.env.remove_alias(&cmd.name).unwrap();
                self.run_pipeline(&alias, stdout, stderr, stdin)?;
                self.env.set_alias(cmd.name.clone(), pipeline);
                return Ok(self.last_status);
            }
            self.run_pipeline(&alias, stdout, stderr, stdin)?;
            return Ok(self.last_status);
        }

        // This is where we take ownership of the file descriptor, which will be closed when
        // `stdout` is dropped.
        let stdout = File::from_raw_file_descriptor(stdout);
        let mut stderr = File::from_raw_file_descriptor(stderr);

        if let Some(func) = self.env.get_func(&Ident::new(&cmd.name)) {
            self.run_func(&cmd.args, &func, &mut stderr)?;
            return Ok(0);
        }

        let env = env
            .iter()
            .map(|env_set| -> ShellResult<(String, String)> {
                Ok((
                    env_set.name.to_string(),
                    self.eval_expr(&env_set.expr)?.to_string(),
                ))
            })
            .collect::<ShellResult<Vec<(String, String)>>>()?;

        // Priority two is wasm functions in the environment (via the `load` built-in)
        if let Some(wasm_func) = self.env.get_module_func(&cmd.name) {
            return match self.run_wasm_func(wasm_func, cmd, stdout, stderr, stdin, &env) {
                Ok(_) => Ok(0),
                Err(_) => Ok(1),
            };
        }

        let args = cmd
            .args
            .iter()
            .map(|arg| self.eval_expr(arg).map(|val| val.to_string()))
            .collect::<ShellResult<Vec<_>>>()?;

        // Priority three is built-ins
        if let Some(builtin) = Builtin::from_name(&cmd.name) {
            let io_streams = IoStreams {
                stdout,
                stderr,
                stdin: stdin.map(|reader| unsafe {
                    File::from_raw_file_descriptor(reader.into_raw_file_descriptor())
                }),
                to_tty,
            };
            let status = builtin.run(self, args, io_streams, &env)?;
            return Ok(status);
        }

        // If the command name is not an alias, wasm function, or built-in, it must be a Command
        let mut command = process::Command::new(cmd.name.as_str());
        command.args(args).stdout(stdout).stderr(stderr).envs(env);

        if let Some(stdin) = stdin {
            command.stdin(stdin);
        }

        let mut child = command
            .spawn()
            .map_err(|err| match err.kind() {
                io::ErrorKind::NotFound => {
                    self.set_status(127);
                    ShellError::CommandNotFound(cmd.name.clone())
                }
                _ => ShellError::CommandFailed(err),
            })
            .map_err(|err| {
                self.print_err(err);
            })?;
        // Final command in pipeline
        if to_tty {
            let out = child.wait().unwrap();
            #[cfg(unix)]
            let out = out
                .code()
                .or_else(|| out.signal().map(|s| s + 128))
                .unwrap_or(0);
            #[cfg(windows)]
            let out = out.status.code().unwrap();

            return Ok(out);
        }

        // TODO: I'm sure that this is not correct. Commands in a pipe should definitely not always
        // have a 0 status...
        Ok(0)
    }

    fn eval_alias_assign(
        &mut self,
        AliasAssign { name, pipeline }: &AliasAssign,
    ) -> ShellResult<()> {
        // TODO: avoid clone here?
        self.env.set_alias(name.clone(), pipeline.clone());
        Ok(())
    }

    fn eval_assign(&mut self, Assign { name, expr, global }: &Assign) -> ShellResult<()> {
        let val = self.eval_expr(expr)?;
        if *global {
            self.env.set_global(name.clone(), val);
        } else {
            self.env.set(name.clone(), val);
        }

        Ok(())
    }

    fn eval_export(&mut self, export: &Export) -> ShellResult<()> {
        let name_str: &str = &export.name;
        std::env::set_var(name_str, self.eval_expr(&export.expr)?.to_string());

        Ok(())
    }

    fn eval_expr(&mut self, expr: &Expr) -> ShellResult<Value> {
        Ok(match expr {
            Expr::Ident(name) => self
                .env
                .get(name)
                .cloned()
                .ok_or_else(|| ShellError::Unbound(name.clone()))
                .map_err(|err| {
                    self.print_err(err);
                })?,
            Expr::Infix(infix) => self.eval_infix(infix)?,
            Expr::Prefix(prefix) => self.eval_prefix(prefix)?,
            Expr::String(s) => Value::String(s.clone()),
            Expr::Bool(b) => Value::Bool(*b),
            Expr::Number(n) => Value::Number(*n),
            Expr::Pipeline(pipeline) => self.eval_pipeline(pipeline, /*capture =*/ true)?,
            Expr::LastStatus => Value::Number(self.last_status as f64),
            Expr::Env(name) => self.eval_env_var(name)?,
            Expr::Tilde => Value::String(SmolStr::new(
                dirs::home_dir().unwrap_or_default().to_string_lossy(),
            )),
        })
    }

    fn eval_infix(&mut self, infix: &InfixExpr) -> ShellResult<Value> {
        let lhs = self.eval_expr(&infix.lhs)?;
        let rhs = self.eval_expr(&infix.rhs)?;
        let lhs_type = lhs.type_of();
        let rhs_type = rhs.type_of();

        match (lhs, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => self.eval_numeric_infix(infix.op, lhs, rhs),
            (Value::String(lhs), Value::String(rhs)) => self.eval_string_infix(infix.op, lhs, rhs),
            (Value::String(lhs), Value::Number(rhs)) => {
                self.eval_string_numeric_infix(infix.op, lhs, rhs)
            }
            (Value::Number(lhs), Value::String(rhs)) => {
                self.eval_numeric_string_infix(infix.op, lhs, rhs)
            }
            (Value::Bool(lhs), Value::Bool(rhs)) => self.eval_bool_infix(infix.op, lhs, rhs),
            (Value::Bool(lhs), Value::String(rhs)) => {
                self.eval_string_infix(infix.op, lhs.to_string().into(), rhs)
            }
            (Value::String(lhs), Value::Bool(rhs)) => {
                self.eval_string_infix(infix.op, lhs, rhs.to_string().into())
            }
            _ => {
                self.print_err(ShellError::TypeErrorInfix {
                    lhs: lhs_type,
                    rhs: rhs_type,
                    op: infix.op,
                });
                Err(())
            }
        }
    }

    fn eval_if(&mut self, if_: &If) -> ShellResult<ControlFlow> {
        let cond = self.eval_expr(&if_.condition)?;
        if cond.is_truthy() {
            control_flow!(self.eval_block(&if_.body)?);
        } else if let Some(else_) = &if_.else_ {
            control_flow!(self.eval_block(else_)?);
        }

        Ok(ControlFlow::Value(Value::Null))
    }

    fn eval_while(&mut self, while_: &While) -> ShellResult<ControlFlow> {
        while self.eval_expr(&while_.condition)?.is_truthy() {
            match self.eval_block(&while_.body)? {
                ControlFlow::Break => break,
                ControlFlow::Continue => continue,
                ControlFlow::Return => return Ok(ControlFlow::Return),
                ControlFlow::Value(_) => {}
            }
        }
        Ok(ControlFlow::Value(Value::Null))
    }

    fn eval_block(&mut self, block: &[Stmt]) -> ShellResult<ControlFlow> {
        for stmt in block {
            control_flow!(self.eval_stmt(stmt)?);
        }

        Ok(ControlFlow::Value(Value::Null))
    }

    fn eval_prefix(&mut self, prefix: &PrefixExpr) -> ShellResult<Value> {
        let value = self.eval_expr(&prefix.expr)?;

        match value {
            Value::Number(n) => self.eval_numeric_prefix(prefix.op, n),
            Value::String(s) if s.parse::<f64>().is_ok() => {
                self.eval_numeric_prefix(prefix.op, s.parse().unwrap())
            }
            _ => {
                self.print_err(ShellError::TypeErrorPrefix {
                    expr: value.type_of(),
                    op: prefix.op,
                });
                Err(())
            }
        }
    }

    fn eval_numeric_infix(&mut self, op: InfixOp, lhs: f64, rhs: f64) -> ShellResult<Value> {
        Ok(match op {
            InfixOp::Add => Value::Number(lhs + rhs),
            InfixOp::Sub => Value::Number(lhs - rhs),
            InfixOp::Mul => Value::Number(lhs * rhs),
            InfixOp::Div => Value::Number(lhs / rhs),
            InfixOp::Lt => Value::Bool(lhs < rhs),
            InfixOp::Gt => Value::Bool(lhs > rhs),
            InfixOp::Le => Value::Bool(lhs <= rhs),
            InfixOp::Ge => Value::Bool(lhs >= rhs),
            InfixOp::Eq => Value::Bool(lhs == rhs),
            InfixOp::Ne => Value::Bool(lhs != rhs),
        })
    }

    fn eval_bool_infix(&mut self, op: InfixOp, lhs: bool, rhs: bool) -> ShellResult<Value> {
        let result = match op {
            InfixOp::Eq => Value::Bool(lhs == rhs),
            InfixOp::Ne => Value::Bool(lhs != rhs),
            _ => self.eval_string_infix(op, lhs.to_string().into(), rhs.to_string().into())?,
        };

        Ok(result)
    }

    fn eval_numeric_prefix(&mut self, op: PrefixOp, n: f64) -> ShellResult<Value> {
        let result = match op {
            PrefixOp::Sign => n,
            PrefixOp::Neg => -n,
            PrefixOp::Bang => {
                self.print_err(ShellError::TypeErrorPrefix {
                    expr: Type::Number,
                    op,
                });
                return Err(());
            }
        };

        Ok(Value::Number(result))
    }

    fn eval_string_infix(&mut self, op: InfixOp, lhs: SmolStr, rhs: SmolStr) -> ShellResult<Value> {
        let result = match op {
            InfixOp::Add => Value::String(format!("{lhs}{rhs}").into()),
            InfixOp::Eq => Value::Bool(lhs == rhs),
            InfixOp::Ne => Value::Bool(lhs != rhs),
            _ => {
                self.print_err(ShellError::TypeErrorInfix {
                    lhs: Type::String,
                    rhs: Type::String,
                    op,
                });
                return Err(());
            }
        };

        Ok(result)
    }

    fn eval_string_numeric_infix(
        &mut self,
        op: InfixOp,
        lhs: SmolStr,
        rhs: f64,
    ) -> ShellResult<Value> {
        if let Ok(lhs) = lhs.parse::<f64>() {
            return self.eval_numeric_infix(op, lhs, rhs);
        }

        let result = match op {
            InfixOp::Add => format!("{lhs}{rhs}"),
            InfixOp::Mul => lhs.repeat(rhs as usize),
            _ => {
                self.print_err(ShellError::TypeErrorInfix {
                    lhs: Type::String,
                    rhs: Type::Number,
                    op,
                });
                return Err(());
            }
        };

        Ok(Value::String(result.into()))
    }

    fn eval_numeric_string_infix(
        &mut self,
        op: InfixOp,
        lhs: f64,
        rhs: SmolStr,
    ) -> ShellResult<Value> {
        if let Ok(rhs) = rhs.parse::<f64>() {
            return self.eval_numeric_infix(op, lhs, rhs);
        }

        let result = match op {
            InfixOp::Add => format!("{lhs}{rhs}"),
            InfixOp::Mul => rhs.repeat(lhs as usize),
            _ => {
                self.print_err(ShellError::TypeErrorInfix {
                    lhs: Type::Number,
                    rhs: Type::String,
                    op,
                });
                return Err(());
            }
        };

        Ok(Value::String(result.into()))
    }

    fn eval_env_var(&mut self, name: &Ident) -> ShellResult<Value> {
        let name_str: &str = name;
        let var = std::env::var_os(name_str).unwrap_or_default();
        let val = var.to_string_lossy();
        Ok(match val {
            // We know that if this is the borrwed variant, it's a valid UTF-8 string, so
            // there's no need for any cloning.
            Cow::Borrowed(_) => Value::String(var.into_string().unwrap().into()),
            Cow::Owned(new) => Value::String(new.into()),
        })
    }

    fn run_wasm_func(
        &mut self,
        wasm_func: shwasi_engine::WasmFuncUntyped,
        cmd: &Command,
        mut stdout: File,
        mut stderr: File,
        stdin: Option<os_pipe::PipeReader>,
        env: &[(String, String)],
    ) -> ShellResult<()> {
        if let Err(err) = unsafe {
            self.env.prepare_wasi(
                std::iter::empty::<String>(),
                stdin.map(|s| s.into_raw_file_descriptor()),
                env,
            )
        } {
            writeln!(stderr, "shwasi: error prepare WASI: {err}").expect("write to stderr failed!");
            return Err(());
        }

        let arg_types = wasm_func.arg_types(self.env.store_mut()).to_vec();
        if arg_types.len() != cmd.args.len() {
            writeln!(
                stderr,
                "shwasi: expected {} args for wasm function",
                arg_types.len()
            )
            .expect("write to stderr failed!");
            return Err(());
        }

        // We must convert shwasi values to wasm values
        let args = cmd
            .args
            .iter()
            .zip(arg_types)
            .map(|(arg, ty)| -> ShellResult<shwasi_engine::Value> {
                let mut to_val = |n| match ty {
                    ValType::I32 => Ok(shwasi_engine::Value::I32(n as u32)),
                    ValType::I64 => Ok(shwasi_engine::Value::I64(n as u64)),
                    ValType::F32 => Ok(shwasi_engine::Value::F32(n as f32)),
                    ValType::F64 => Ok(shwasi_engine::Value::F64(n)),
                    _ => {
                        writeln!(
                            stderr,
                            "shwasi: could not convert {n} to {ty} for wasm function `{}`",
                            cmd.name
                        )
                        .expect("write to stderr failed!");
                        Err(())
                    }
                };
                match self.eval_expr(arg)? {
                    Value::Number(n) => to_val(n),
                    Value::String(s) if s.parse::<f64>().is_ok() => {
                        let n = s.parse::<f64>().unwrap();
                        to_val(n)
                    }
                    Value::String(_) => {
                        writeln!(
                            stderr,
                            "shwasi: cannot pass string to wasm function `{}`",
                            cmd.name
                        )
                        .expect("write to stderr failed!");
                        Err(())
                    }
                    Value::Bool(b) => match ty {
                        ValType::I32 => Ok(shwasi_engine::Value::I32(b as u32)),
                        ValType::I64 => Ok(shwasi_engine::Value::I64(b as u64)),
                        _ => {
                            writeln!(
                                stderr,
                                "shwasi: could not convert bool to {ty} for wasm function `{}`",
                                cmd.name
                            )
                            .expect("write to stderr failed!");
                            Err(())
                        }
                    },
                    Value::Null => {
                        writeln!(
                            stderr,
                            "shwasi: cannot pass null to wasm function `{}`",
                            cmd.name
                        )
                        .expect("write to stderr failed!");
                        Err(())
                    }
                }
            })
            .collect::<ShellResult<Vec<_>>>()?;

        let results = match wasm_func.call(self.env.store_mut(), &args) {
            Ok(results) => results,
            Err(err) => {
                writeln!(
                    stderr,
                    "shwasi: error calling wasm function `{}`: {err}",
                    cmd.name
                )
                .expect("write to stderr failed!");
                return Err(());
            }
        };
        for result in results {
            writeln!(stdout, "{result}").expect("write to stdout failed!");
        }

        Ok(())
    }

    fn run_func(&mut self, args: &[Expr], func: &Def, stderr: &mut File) -> ShellResult<()> {
        self.env.push_scope();
        let matches = match clap::Command::new(func.name.to_string())
            .no_binary_name(true)
            .args(func.args.0.iter().map(|arg| {
                match arg {
                    DefArg::Positional { name } => clap::Arg::new(name.to_string()).required(true),
                    DefArg::Named {
                        name,
                        alias,
                        default,
                    } => {
                        let mut arg = clap::Arg::new(name.to_string())
                            .default_value(default.to_string())
                            .required(false)
                            .long(name.to_string());
                        if let Some(alias) = alias {
                            arg = arg.short(*alias);
                        }
                        arg
                    }
                    DefArg::Boolean { name, alias } => clap::Arg::new(name.to_string())
                        .required(false)
                        .action(clap::ArgAction::SetTrue)
                        .long(name.to_string())
                        .short(*alias),
                }
            }))
            .try_get_matches_from(
                args.iter()
                    .map(|arg| -> ShellResult<String> { Ok(self.eval_expr(arg)?.to_string()) })
                    .collect::<ShellResult<Vec<_>>>()?,
            ) {
            Ok(matches) => matches,
            Err(err) => {
                write!(stderr, "{err}").expect("write to stderr failed!");
                return Err(());
            }
        };
        for arg in &func.args.0 {
            let (name, value) = match arg {
                DefArg::Positional { name } | DefArg::Named { name, .. } => {
                    let p = matches.get_one::<String>(name).unwrap();
                    (name.clone(), Value::String(p.into()))
                }
                DefArg::Boolean { name, .. } => {
                    let value = matches.get_flag(name);
                    (name.clone(), Value::Bool(value))
                }
            };
            self.env.set(name, value);
        }
        if let Err(err) = self.eval_block(&func.body) {
            self.env.pop_scope();
            return Err(err);
        }
        self.env.pop_scope();
        Ok(())
    }

    fn print_err(&mut self, err: ShellError) {
        let mut stderr = unsafe { OpenFileDescriptor::new(self.stderr) };
        writeln!(stderr, "shwasi: {err}").expect("write to stderr failed!");
    }

    fn set_status(&mut self, exit_status: i32) {
        self.last_status = exit_status;
    }

    fn dup_fd(&mut self, fd: OpenFileDescriptor) -> ShellResult<FileDescriptor> {
        fd.try_clone()
            .map_err(|err| match err {
                filedescriptor::Error::Dup { source, .. } => ShellError::DupError(source),
                _ => unreachable!(),
            })
            .map_err(|err| {
                self.print_err(err);
            })
    }
}

impl Default for Shell {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
enum ControlFlow {
    Break,
    Continue,
    Return,
    Value(Value),
}

/// A wrapper around a `FileDescriptor` that does not close the underlying file descriptor when
/// dropped.
///
/// This is useful when we want to preserve a file descriptor across multiple calls to a function,
/// and makes it easier to reason about the lifetime of the file descriptor.
struct OpenFileDescriptor(ManuallyDrop<FileDescriptor>);

impl OpenFileDescriptor {
    pub unsafe fn new(fd: RawFileDescriptor) -> Self {
        Self(ManuallyDrop::new(unsafe {
            FileDescriptor::from_raw_file_descriptor(fd)
        }))
    }

    /// Clones the underlying file descriptor.
    ///
    /// This will return an **owned** FileDescriptor, which means that the cloned file descriptor
    /// will be closed when the returned value is dropped.
    pub fn try_clone(&self) -> Result<FileDescriptor, filedescriptor::Error> {
        self.0.try_clone()
    }
}

impl IntoRawFileDescriptor for OpenFileDescriptor {
    fn into_raw_file_descriptor(self) -> RawFileDescriptor {
        self.0.as_raw_file_descriptor()
    }
}

impl io::Write for OpenFileDescriptor {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.flush()
    }
}
