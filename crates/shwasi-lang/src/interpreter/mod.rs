mod builtins;
mod env;
mod error;
mod value;

use crate::{
    ast::{
        AliasAssign, Assign, EnvSet, Export, InfixOp, Pipeline, PipelineEnd, PipelineEndKind,
        PrefixOp,
    },
    interpreter::{builtins::Builtin, env::Env},
    parser::ast::{Command, Expr, InfixExpr, PrefixExpr, Stmt},
    Lexer, Parser,
};
pub use error::*;
use filedescriptor::{
    AsRawFileDescriptor, FileDescriptor, FromRawFileDescriptor, IntoRawFileDescriptor,
    RawFileDescriptor,
};
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

pub struct Shell {
    env: Env,
    stdout: RawFileDescriptor,
    last_status: i32,
}

impl Shell {
    pub fn new() -> Self {
        Self {
            env: Env::new(),
            stdout: io::stdout().as_raw_file_descriptor(),
            last_status: 0,
        }
    }

    /// Runs the shell on the given source code.
    #[allow(clippy::result_unit_err)]
    pub fn run(&mut self, src: &str, name: &str) -> ShellResult<Option<Value>> {
        let buf = Lexer::new(src).lex();
        let program = match Parser::new(&buf).parse() {
            Ok(program) => program,
            Err(err) => {
                // SAFETY: `stdout` must be a valid file descriptor
                let mut stdout = unsafe { OpenFileDescriptor::new(self.stdout) };
                err.write_to(src, name, &mut stdout);
                self.set_status(1);
                return Err(());
            }
        };

        let mut result = Value::Null;
        for stmt in program {
            result = match self.eval_stmt(&stmt) {
                Ok(result) => result,
                // TODO: write to global stderr instead
                Err(_) => continue,
            }
        }

        // Reset stdout to the original stdout. This is necessary for operations like `source`,
        // where the user may have redirected stdout somewhere else temporarily.
        unsafe { self.stdout(io::stdout().as_raw_file_descriptor()) };

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

    fn eval_stmt(&mut self, stmt: &Stmt) -> ShellResult<Value> {
        match stmt {
            Stmt::Pipeline(pipeline) => self.eval_pipeline(pipeline, /*capture =*/ false),
            Stmt::Expr(expr) => self.eval_expr(expr),
            Stmt::AliasAssign(assign) => {
                self.eval_alias_assign(assign)?;
                Ok(Value::Null)
            }
            Stmt::Assign(assign) => {
                self.eval_assign(assign)?;
                Ok(Value::Null)
            }
            Stmt::Export(export) => {
                self.eval_export(export)?;
                Ok(Value::Null)
            }
        }
    }

    fn eval_pipeline(&mut self, pipeline: &Pipeline, capture: bool) -> ShellResult<Value> {
        if capture && pipeline.write.is_none() {
            let (mut read_end, write_end) = os_pipe::pipe()
                .map_err(ShellError::PipeError)
                .map_err(|err| {
                    self.print_err(err);
                })?;
            // SAFETY: write end is a valid file descriptor that has not been closed yet
            unsafe { self.run_pipeline(pipeline, write_end.into_raw_file_descriptor(), None)? };
            let mut out = vec![];
            read_end
                .read_to_end(&mut out)
                .map_err(ShellError::PipeError)
                .map_err(|err| {
                    self.print_err(err);
                })?;
            let mut out = String::from_utf8_lossy(&out).to_string();
            while out.ends_with('\n') || out.ends_with('\r') {
                out.truncate(out.len() - 1);
            }
            return Ok(Value::String(out.into()));
        }

        match pipeline.write.as_deref() {
            Some(PipelineEnd {
                expr,
                kind: PipelineEndKind::Write,
            }) => {
                let path = self.eval_expr(expr)?.to_string();
                let file = File::create(path)
                    .map_err(|err| {
                        self.set_status(1);
                        ShellError::RedirectError(err)
                    })
                    .map_err(|err| {
                        self.print_err(err);
                    })?;
                // SAFETY: stdout here comes from a just-opened file, which is a valid raw fd
                unsafe { self.run_pipeline(pipeline, file.into_raw_file_descriptor(), None)? };
            }
            Some(PipelineEnd {
                expr,
                kind: PipelineEndKind::Append,
            }) => {
                let path = self.eval_expr(expr)?.to_string();
                let file = OpenOptions::new()
                    .append(true)
                    .create(true)
                    .open(path)
                    .map_err(|err| {
                        self.set_status(1);
                        ShellError::RedirectError(err)
                    })
                    .map_err(|err| {
                        self.print_err(err);
                    })?;
                // SAFETY: stdout here comes from a just-opened file, which is a valid raw fd
                unsafe { self.run_pipeline(pipeline, file.into_raw_file_descriptor(), None)? };
            }
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
                let stdout_dup = stdout_fd
                    .try_clone()
                    .map_err(|err| match err {
                        filedescriptor::Error::Dup { source, .. } => ShellError::DupError(source),
                        _ => unreachable!(),
                    })
                    .map_err(|err| {
                        self.print_err(err);
                    })?;
                // SAFETY: `stdout_dup` is valid because it's just been dupliated from our stdout,
                // which must've been valid as a result of the error handling
                unsafe {
                    self.run_pipeline(pipeline, stdout_dup.into_raw_file_descriptor(), None)?;
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

    /// Execute a pipeline, writing results to `default_stdout` and reading stdin from
    /// `default_stdin`.
    ///
    /// # Safety
    /// This method can be called safely as along as `default_stdout` is a valid file descriptor.
    /// Since this method eventually closes the provided fd, calling code must make sure that the
    /// file descriptor is not closed again.
    unsafe fn run_pipeline(
        &mut self,
        pipeline: &Pipeline,
        default_stdout: RawFileDescriptor,
        default_stdin: Option<os_pipe::PipeReader>,
    ) -> ShellResult<()> {
        let old = self.stdout;
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
            let Ok(result) = self.run_command(cmd, &pipeline.env, stdout, stdin.take()) else {
                self.stdout(old);
                if let Some((out, _)) = pipes {
                    stdin = Some(out);
                }
                last_result = self.last_status;
                continue;
            };
            self.stdout(old);

            if let Some((out, _)) = pipes {
                stdin = Some(out);
            }

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
        stdin: Option<os_pipe::PipeReader>,
    ) -> ShellResult<i32> {
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
                self.run_pipeline(&alias, stdout, stdin)?;
                self.env.set_alias(cmd.name.clone(), pipeline);
                return Ok(self.last_status);
            }
            self.run_pipeline(&alias, stdout, stdin)?;
            return Ok(self.last_status);
        }

        // This is where we take ownership of the file descriptor, which will be closed when
        // `stdout` is dropped.
        let stdout = File::from_raw_file_descriptor(stdout);

        if let Some(wasm_func) = self.env.get_module_func(&cmd.name) {
            return match self.run_wasm_func(wasm_func, cmd, stdout) {
                Ok(_) => Ok(0),
                Err(_) => Ok(1),
            };
        }

        let args = cmd
            .args
            .iter()
            .map(|arg| self.eval_expr(arg).map(|val| val.to_string()))
            .collect::<ShellResult<Vec<_>>>()?;

        if let Some(builtin) = Builtin::from_name(cmd.name.as_str()) {
            let status = builtin.run(self, args, stdout)?;
            return Ok(status);
        }

        let mut command = process::Command::new(cmd.name.as_str());
        // TODO: properly handle stderr
        command
            .args(args)
            .stdout(stdout)
            .stderr(process::Stdio::inherit());

        for env_set in env {
            let value = self.eval_expr(&env_set.expr)?;
            command.env(env_set.name.as_str(), value.to_string());
        }

        if let Some(stdin) = stdin {
            command.stdin(stdin);
        }

        let out = command
            .output()
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

        #[cfg(unix)]
        let out = out
            .status
            .code()
            .or_else(|| out.status.signal().map(|s| s + 128))
            .unwrap_or(0);
        #[cfg(windows)]
        let out = out.status.code().unwrap();

        Ok(out)
    }

    fn eval_alias_assign(
        &mut self,
        AliasAssign { name, pipeline }: &AliasAssign,
    ) -> ShellResult<()> {
        // TODO: avoid clone here?
        self.env.set_alias(name.clone(), pipeline.clone());
        Ok(())
    }

    fn eval_assign(&mut self, Assign { name, expr }: &Assign) -> ShellResult<()> {
        let val = self.eval_expr(expr)?;
        self.env.set(name.clone(), val);

        Ok(())
    }

    fn eval_export(&mut self, export: &Export) -> ShellResult<()> {
        std::env::set_var(
            export.name.as_str(),
            self.eval_expr(&export.expr)?.to_string(),
        );

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

    fn eval_env_var(&mut self, name: &SmolStr) -> ShellResult<Value> {
        let var = std::env::var_os(name.as_str()).unwrap_or_default();
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
    ) -> ShellResult<()> {
        let arg_types = wasm_func.arg_types(self.env.store_mut()).to_vec();
        if arg_types.len() != cmd.args.len() {
            writeln!(
                stdout,
                "shwasi: expected {} args for wasm function",
                arg_types.len()
            )
            .expect("write to stdout failed!");
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
                            stdout,
                            "shwasi: could not convert {n} to {ty} for wasm function `{}`",
                            cmd.name
                        )
                        .expect("write to stdout failed!");
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
                            stdout,
                            "shwasi: cannot pass string to wasm function `{}`",
                            cmd.name
                        )
                        .expect("write to stdout failed!");
                        Err(())
                    }
                    Value::Bool(b) => match ty {
                        ValType::I32 => Ok(shwasi_engine::Value::I32(b as u32)),
                        ValType::I64 => Ok(shwasi_engine::Value::I64(b as u64)),
                        _ => {
                            writeln!(
                                stdout,
                                "shwasi: could not convert bool to {ty} for wasm function `{}`",
                                cmd.name
                            )
                            .expect("write to stdout failed!");
                            Err(())
                        }
                    },
                    Value::Null => {
                        writeln!(
                            stdout,
                            "shwasi: cannot pass null to wasm function `{}`",
                            cmd.name
                        )
                        .expect("write to stdout failed!");
                        Err(())
                    }
                }
            })
            .collect::<ShellResult<Vec<_>>>()?;

        let results = match wasm_func.call(self.env.store_mut(), &args) {
            Ok(results) => results,
            Err(err) => {
                writeln!(
                    stdout,
                    "shwasi: error calling wasm function `{}`: {err}",
                    cmd.name
                )
                .expect("write to stdout failed!");
                return Err(());
            }
        };
        for result in results {
            let mut stdout = unsafe { OpenFileDescriptor::new(self.stdout) };
            writeln!(stdout, "{result}").expect("write to stdout failed!");
        }

        Ok(())
    }

    fn print_err(&mut self, err: ShellError) {
        let mut stdout = unsafe { OpenFileDescriptor::new(self.stdout) };
        writeln!(stdout, "shwasi: {err}").expect("write to stdout failed!");
    }

    fn set_status(&mut self, exit_status: i32) {
        self.last_status = exit_status;
    }
}

impl Default for Shell {
    fn default() -> Self {
        Self::new()
    }
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

    pub fn try_clone(&self) -> Result<Self, filedescriptor::Error> {
        Ok(Self(ManuallyDrop::new(self.0.try_clone()?)))
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
