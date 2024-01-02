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
};
use smol_str::SmolStr;
use std::{
    borrow::Cow,
    fs::{File, OpenOptions},
    io::{self, Read},
    process,
};
pub use value::*;

pub struct Shell {
    env: Env,
    stdout: FileDescriptor,
    last_status: i32,
}

impl Shell {
    pub fn new() -> Self {
        Self {
            env: Env::new(),
            // TODO: this shouldn't be here
            stdout: FileDescriptor::dup(&io::stdout().as_raw_file_descriptor()).unwrap(),
            last_status: 0,
        }
    }

    pub fn run(&mut self, src: &str) -> ShellResult<Option<Value>> {
        let buf = Lexer::new(src).lex();
        let program = Parser::new(&buf).parse().map_err(ShellError::ParseError)?;

        let mut result = Value::Null;
        for stmt in program {
            result = self.eval_stmt(&stmt)?;
        }

        // Reset stdout to the original stdout. This is necessary for operations like `source`,
        // where the user may have redirected stdout somewhere else temporarily.
        self.stdout = FileDescriptor::dup(&io::stdout().as_raw_file_descriptor()).map_err(
            |err| match err {
                filedescriptor::Error::Dup { fd, source } => ShellError::FdDupError(fd, source),
                _ => unreachable!(),
            },
        )?;

        Ok((!result.is_null()).then_some(result))
    }

    /// Sets the stdout file descriptor to `stdout`.
    ///
    /// # Safety
    /// This takes ownership of the file descriptor. The caller must ensure that the file
    /// descriptor is not closed while it is still in use, etc.
    pub(crate) unsafe fn stdout(&mut self, stdout: impl IntoRawFileDescriptor) {
        self.stdout =
            unsafe { FileDescriptor::from_raw_file_descriptor(stdout.into_raw_file_descriptor()) };
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
            let (mut read_end, write_end) = os_pipe::pipe().map_err(ShellError::PipeError)?;
            self.run_pipeline(pipeline, write_end, None)?;
            let mut out = vec![];
            read_end
                .read_to_end(&mut out)
                .map_err(ShellError::PipeError)?;
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
                let file = File::create(path).map_err(ShellError::CommandFailed)?;
                self.run_pipeline(pipeline, file, None)?;
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
                    .map_err(ShellError::CommandFailed)?;
                self.run_pipeline(pipeline, file, None)?;
            }
            None => {
                // We duplicate the stdout fd because `run_pipeline` will close whatever's passed
                // in, which we don't want because we need to use the file descriptor multiple
                // times.
                self.run_pipeline(pipeline, self.stdout.try_clone().unwrap(), None)?;
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

    fn run_pipeline(
        &mut self,
        pipeline: &Pipeline,
        default_stdout: impl IntoRawFileDescriptor,
        default_stdin: Option<os_pipe::PipeReader>,
    ) -> ShellResult<()> {
        let mut last_result = None;
        // We essentially downcast our impl IntoRawFileDescriptor into a raw fd. This is just so we
        // can have a unified type for stdout.
        let default_stdout = default_stdout.into_raw_file_descriptor();
        let mut stdin = default_stdin;
        let mut iter = pipeline.commands.iter().peekable();
        while let Some(cmd) = iter.next() {
            let stdout;
            let pipes = if iter.peek().is_some() {
                let (out, in_) = os_pipe::pipe().map_err(ShellError::PipeError)?;
                stdout = in_
                    .try_clone()
                    .map_err(ShellError::PipeError)?
                    .into_raw_file_descriptor();
                Some((out, in_))
            } else {
                // Last command in pipeline
                stdout = default_stdout;
                None
            };

            let result = self.run_command(cmd, &pipeline.env, stdout, stdin.take())?;

            if let Some((out, _)) = pipes {
                stdin = Some(out);
            }

            last_result = result;
        }

        self.set_status(last_result);

        Ok(())
    }

    /// Evaluates a command, returning the exit status of the command.
    ///
    /// The exit command is optional, so `None` is returned if the command did not have an exit
    /// status. This is the case for builtins, for example.
    fn run_command(
        &mut self,
        cmd: &Command,
        env: &[EnvSet],
        stdout: impl IntoRawFileDescriptor,
        stdin: Option<os_pipe::PipeReader>,
    ) -> ShellResult<Option<process::ExitStatus>> {
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
                return Ok(None);
            }
            self.run_pipeline(&alias, stdout, stdin)?;
            return Ok(None);
        }

        let args = cmd
            .args
            .iter()
            .map(|arg| self.eval_expr(arg).map(|val| val.to_string()))
            .collect::<ShellResult<Vec<_>>>()?;

        // SAFETY: the `stdout` argument comes from something that implements IntoRawFileDescriptor,
        // which means it must be valid.
        // This is where we take ownership of the file descriptor, which will be closed when
        // `stdout` is dropped.
        let stdout = unsafe { File::from_raw_file_descriptor(stdout.into_raw_file_descriptor()) };

        if let Some(builtin) = Builtin::from_name(cmd.name.as_str()) {
            builtin.run(self, args, stdout)?;
            return Ok(None);
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

        let out = command.output().map_err(ShellError::from_command_error)?;

        Ok(Some(out.status))
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
                .ok_or_else(|| ShellError::Unbound(name.clone()))?,
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
            _ => Err(ShellError::TypeErrorInfix {
                lhs: lhs_type,
                rhs: rhs_type,
                op: infix.op,
            }),
        }
    }

    fn eval_prefix(&mut self, prefix: &PrefixExpr) -> ShellResult<Value> {
        let value = self.eval_expr(&prefix.expr)?;

        match value {
            Value::Number(n) => self.eval_numeric_prefix(prefix.op, n),
            Value::String(s) if s.parse::<f64>().is_ok() => {
                self.eval_numeric_prefix(prefix.op, s.parse().unwrap())
            }
            _ => Err(ShellError::TypeErrorPrefix {
                expr: value.type_of(),
                op: prefix.op,
            }),
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
                return Err(ShellError::TypeErrorPrefix {
                    expr: Type::Number,
                    op,
                })
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
                return Err(ShellError::TypeErrorInfix {
                    lhs: Type::String,
                    rhs: Type::String,
                    op,
                })
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
                return Err(ShellError::TypeErrorInfix {
                    lhs: Type::String,
                    rhs: Type::Number,
                    op,
                })
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
                return Err(ShellError::TypeErrorInfix {
                    lhs: Type::Number,
                    rhs: Type::String,
                    op,
                })
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

    fn set_status(&mut self, exit_status: Option<process::ExitStatus>) {
        self.last_status = exit_status.and_then(|s| s.code()).unwrap_or(0);
    }
}

impl Default for Shell {
    fn default() -> Self {
        Self::new()
    }
}
