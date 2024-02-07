mod builtins;
mod env;
pub mod error;
mod pipeline;
mod value;

#[cfg(unix)]
use std::os::unix::process::ExitStatusExt;
#[cfg(windows)]
use std::os::windows::process::ExitStatusExt;
use std::{
    borrow::Cow,
    fs::{self, File},
    io::{self, Read, Write},
};

use filedescriptor::{AsRawFileDescriptor, FromRawFileDescriptor, IntoRawFileDescriptor};
use itertools::Either;

use self::error::Result;
use crate::{
    shell_v2::{
        builtins::Builtin,
        env::{Env, WasiContext},
        error::{Error, ErrorKind, WithPosition},
        pipeline::Stdio,
        value::{Value, ValueType},
    },
    v2::{
        ast::{
            self, Alias, Assignment, Ast, Binop, BinopKind, Boolean, Capture, EnvVarHandle, Export,
            HomeDir, Node, NodeKind, Pipeline, PipelineEndKind, Unop, UnopKind,
        },
        Parser, Source,
    },
};

/// The main interpreter of `wsh` source code.
pub struct Shell {
    /// The global environment of the shell.
    env: Env,

    /// The current byte offset of the shell in the source code.
    current_pos: usize,
    /// The exit status code of the last command run.
    last_status: i32,
    /// The global stdout of the shell. Note that while the `File` API describes an object that may
    /// be read from, the provided file will **never** be read from, meaning pipe ends may be used.
    global_stdout: File,
    global_stderr: File,
}

impl Shell {
    /// Create a new `Shell`.
    pub fn new() -> Self {
        Shell {
            current_pos: 0,
            last_status: 0,
            env: Env::new(),
            // SAFETY: io::stdout() is a valid raw file descriptor.
            global_stdout: unsafe {
                File::from_raw_file_descriptor(io::stdout().as_raw_file_descriptor())
            },
            // SAFETY: io::stderr() is a valid raw file descriptor.
            global_stderr: unsafe {
                File::from_raw_file_descriptor(io::stderr().as_raw_file_descriptor())
            },
        }
    }

    /// Run the shell with the given source code. Note that this may be used in a REPL or file
    /// context.
    pub fn run(&mut self, source: &Source) -> Result<()> {
        let parser = Parser::new(source);
        let ast = parser
            .parse()
            .map_err(|err| Error::new(ErrorKind::ParseError(err), 0))?;
        for stmt in ast.root().stmts.deref(&ast).iter(&ast) {
            self.eval_stmt(&ast, stmt.deref(&ast))?;
        }
        Ok(())
    }

    /// Set the global stdout of the shell to a File, returning the previous value.
    pub fn set_stdout(&mut self, stdout: File) -> File {
        std::mem::replace(&mut self.global_stdout, stdout)
    }

    /// Set the global stderr of the shell to a File, returning the previous value.
    pub fn set_stderr(&mut self, stdout: File) -> File {
        std::mem::replace(&mut self.global_stderr, stdout)
    }

    fn eval_stmt(&mut self, ast: &Ast, node: Node) -> Result<()> {
        self.current_pos = node.offset();
        match node.kind() {
            NodeKind::Pipeline(pipeline) => self.eval_pipeline(ast, pipeline),
            NodeKind::Export(export) => self.eval_export(ast, export),
            NodeKind::Assignment(assignment) => self.eval_assignment(ast, assignment),
            NodeKind::Alias(alias) => self.eval_alias(ast, alias),
            s => todo!("stmt for {s:?}"),
        }
    }

    fn eval_pipeline(&mut self, ast: &Ast, pipeline: &Pipeline) -> Result<()> {
        let pos = self.current_pos;
        // Enough information exists for this to be possible in `make_pipeline`, but we do it here
        // so the lifetime of this Vec outlives the lifetime of the Pipeline returned from
        // `make_pipeline`. This is so we don't have to clone the environment all the time.
        let env = pipeline
            .env
            .deref(ast)
            .iter(ast)
            .map(|env_set| {
                self.eval_expr(ast, env_set.value.deref(ast))
                    .map(|value| (env_set.name.deref(ast).to_string(), value.to_string()))
            })
            .collect::<Result<Vec<_>>>()?;
        let mut exec = self.make_pipeline(ast, pipeline, &env)?;
        // Configure stdout to the pipeline redirect, if any (defaults to shell stdout)
        let stdout = match &pipeline.end {
            Some(end) => {
                let mut opts = fs::OpenOptions::new();
                match end.kind {
                    PipelineEndKind::Write => opts.write(true).truncate(true).create(true),
                    PipelineEndKind::Append => opts.append(true).create(true),
                };
                let file = self.eval_expr(ast, end.file.deref(ast))?;
                opts.open(file.to_string())
                    .map_err(ErrorKind::BadRedirect)
                    .with_position(pos)?
            }
            None => self
                .global_stdout
                .try_clone()
                .map_err(ErrorKind::CommandFailedToStart)
                .with_position(pos)?,
        };
        let stdio = Stdio {
            stdout,
            stderr: self
                .global_stderr
                .try_clone()
                .map_err(ErrorKind::CommandFailedToStart)
                .with_position(pos)?,
            stdin: None,
        };
        let mut handle = exec
            .spawn(self, stdio)
            .map_err(|err| {
                self.last_status = if err.kind() == io::ErrorKind::NotFound {
                    127
                } else {
                    1
                };
                ErrorKind::CommandFailedToStart(err)
            })
            .with_position(pos)?;
        let exit_status = handle
            .wait(self)
            .map_err(ErrorKind::CommandFailed)
            .with_position(pos)?;
        #[allow(clippy::unnecessary_cast)]
        let exit_status = exit_status.into_raw() as i32;
        self.last_status = exit_status;
        Ok(())
    }

    fn eval_export(&mut self, ast: &Ast, export: &Export) -> Result<()> {
        let value = self.eval_expr(ast, export.value.deref(ast))?.to_string();
        std::env::set_var(&**export.name.deref(ast), value);
        Ok(())
    }

    fn eval_assignment(&mut self, ast: &Ast, assignment: &Assignment) -> Result<()> {
        let value = self.eval_expr(ast, assignment.value.deref(ast))?;
        self.env.set_var(assignment.name.deref(ast).clone(), value);
        Ok(())
    }

    fn eval_alias(&mut self, ast: &Ast, alias: &Alias) -> Result<()> {
        let name = alias.name.deref(ast).clone();
        let subtree = alias.pipeline.deref(ast).clone();
        self.env.set_alias(name.to_string(), subtree);
        Ok(())
    }

    fn eval_expr(&mut self, ast: &Ast, expr: Node) -> Result<Value> {
        self.current_pos = expr.offset();
        let val = match expr.kind() {
            NodeKind::String(s) => Value::String(s.deref(ast).to_string().into_boxed_str()),
            NodeKind::Number(f) => Value::Number(f.0),
            NodeKind::Boolean(b) => match b {
                Boolean::True => Value::Boolean(true),
                Boolean::False => Value::Boolean(false),
            },
            NodeKind::Ident(i) => self
                .env
                .get_var(i.deref(ast))
                .cloned()
                .ok_or_else(|| ErrorKind::UnboundVariable(i.deref(ast).clone()))
                .with_position(self.current_pos)?,
            NodeKind::EnvVar(env_var) => self.eval_env_var(ast, *env_var),
            NodeKind::Binop(binop) => self.eval_binop(ast, binop)?,
            NodeKind::Unop(unop) => self.eval_unop(ast, unop)?,
            NodeKind::LastStatus(_) => Value::Number(self.last_status as f64),
            NodeKind::HomeDir(h) => self.eval_home_dir(ast, h)?,
            NodeKind::Capture(capture) => self.eval_capture(ast, capture)?,
            _ => unreachable!("should not be called on non-expr"),
        };
        Ok(val)
    }

    fn eval_binop(&mut self, ast: &Ast, binop: &Binop) -> Result<Value> {
        let lhs = self.eval_expr(ast, binop.lhs.deref(ast))?;
        let rhs = self.eval_expr(ast, binop.rhs.deref(ast))?;
        let (lhs_ty, rhs_ty) = (lhs.type_(), rhs.type_());
        let val = match (lhs, rhs) {
            (Value::Number(a), Value::Number(b)) => self.eval_numeric_binop(binop.op, a, b),
            (Value::Number(a), Value::String(s)) if s.parse::<f64>().is_ok() => {
                self.eval_numeric_binop(binop.op, a, s.parse().unwrap())
            }
            (Value::String(s1), Value::String(s2)) => self.eval_object_binop(binop.op, s1, s2)?,
            (Value::Boolean(b1), Value::Boolean(b2)) => self.eval_bool_binop(binop.op, b1, b2)?,
            // TODO: string and number infix op
            _ => {
                return Err(self.error(ErrorKind::BinopTypeError {
                    op: binop.op,
                    lhs: lhs_ty,
                    rhs: rhs_ty,
                }))
            }
        };
        Ok(val)
    }

    fn eval_unop(&mut self, ast: &Ast, unop: &Unop) -> Result<Value> {
        let expr = self.eval_expr(ast, unop.expr.deref(ast))?;
        let val = match expr {
            Value::Number(n) => self.eval_numeric_unop(unop.op, n)?,
            Value::Boolean(b) => self.eval_bool_unop(unop.op, b)?,
            Value::String(s) if s.parse::<f64>().is_ok() => {
                self.eval_numeric_unop(unop.op, s.parse::<f64>().unwrap())?
            }
            Value::String(_) => {
                return Err(self.error(ErrorKind::UnopTypeError {
                    op: unop.op,
                    type_: ValueType::String,
                }))
            }
        };
        Ok(val)
    }

    fn eval_capture(&mut self, ast: &Ast, capture: &Capture) -> Result<Value> {
        let (mut reader, writer) = os_pipe::pipe()
            .map_err(ErrorKind::CommandFailedToStart)
            .with_position(self.current_pos)?;
        // We create a thread to do reads here so that we always make progress on io. Deadlocks
        // might occur when pipes gets completely filled, and this thread makes sure that doesn't
        // happen.
        let handle = std::thread::spawn(move || -> io::Result<Vec<u8>> {
            let mut output = vec![];
            reader.read_to_end(&mut output)?;
            Ok(output)
        });
        // SAFETY: the write end is a valid file descriptor, and it can be written to
        let old = self.set_stdout(unsafe {
            File::from_raw_file_descriptor(writer.into_raw_file_descriptor())
        });
        self.eval_pipeline(
            ast,
            capture.pipeline.deref(ast).kind().as_pipeline().unwrap(),
        )?;
        // This will implicitly drop the write end of the pipe (which was `global_stdout`),
        // allowing the pipe to be read from without blocking.
        self.set_stdout(old);
        let out = handle
            .join()
            .expect("reader thread should not panic")
            .map_err(ErrorKind::CaptureError)
            .with_position(self.current_pos)?;
        // This little routine here will prevent us from cloning the byte output if it's already
        // UTF-8 encoded.
        let mut out = match String::from_utf8_lossy(&out) {
            // SAFETY: we just validated that the bytes can be losslessly converted to a String
            Cow::Borrowed(_) => unsafe { String::from_utf8_unchecked(out) },
            Cow::Owned(new) => new,
        };
        while out.ends_with('\n') || out.ends_with('\r') {
            out.truncate(out.len() - 1);
        }
        Ok(Value::String(out.into_boxed_str()))
    }

    fn eval_numeric_unop(&mut self, op: UnopKind, f: f64) -> Result<Value> {
        let val = match op {
            UnopKind::Neg => Value::Number(-f),
            UnopKind::Sign => Value::Number(f),
            UnopKind::Bang => {
                return Err(self.error(ErrorKind::UnopTypeError {
                    op,
                    type_: ValueType::Number,
                }))
            }
        };
        Ok(val)
    }

    fn eval_object_binop(&mut self, op: BinopKind, lhs: Box<str>, rhs: Box<str>) -> Result<Value> {
        let val = match op {
            BinopKind::Add => Value::String(format!("{lhs}{rhs}").into_boxed_str()),
            BinopKind::Eq => Value::Boolean(lhs == rhs),
            BinopKind::Ne => Value::Boolean(lhs != rhs),
            _ => {
                return Err(self.error(ErrorKind::BinopTypeError {
                    op,
                    lhs: ValueType::String,
                    rhs: ValueType::String,
                }))
            }
        };
        Ok(val)
    }

    fn eval_bool_unop(&mut self, op: UnopKind, b: bool) -> Result<Value> {
        let val = match op {
            UnopKind::Neg => Value::Boolean(!b),
            _ => {
                return Err(self.error(ErrorKind::UnopTypeError {
                    op,
                    type_: ValueType::Boolean,
                }))
            }
        };
        Ok(val)
    }

    fn eval_numeric_binop(&mut self, op: BinopKind, lhs: f64, rhs: f64) -> Value {
        match op {
            BinopKind::Add => Value::Number(lhs + rhs),
            BinopKind::Sub => Value::Number(lhs - rhs),
            BinopKind::Div => Value::Number(lhs / rhs),
            BinopKind::Mul => Value::Number(lhs * rhs),
            BinopKind::Lt => Value::Boolean(lhs < rhs),
            BinopKind::Gt => Value::Boolean(lhs > rhs),
            BinopKind::Le => Value::Boolean(lhs <= rhs),
            BinopKind::Ge => Value::Boolean(lhs >= rhs),
            BinopKind::Eq => Value::Boolean(lhs == rhs),
            BinopKind::Ne => Value::Boolean(lhs != rhs),
        }
    }

    fn eval_bool_binop(&mut self, op: BinopKind, lhs: bool, rhs: bool) -> Result<Value> {
        let val = match op {
            BinopKind::Eq => Value::Boolean(lhs == rhs),
            BinopKind::Ne => Value::Boolean(lhs != rhs),
            _ => {
                return Err(self.error(ErrorKind::BinopTypeError {
                    op,
                    lhs: ValueType::Boolean,
                    rhs: ValueType::Boolean,
                }))
            }
        };
        Ok(val)
    }

    fn eval_home_dir(&mut self, _ast: &Ast, _home_dir: &HomeDir) -> Result<Value> {
        let path = dirs::home_dir()
            .ok_or(ErrorKind::HomeDirNotFound)
            .with_position(self.current_pos)?;
        let home = path
            .to_str()
            .ok_or(ErrorKind::HomeDirInvalidUTF8)
            .with_position(self.current_pos)?;
        Ok(Value::String(home.into()))
    }

    /// Create a pipeline to execute. The `make_*` familly of methods all contribute to pieces of
    /// this pipeline.
    fn make_pipeline<'env>(
        &mut self,
        ast: &Ast,
        pipeline: &Pipeline,
        env: &'env [(String, String)],
    ) -> Result<pipeline::Pipeline<'env, Shell>> {
        let cmds = pipeline.cmds.deref(ast);
        let first = cmds.first(ast).unwrap().deref(ast);
        let cmd = self.make_cmd(ast, first.kind().as_command().unwrap(), env)?;
        let exec = cmds.iter(ast).skip(1).try_fold(
            match cmd {
                Either::Left(cmd) => pipeline::Pipeline::new(cmd),
                Either::Right(pipeline) => pipeline,
            },
            |mut pipeline, cmd| {
                let deref = cmd.deref(ast);
                let cmd = deref.kind().as_command().unwrap();
                match self.make_cmd(ast, cmd, env)? {
                    Either::Left(cmd) => pipeline.pipe(cmd),
                    Either::Right(other) => pipeline.extend(other),
                }
                Ok(pipeline)
            },
        )?;
        Ok(exec)
    }

    fn make_cmd<'env>(
        &mut self,
        ast: &Ast,
        cmd: &ast::Command,
        env: &'env [(String, String)],
    ) -> Result<Either<pipeline::Command<'env, Self>, pipeline::Pipeline<'env, Self>>> {
        let exprs = cmd.exprs.deref(ast);
        let name = self
            .eval_expr(ast, exprs.first(ast).unwrap().deref(ast))?
            .to_string();

        // First, we check if the command is an alias
        if let Some(sub_ast) = self.env.get_alias(&name) {
            let args = exprs
                .iter(ast)
                .skip(1)
                .map(|expr| self.eval_expr(ast, expr.deref(ast)).map(|e| e.to_string()))
                .collect::<Result<Vec<_>>>()?;
            let node = sub_ast.first().unwrap();
            let pipeline = node.kind().as_pipeline().unwrap();
            let mut pipeline = self.make_pipeline(&sub_ast, pipeline, env)?;
            // We append this command's args to the end of the aliased pipeline
            pipeline.append_args(&args);
            self.env.set_alias(name, sub_ast);
            return Ok(Either::Right(pipeline));
        }

        // Next, we check if the function was loaded from Wasm
        if let Some(func) = self.env.get_module_func(&name) {
            // Note that these args have **not** be converted to strings yet
            let args = exprs
                .iter(ast)
                .skip(1)
                .map(|expr| self.eval_expr(ast, expr.deref(ast)))
                .collect::<Result<Vec<_>>>()?;
            return Ok(Either::Left(self.make_wasm_func(func, args, env)?));
        }

        let args = exprs
            .iter(ast)
            .skip(1)
            .map(|expr| self.eval_expr(ast, expr.deref(ast)).map(|e| e.to_string()))
            .collect::<Result<Vec<_>>>()?;

        // We check if the command is a built-in
        if let Some(builtin) = Builtin::from_name(&name) {
            let merge_stderr = cmd.merge_stderr;
            return Ok(Either::Left(pipeline::Command::from(
                pipeline::Closure::wrap(
                    move |shell: &mut Shell,
                          mut stdio: Stdio,
                          args: &[String],
                          env: &[(String, String)]| {
                        if merge_stderr {
                            stdio.stderr = match stdio.stdout.try_clone() {
                                Ok(file) => file,
                                Err(err) => {
                                    writeln!(
                                        stdio.stderr,
                                        "error cloning stdout to merge with stderr: {err}"
                                    )
                                    .expect("write to stderr failed!");
                                    return 1;
                                }
                            };
                        }
                        builtin.run(shell, stdio, args, env)
                    },
                    args,
                    env,
                ),
            )));
        }

        // If nothing else, it's a regular command
        Ok(Either::Left(pipeline::Command::Basic(
            pipeline::BasicCommand::new(&name)
                .env(env)
                .args(args)
                .merge_stderr(cmd.merge_stderr),
        )))
    }

    fn make_wasm_func<'env>(
        &mut self,
        func: wsh_engine::WasmFuncUntyped,
        args: Vec<Value>,
        env: &'env [(String, String)],
    ) -> Result<pipeline::Command<'env, Self>> {
        let expect_len = func.arg_types(self.env.store()).len();
        if expect_len != args.len() {
            return Err(self.error(ErrorKind::WasmArgLenMismatch {
                want: expect_len,
                got: args.len(),
            }));
        }

        // Converts wsh `Value`s to Wasm `Value`s
        let wasm_args = args
            .into_iter()
            .zip(func.arg_types(self.env.store()))
            .enumerate()
            .map(|(i, (value, expect_ty))| -> Result<_> {
                let value = match value {
                    Value::Number(n) => n,
                    Value::String(s) => s.parse::<f64>().map_err(|_err| {
                        self.error(ErrorKind::BadWasmArg {
                            idx: i,
                            reason: "cannot pass string to Wasm",
                        })
                    })?,
                    Value::Boolean(b) => b as u8 as f64,
                };
                let val = match expect_ty {
                    wsh_parser::ValType::I32 => wsh_engine::Value::I32(value as u32),
                    wsh_parser::ValType::I64 => wsh_engine::Value::I64(value as u64),
                    wsh_parser::ValType::F32 => wsh_engine::Value::F32(value as f32),
                    wsh_parser::ValType::F64 => wsh_engine::Value::F64(value),
                    _ => {
                        return Err(self.error(ErrorKind::BadWasmArg {
                            idx: i,
                            reason: "cannot call Wasm function that takes in references",
                        }))
                    }
                };
                Ok(val)
            })
            .collect::<Result<Vec<_>>>()?;
        // This closure will call the Wasm function
        Ok(pipeline::Command::Closure(pipeline::Closure::wrap(
            move |shell: &mut Shell,
                  mut stdio: Stdio,
                  _args: &[String],
                  env: &[(String, String)]| {
                if let Err(err) = shell.env.prepare_wasi(
                    WasiContext::new(&stdio.stdout, &stdio.stderr, stdio.stdin.as_ref()).env(env),
                ) {
                    writeln!(stdio.stderr, "error preparing WASI: {err}")
                        .expect("write to stderr failed");
                    return 1;
                }
                let ret_vals = match func.call(shell.env.store_mut(), &wasm_args) {
                    Ok(vals) => vals,
                    Err(err) => {
                        writeln!(stdio.stderr, "Wasm function failed: {err}")
                            .expect("write to stderr failed");
                        return 1;
                    }
                };
                for ret_val in ret_vals {
                    writeln!(stdio.stdout, "{ret_val}").expect("write to stdout failed");
                }
                0
            },
            vec![],
            env,
        )))
    }

    fn eval_env_var(&mut self, ast: &Ast, env_var: EnvVarHandle) -> Value {
        let env_var = env_var.deref(ast);
        let value = std::env::var(&**env_var).unwrap_or_default();
        Value::String(value.into_boxed_str())
    }

    fn error(&self, kind: ErrorKind) -> Error {
        Error::new(kind, self.current_pos)
    }
}

impl Default for Shell {
    fn default() -> Self {
        Self::new()
    }
}
