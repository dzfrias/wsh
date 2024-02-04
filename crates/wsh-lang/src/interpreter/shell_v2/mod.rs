mod builtins;
pub mod error;
mod pipeline;
mod value;

#[cfg(unix)]
use std::os::unix::process::ExitStatusExt;
#[cfg(windows)]
use std::os::windows::process::ExitStatusExt;
use std::{fs, io};

use self::error::Result;
use crate::{
    interpreter::shell_v2::{pipeline::Stdio, value::Value},
    shell_v2::{
        builtins::Builtin,
        error::{Error, ErrorKind, WithPosition},
        value::{Obj, ObjPtr, ValueType},
    },
    v2::{
        ast::{
            self, Ast, Binop, BinopKind, Boolean, EnvVarHandle, HomeDir, Node, NodeKind, Pipeline,
            PipelineEndKind, Unop, UnopKind,
        },
        Parser, Source,
    },
};

pub struct Shell {
    to_free: Vec<ObjPtr>,
    current_pos: usize,
    last_status: i32,
}

impl Shell {
    pub fn new() -> Self {
        Shell {
            to_free: vec![],
            current_pos: 0,
            last_status: 0,
        }
    }

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

    fn eval_stmt(&mut self, ast: &Ast, node: Node) -> Result<()> {
        self.current_pos = node.offset();
        match node.kind() {
            NodeKind::Pipeline(pipeline) => self.eval_pipeline(ast, pipeline),
            s => todo!("stmt for {s:?}"),
        }
    }

    fn eval_pipeline(&mut self, ast: &Ast, pipeline: &Pipeline) -> Result<()> {
        let cmds = pipeline.cmds.deref(ast);
        let first = cmds
            .first(ast)
            .expect("pipeline should have at least one command")
            .deref(ast);
        let pos = self.current_pos;
        let cmd = self.make_cmd(ast, first.kind().as_command().unwrap())?;
        let mut exec = cmds.iter(ast).skip(1).try_fold(
            pipeline::Pipeline::new(cmd),
            |mut pipeline, cmd| {
                let deref = cmd.deref(ast);
                let cmd = deref.kind().as_command().unwrap();
                pipeline.pipe(self.make_cmd(ast, cmd)?);
                Ok(pipeline)
            },
        )?;
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
            None => pipeline::stdout()
                .map_err(ErrorKind::CommandFailedToStart)
                .with_position(pos)?,
        };
        let stdio = Stdio {
            stdout,
            stderr: pipeline::stderr()
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

    fn eval_expr(&mut self, ast: &Ast, expr: Node) -> Result<Value> {
        self.current_pos = expr.offset();
        let val = match expr.kind() {
            NodeKind::String(s) => {
                // Will this result in an invalid ptr deref when ast is dropped?
                let ptr = Obj::string(s.deref(ast));
                self.to_free.push(ptr);
                Value::obj(ptr)
            }
            NodeKind::Number(f) => Value::number(f.0),
            NodeKind::Boolean(b) => match b {
                Boolean::True => Value::TRUE,
                Boolean::False => Value::FALSE,
            },
            NodeKind::Ident(_) => todo!(),
            NodeKind::EnvVar(env_var) => self.eval_env_var(ast, *env_var),
            NodeKind::Binop(binop) => self.eval_binop(ast, binop)?,
            NodeKind::Unop(unop) => self.eval_unop(ast, unop)?,
            NodeKind::LastStatus(_) => Value::number(self.last_status as f64),
            NodeKind::HomeDir(h) => self.eval_home_dir(ast, h)?,
            NodeKind::Capture(_) => todo!(),
            _ => unreachable!("should not be called on non-expr"),
        };
        Ok(val)
    }

    fn eval_binop(&mut self, ast: &Ast, binop: &Binop) -> Result<Value> {
        let lhs = self.eval_expr(ast, binop.lhs.deref(ast))?;
        let rhs = self.eval_expr(ast, binop.rhs.deref(ast))?;
        // SAFETY: types are being checked before casting
        let val = unsafe {
            match (lhs.type_(), rhs.type_()) {
                (ValueType::Number, ValueType::Number) => {
                    self.eval_numeric_binop(binop.op, lhs.as_num(), rhs.as_num())
                }
                (ValueType::Number, ValueType::Object(_))
                    if rhs.as_obj().coerce_number().is_some() =>
                {
                    self.eval_numeric_binop(
                        binop.op,
                        lhs.as_num(),
                        rhs.as_obj().coerce_number().unwrap(),
                    )
                }
                (ValueType::Object(_), ValueType::Object(_)) => {
                    self.eval_object_binop(binop.op, lhs.as_obj(), rhs.as_obj())?
                }
                (ValueType::Boolean, ValueType::Boolean) => {
                    self.eval_bool_binop(binop.op, lhs.as_bool(), rhs.as_bool())?
                }
                // TODO: string and number infix op
                _ => {
                    return Err(self.error(ErrorKind::BinopTypeError {
                        op: binop.op,
                        lhs: lhs.type_(),
                        rhs: rhs.type_(),
                    }))
                }
            }
        };
        Ok(val)
    }

    fn eval_unop(&mut self, ast: &Ast, unop: &Unop) -> Result<Value> {
        let expr = self.eval_expr(ast, unop.expr.deref(ast))?;
        // SAFETY: types are being checked before casting
        let val = unsafe {
            match expr.type_() {
                ValueType::Number => self.eval_numeric_unop(unop.op, expr.as_num())?,
                ValueType::Boolean => self.eval_bool_unop(unop.op, expr.as_bool())?,
                ValueType::Object(_) if expr.as_obj().coerce_number().is_some() => {
                    self.eval_numeric_unop(unop.op, expr.as_obj().coerce_number().unwrap())?
                }
                _ => {
                    return Err(self.error(ErrorKind::UnopTypeError {
                        op: unop.op,
                        type_: expr.type_(),
                    }))
                }
            }
        };
        Ok(val)
    }

    fn eval_numeric_unop(&mut self, op: UnopKind, f: f64) -> Result<Value> {
        let val = match op {
            UnopKind::Neg => Value::number(-f),
            UnopKind::Sign => Value::number(f),
            UnopKind::Bang => {
                return Err(self.error(ErrorKind::UnopTypeError {
                    op,
                    type_: ValueType::Number,
                }))
            }
        };
        Ok(val)
    }

    fn eval_object_binop(&mut self, op: BinopKind, lhs: ObjPtr, rhs: ObjPtr) -> Result<Value> {
        let val = match (&*lhs, &*rhs) {
            (Obj::String(s), Obj::String(u)) => match op {
                BinopKind::Add => {
                    let ptr = s.concat(u);
                    self.to_free.push(ptr);
                    Value::obj(ptr)
                }
                BinopKind::Eq => Value::bool(s == u),
                BinopKind::Ne => Value::bool(s != u),
                _ => {
                    return Err(self.error(ErrorKind::BinopTypeError {
                        op,
                        lhs: ValueType::Object(lhs.type_()),
                        rhs: ValueType::Object(rhs.type_()),
                    }))
                }
            },
        };
        Ok(val)
    }

    fn eval_bool_unop(&mut self, op: UnopKind, b: bool) -> Result<Value> {
        let val = match op {
            UnopKind::Neg => Value::bool(!b),
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
            BinopKind::Add => Value::number(lhs + rhs),
            BinopKind::Sub => Value::number(lhs - rhs),
            BinopKind::Div => Value::number(lhs / rhs),
            BinopKind::Mul => Value::number(lhs * rhs),
            BinopKind::Lt => Value::bool(lhs < rhs),
            BinopKind::Gt => Value::bool(lhs > rhs),
            BinopKind::Le => Value::bool(lhs <= rhs),
            BinopKind::Ge => Value::bool(lhs >= rhs),
            BinopKind::Eq => Value::bool(lhs == rhs),
            BinopKind::Ne => Value::bool(lhs != rhs),
        }
    }

    fn eval_bool_binop(&mut self, op: BinopKind, lhs: bool, rhs: bool) -> Result<Value> {
        let val = match op {
            BinopKind::Eq => Value::bool(lhs == rhs),
            BinopKind::Ne => Value::bool(lhs != rhs),
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
            .with_position(self.current_pos)?
            .into_boxed_path();
        // It's okay to leak the box here because we clean it up later
        let home = Box::leak(path)
            .to_str()
            .ok_or(ErrorKind::HomeDirInvalidUTF8)
            .with_position(self.current_pos)?;
        let ptr = Obj::string(home);
        self.to_free.push(ptr);
        Ok(Value::obj(ptr))
    }

    fn make_cmd(&mut self, ast: &Ast, cmd: &ast::Command) -> Result<pipeline::Command<Self>> {
        let exprs = cmd.exprs.deref(ast);
        let name = self
            .eval_expr(ast, exprs.first(ast).unwrap().deref(ast))?
            .to_string();
        let args = exprs
            .iter(ast)
            .skip(1)
            .map(|expr| self.eval_expr(ast, expr.deref(ast)).map(|e| e.to_string()))
            .collect::<Result<Vec<_>>>()?;
        if let Some(builtin) = Builtin::from_name(&name) {
            return Ok(pipeline::Command::from(
                move |shell: &mut Shell, stdio: Stdio| builtin.run(shell, stdio, &args),
            ));
        }
        Ok(pipeline::Command::Basic(
            pipeline::BasicCommand::new(&name)
                .args(args)
                .merge_stderr(cmd.merge_stderr),
        ))
    }

    fn eval_env_var(&mut self, ast: &Ast, env_var: EnvVarHandle) -> Value {
        let env_var = env_var.deref(ast);
        let value = std::env::var(&**env_var)
            .unwrap_or_default()
            .into_boxed_str();
        let ptr = Obj::string(Box::leak(value));
        self.to_free.push(ptr);
        Value::obj(ptr)
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

impl Drop for Shell {
    fn drop(&mut self) {
        for val in &self.to_free {
            unsafe { val.free() }
        }
    }
}
