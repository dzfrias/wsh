mod builtins;
mod env;
mod error;
mod value;

use crate::{
    ast::{AliasAssign, Assign, EnvSet, Export, InfixOp, Pipeline, PipelineEndKind, PrefixOp},
    interpreter::{builtins::Builtin, env::Env},
    parser::ast::{Ast, Command, Expr, InfixExpr, PrefixExpr, Stmt},
};
pub use error::*;
use smol_str::SmolStr;
use std::{borrow::Cow, fs::OpenOptions, process};
pub use value::*;

#[derive(Default)]
pub struct Interpreter {
    env: Env,
    last_status: i32,
}

impl Interpreter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn run(&mut self, program: Ast) -> RuntimeResult<Option<Value>> {
        let mut result = Value::Null;
        for stmt in program {
            result = self.eval_stmt(&stmt)?;
        }

        Ok((!result.is_null()).then_some(result))
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> RuntimeResult<Value> {
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

    fn eval_pipeline(&mut self, pipeline: &Pipeline, capture: bool) -> RuntimeResult<Value> {
        let expression = self.make_pipeline(pipeline)?;
        let Some(expression) = expression else {
            return Ok(if capture {
                Value::String("".into())
            } else {
                Value::Null
            });
        };
        let result = if capture {
            let out = expression
                .stdout_capture()
                .unchecked()
                .run()
                .map_err(RuntimeError::CommandFailed)?;
            self.set_status(out.status);
            Value::String(String::from_utf8_lossy(&out.stdout).trim().into())
        } else {
            let status = expression
                .unchecked()
                .run()
                .map_err(RuntimeError::CommandFailed)?
                .status;
            self.set_status(status);
            Value::Null
        };

        Ok(result)
    }

    fn eval_alias_assign(
        &mut self,
        AliasAssign { name, pipeline }: &AliasAssign,
    ) -> RuntimeResult<()> {
        // TODO: avoid clone here?
        self.env.set_alias(name.clone(), pipeline.clone());
        Ok(())
    }

    fn eval_assign(&mut self, Assign { name, expr }: &Assign) -> RuntimeResult<()> {
        let val = self.eval_expr(expr)?;
        self.env.set(name.clone(), val);

        Ok(())
    }

    fn eval_export(&mut self, export: &Export) -> RuntimeResult<()> {
        std::env::set_var(
            export.name.as_str(),
            self.eval_expr(&export.expr)?.to_string(),
        );

        Ok(())
    }

    fn eval_expr(&mut self, expr: &Expr) -> RuntimeResult<Value> {
        Ok(match expr {
            Expr::Ident(name) => self
                .env
                .get(name)
                .cloned()
                .ok_or_else(|| RuntimeError::Unbound(name.clone()))?,
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

    fn eval_infix(&mut self, infix: &InfixExpr) -> RuntimeResult<Value> {
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
            _ => Err(RuntimeError::TypeErrorInfix {
                lhs: lhs_type,
                rhs: rhs_type,
                op: infix.op,
            }),
        }
    }

    fn eval_prefix(&mut self, prefix: &PrefixExpr) -> RuntimeResult<Value> {
        let value = self.eval_expr(&prefix.expr)?;

        match value {
            Value::Number(n) => self.eval_numeric_prefix(prefix.op, n),
            Value::String(s) if s.parse::<f64>().is_ok() => {
                self.eval_numeric_prefix(prefix.op, s.parse().unwrap())
            }
            _ => Err(RuntimeError::TypeErrorPrefix {
                expr: value.type_of(),
                op: prefix.op,
            }),
        }
    }

    fn eval_numeric_infix(&mut self, op: InfixOp, lhs: f64, rhs: f64) -> RuntimeResult<Value> {
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

    fn eval_bool_infix(&mut self, op: InfixOp, lhs: bool, rhs: bool) -> RuntimeResult<Value> {
        let result = match op {
            InfixOp::Eq => Value::Bool(lhs == rhs),
            InfixOp::Ne => Value::Bool(lhs != rhs),
            _ => self.eval_string_infix(op, lhs.to_string().into(), rhs.to_string().into())?,
        };

        Ok(result)
    }

    fn eval_numeric_prefix(&mut self, op: PrefixOp, n: f64) -> RuntimeResult<Value> {
        let result = match op {
            PrefixOp::Sign => n,
            PrefixOp::Neg => -n,
            PrefixOp::Bang => {
                return Err(RuntimeError::TypeErrorPrefix {
                    expr: Type::Number,
                    op,
                })
            }
        };

        Ok(Value::Number(result))
    }

    fn eval_string_infix(
        &mut self,
        op: InfixOp,
        lhs: SmolStr,
        rhs: SmolStr,
    ) -> RuntimeResult<Value> {
        let result = match op {
            InfixOp::Add => Value::String(format!("{lhs}{rhs}").into()),
            InfixOp::Eq => Value::Bool(lhs == rhs),
            InfixOp::Ne => Value::Bool(lhs != rhs),
            _ => {
                return Err(RuntimeError::TypeErrorInfix {
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
    ) -> RuntimeResult<Value> {
        if let Ok(lhs) = lhs.parse::<f64>() {
            return self.eval_numeric_infix(op, lhs, rhs);
        }

        let result = match op {
            InfixOp::Add => format!("{lhs}{rhs}"),
            InfixOp::Mul => lhs.repeat(rhs as usize),
            _ => {
                return Err(RuntimeError::TypeErrorInfix {
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
    ) -> RuntimeResult<Value> {
        if let Ok(rhs) = rhs.parse::<f64>() {
            return self.eval_numeric_infix(op, lhs, rhs);
        }

        let result = match op {
            InfixOp::Add => format!("{lhs}{rhs}"),
            InfixOp::Mul => rhs.repeat(lhs as usize),
            _ => {
                return Err(RuntimeError::TypeErrorInfix {
                    lhs: Type::Number,
                    rhs: Type::String,
                    op,
                })
            }
        };

        Ok(Value::String(result.into()))
    }

    fn eval_env_var(&mut self, name: &SmolStr) -> RuntimeResult<Value> {
        let var = std::env::var_os(name.as_str()).unwrap_or_default();
        let val = var.to_string_lossy();
        Ok(match val {
            // We know that if this is the borrwed variant, it's a valid UTF-8 string, so
            // there's no need for any cloning.
            Cow::Borrowed(_) => Value::String(var.into_string().unwrap().into()),
            Cow::Owned(new) => Value::String(new.into()),
        })
    }

    fn make_pipeline(&mut self, pipeline: &Pipeline) -> RuntimeResult<Option<duct::Expression>> {
        let mut expression = self.make_exec(&pipeline.commands[0])?;
        for cmd in pipeline.commands.iter().skip(1) {
            let Some(exec) = self.make_exec(cmd)? else {
                continue;
            };
            expression = expression.map(|expr| expr.pipe(&exec)).or(Some(exec));
        }
        if let Some(exec) = expression.clone() {
            for EnvSet { name, expr } in &pipeline.env {
                let val = self.eval_expr(expr)?;
                expression = Some(exec.env(name.as_str(), val.to_string()));
            }
        }
        if let Some(write) = pipeline.write.as_ref() {
            let write_to = self.eval_expr(&write.expr)?;
            expression = expression
                .map(|expr| match write.kind {
                    PipelineEndKind::Append => {
                        let file = OpenOptions::new()
                            .append(true)
                            .create(true)
                            .open(write_to.to_string())
                            .map_err(RuntimeError::CommandFailed)?;
                        Ok(expr.stdout_file(file))
                    }
                    PipelineEndKind::Write => Ok(expr.stdout_path(write_to.to_string())),
                })
                .transpose()?;
        }

        Ok(expression)
    }

    fn make_exec(
        &mut self,
        Command { name, args }: &Command,
    ) -> RuntimeResult<Option<duct::Expression>> {
        // TODO: avoid clone here?
        if let Some(mut alias) = self.env.get_alias(name).cloned() {
            alias
                .commands
                .last_mut()
                .unwrap()
                .args
                .extend_from_slice(args);
            // Recursive alias found
            if alias.commands.iter().any(|cmd| &cmd.name == name) {
                let pipeline = self.env.remove_alias(name).unwrap();
                let exec = self.make_pipeline(&alias)?;
                self.env.set_alias(name.clone(), pipeline);
                return Ok(exec);
            }
            let exec = self.make_pipeline(&alias)?;
            return Ok(exec);
        }

        let args = args
            .iter()
            .map(|arg| self.eval_expr(arg).map(|val| val.to_string()))
            .collect::<RuntimeResult<Vec<_>>>()?;

        if let Some(builtin) = Builtin::from_name(name.as_str()) {
            builtin.run(&args)?;
            return Ok(None);
        }
        let exec = duct::cmd(name.as_str(), args);
        Ok(Some(exec))
    }

    fn set_status(&mut self, exit_status: process::ExitStatus) {
        if let Some(code) = exit_status.code() {
            self.last_status = code;
        }
    }
}
