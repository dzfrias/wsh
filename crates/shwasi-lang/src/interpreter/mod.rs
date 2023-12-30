mod builtins;
mod env;
mod error;
mod value;

use crate::{
    ast::{AliasAssign, Assign, InfixOp, Pipeline, PrefixOp},
    interpreter::{builtins::Builtin, env::Env},
    parser::ast::{Ast, Command, Expr, InfixExpr, PrefixExpr, Stmt},
};
pub use error::*;
use smol_str::SmolStr;
use std::process;
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
        self.env.set_alias(name.clone(), pipeline.clone());
        Ok(())
    }

    fn eval_assign(&mut self, Assign { name, expr }: &Assign) -> RuntimeResult<()> {
        let val = self.eval_expr(expr)?;
        self.env.set(name.clone(), val);

        Ok(())
    }

    fn eval_expr(&mut self, expr: &Expr) -> RuntimeResult<Value> {
        Ok(match expr {
            Expr::Ident(name) => self.env.get(name).cloned().expect("TODO: error"),
            Expr::Infix(infix) => self.eval_infix(infix)?,
            Expr::Prefix(prefix) => self.eval_prefix(prefix)?,
            Expr::String(s) => Value::String(s.clone()),
            Expr::Number(n) => Value::Number(*n),
            Expr::Pipeline(pipeline) => self.eval_pipeline(pipeline, /*capture =*/ true)?,
            Expr::LastStatus => Value::Number(self.last_status as f64),
        })
    }

    fn eval_infix(&mut self, infix: &InfixExpr) -> RuntimeResult<Value> {
        let lhs = self.eval_expr(&infix.lhs)?;
        let rhs = self.eval_expr(&infix.rhs)?;

        match (lhs, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => self.eval_numeric_infix(infix.op, lhs, rhs),
            (Value::String(lhs), Value::String(rhs)) => self.eval_string_infix(infix.op, lhs, rhs),
            (Value::String(lhs), Value::Number(rhs)) => {
                self.eval_string_numeric_infix(infix.op, lhs, rhs)
            }
            (Value::Number(lhs), Value::String(rhs)) => {
                self.eval_numeric_string_infix(infix.op, lhs, rhs)
            }
            _ => todo!("error"),
        }
    }

    fn eval_prefix(&mut self, prefix: &PrefixExpr) -> RuntimeResult<Value> {
        let value = self.eval_expr(&prefix.expr)?;

        match value {
            Value::Number(n) => self.eval_numeric_prefix(prefix.op, n),
            Value::String(s) if s.parse::<f64>().is_ok() => {
                self.eval_numeric_prefix(prefix.op, s.parse().unwrap())
            }
            _ => todo!("error"),
        }
    }

    fn eval_numeric_infix(&mut self, op: InfixOp, lhs: f64, rhs: f64) -> RuntimeResult<Value> {
        let result = match op {
            InfixOp::Add => lhs + rhs,
            InfixOp::Sub => lhs - rhs,
            InfixOp::Mul => lhs * rhs,
            InfixOp::Div => lhs / rhs,
        };

        Ok(Value::Number(result))
    }

    fn eval_numeric_prefix(&mut self, op: PrefixOp, n: f64) -> RuntimeResult<Value> {
        let result = match op {
            PrefixOp::Sign => n,
            PrefixOp::Neg => -n,
            PrefixOp::Bang => todo!("error"),
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
            InfixOp::Add => format!("{lhs}{rhs}").into(),
            _ => todo!("error"),
        };

        Ok(Value::String(result))
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
            _ => todo!("error"),
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
            _ => todo!("error"),
        };

        Ok(Value::String(result.into()))
    }

    fn make_pipeline(&mut self, pipeline: &Pipeline) -> RuntimeResult<Option<duct::Expression>> {
        let mut expression = self.make_exec(&pipeline.0[0])?;
        for cmd in pipeline.0.iter().skip(1) {
            let Some(exec) = self.make_exec(cmd)? else {
                continue;
            };
            expression = expression.map(|expr| expr.pipe(&exec)).or(Some(exec));
        }

        Ok(expression)
    }

    fn make_exec(
        &mut self,
        Command { name, args }: &Command,
    ) -> RuntimeResult<Option<duct::Expression>> {
        if let Some(alias) = self.env.get_alias(name.as_str()) {
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
