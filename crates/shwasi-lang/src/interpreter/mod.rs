mod builtins;
mod env;
mod error;
mod executor;
mod value;

use std::process;

use crate::{
    ast::{InfixOp, PrefixOp},
    interpreter::{builtins::Builtin, env::Env},
    parser::ast::{Ast, Command, Expr, InfixExpr, PrefixExpr, Stmt},
};
pub use error::*;
pub use executor::Executor;
pub use value::*;

#[derive(Default)]
pub struct Interpreter {
    #[allow(dead_code)]
    env: Env,
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
            Stmt::Command(cmd) => self.eval_cmd(cmd),
            Stmt::Expr(expr) => self.eval_expr(expr),
        }
    }

    fn eval_cmd(&mut self, Command { name, args }: &Command) -> RuntimeResult<Value> {
        let args = args
            .iter()
            .map(|arg| self.eval_expr(arg).map(|val| val.to_string()))
            .collect::<RuntimeResult<Vec<_>>>()?;

        if let Some(builtin) = Builtin::from_name(name.as_str()) {
            builtin.run(&args)?;
            return Ok(Value::Null);
        }

        process::Command::new(name.as_str())
            .args(args)
            .spawn()
            .map_err(RuntimeError::CommandFailed)?
            .wait()
            .map_err(RuntimeError::CommandFailed)?;

        Ok(Value::Null)
    }

    fn eval_expr(&mut self, expr: &Expr) -> RuntimeResult<Value> {
        Ok(match expr {
            Expr::Ident(_) => todo!(),
            Expr::Infix(infix) => self.eval_infix(infix)?,
            Expr::Prefix(prefix) => self.eval_prefix(prefix)?,
            Expr::String(s) => Value::String(s.clone()),
            Expr::Number(n) => Value::Number(*n),
        })
    }

    fn eval_infix(&mut self, infix: &InfixExpr) -> RuntimeResult<Value> {
        let lhs = self.eval_expr(&infix.lhs)?;
        let rhs = self.eval_expr(&infix.rhs)?;

        match (lhs, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => self.eval_numeric_infix(infix.op, lhs, rhs),
            _ => todo!("other infix exprs"),
        }
    }

    fn eval_prefix(&mut self, prefix: &PrefixExpr) -> RuntimeResult<Value> {
        let value = self.eval_expr(&prefix.expr)?;

        match value {
            Value::Number(n) => self.eval_numeric_prefix(prefix.op, n),
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
}
