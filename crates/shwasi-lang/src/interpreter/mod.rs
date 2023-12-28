mod builtins;
mod env;
mod error;
mod value;

use crate::{
    ast::{InfixOp, Pipeline, PrefixOp},
    interpreter::{builtins::Builtin, env::Env},
    parser::ast::{Ast, Command, Expr, InfixExpr, PrefixExpr, Stmt},
};
pub use error::*;
use smol_str::SmolStr;
pub use value::*;

#[derive(Default)]
pub struct Interpreter {
    #[allow(dead_code)]
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
        }
    }

    fn eval_pipeline(
        &mut self,
        Pipeline(commands): &Pipeline,
        capture: bool,
    ) -> RuntimeResult<Value> {
        let popen_error = |e| match e {
            subprocess::PopenError::IoError(e) => RuntimeError::CommandFailed(e),
            e => unreachable!("shouldn't have other popen error: {e}"),
        };

        if commands.len() == 1 {
            let exec = self.make_exec(&commands[0])?;
            if let Some(exec) = exec {
                if capture {
                    let out = exec.capture().map_err(popen_error)?;
                    self.set_status(out.exit_status);
                    return Ok(Value::String(out.stdout_str().trim().into()));
                } else {
                    let status = exec.join().map_err(popen_error)?;
                    self.set_status(status);
                }
            } else {
                self.last_status = 0;
                if capture {
                    return Ok(Value::String("".into()));
                }
            }
            return Ok(Value::Null);
        }
        let pipeline = commands
            .iter()
            .filter_map(|cmd| self.make_exec(cmd).transpose())
            .collect::<RuntimeResult<Vec<_>>>()?;
        let pipeline = subprocess::Pipeline::from_exec_iter(pipeline);
        let result = if capture {
            let out = pipeline.capture().map_err(popen_error)?;
            self.set_status(out.exit_status);
            Value::String(out.stdout_str().trim().into())
        } else {
            let status = pipeline.join().map_err(popen_error)?;
            self.set_status(status);
            Value::Null
        };

        Ok(result)
    }

    fn set_status(&mut self, exit_status: subprocess::ExitStatus) {
        self.last_status = match exit_status {
            subprocess::ExitStatus::Exited(i) => i as i32,
            subprocess::ExitStatus::Signaled(i) => i as i32,
            subprocess::ExitStatus::Other(i) => i,
            subprocess::ExitStatus::Undetermined => 0,
        };
    }

    fn make_exec(
        &mut self,
        Command { name, args }: &Command,
    ) -> RuntimeResult<Option<subprocess::Exec>> {
        let args = args
            .iter()
            .map(|arg| self.eval_expr(arg).map(|val| val.to_string()))
            .collect::<RuntimeResult<Vec<_>>>()?;
        if let Some(builtin) = Builtin::from_name(name.as_str()) {
            builtin.run(&args)?;
            return Ok(None);
        }
        let exec = subprocess::Exec::cmd(name.as_str()).args(&args);
        Ok(Some(exec))
    }

    fn eval_expr(&mut self, expr: &Expr) -> RuntimeResult<Value> {
        Ok(match expr {
            Expr::Ident(_) => todo!(),
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
}
