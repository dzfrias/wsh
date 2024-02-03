#![allow(dead_code)]

mod error;
mod pipeline;
mod value;

use crate::{
    interpreter::shell_v2::{pipeline::Stdio, value::Value},
    shell_v2::value::{Obj, ObjPtr, ValueType},
    v2::{
        ast::{
            self, Ast, Binop, BinopKind, Boolean, HomeDir, Node, NodeKind, Pipeline, Unop, UnopKind,
        },
        Parser, Source,
    },
};

pub struct Shell {
    to_free: Vec<ObjPtr>,
}

impl Shell {
    pub fn new() -> Self {
        Shell { to_free: vec![] }
    }

    pub fn run(&mut self, source: &Source) {
        let parser = Parser::new(source);
        let ast = parser.parse().expect("TODO: errors");
        for stmt in ast.root().stmts.deref(&ast).iter(&ast) {
            self.eval_stmt(&ast, stmt.deref(&ast));
        }
    }

    fn eval_stmt(&mut self, ast: &Ast, node: Node) {
        match node.kind() {
            NodeKind::Pipeline(pipeline) => self.eval_pipeline(ast, pipeline),
            s => todo!("stmt for {s:?}"),
        }
    }

    fn eval_pipeline(&mut self, ast: &Ast, pipeline: &Pipeline) {
        let cmds = pipeline.cmds.deref(ast);
        let first = cmds
            .first(ast)
            .expect("pipeline should have at least one command")
            .deref(ast);
        let cmd = self.make_cmd(ast, first.kind().as_command().unwrap());
        let mut pipeline = pipeline::Pipeline::new(cmd);
        for cmd in cmds.iter(ast).skip(1) {
            let deref = cmd.deref(ast);
            let cmd = deref.kind().as_command().unwrap();
            pipeline.pipe(self.make_cmd(ast, cmd));
        }
        let mut handle = pipeline.spawn(Stdio::default()).unwrap();
        handle.wait().unwrap();
    }

    fn eval_expr(&mut self, ast: &Ast, expr: Node) -> Value {
        match expr.kind() {
            NodeKind::String(s) => {
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
            NodeKind::EnvVar(_) => todo!(),
            NodeKind::Binop(binop) => self.eval_binop(ast, binop),
            NodeKind::Unop(unop) => self.eval_unop(ast, unop),
            NodeKind::LastStatus(_) => todo!(),
            NodeKind::HomeDir(h) => self.eval_home_dir(ast, h),
            NodeKind::Capture(_) => todo!(),
            _ => unreachable!("should not be called on non-expr"),
        }
    }

    fn eval_binop(&mut self, ast: &Ast, binop: &Binop) -> Value {
        let lhs = self.eval_expr(ast, binop.lhs.deref(ast));
        let rhs = self.eval_expr(ast, binop.rhs.deref(ast));
        // SAFETY: types are being checked before casting
        unsafe {
            match (lhs.type_(), rhs.type_()) {
                (ValueType::Number, ValueType::Number) => {
                    self.eval_numeric_binop(binop.op, lhs.as_num(), rhs.as_num())
                }
                (ValueType::Number, ValueType::Object)
                    if rhs.as_obj().coerce_number().is_some() =>
                {
                    self.eval_numeric_binop(
                        binop.op,
                        lhs.as_num(),
                        rhs.as_obj().coerce_number().unwrap(),
                    )
                }
                (ValueType::Object, ValueType::Object) => {
                    self.eval_object_binop(binop.op, lhs.as_obj(), rhs.as_obj())
                }
                (ValueType::Boolean, ValueType::Boolean) => {
                    self.eval_bool_binop(binop.op, lhs.as_bool(), rhs.as_bool())
                }
                // TODO: string and number infix op
                _ => todo!(),
            }
        }
    }

    fn eval_unop(&mut self, ast: &Ast, unop: &Unop) -> Value {
        let expr = self.eval_expr(ast, unop.expr.deref(ast));
        // SAFETY: types are being checked before casting
        unsafe {
            match expr.type_() {
                ValueType::Number => self.eval_numeric_unop(unop.op, expr.as_num()),
                ValueType::Boolean => self.eval_bool_unop(unop.op, expr.as_bool()),
                ValueType::Object if expr.as_obj().coerce_number().is_some() => {
                    self.eval_numeric_unop(unop.op, expr.as_obj().coerce_number().unwrap())
                }
                _ => todo!(),
            }
        }
    }

    fn eval_numeric_unop(&mut self, op: UnopKind, f: f64) -> Value {
        match op {
            UnopKind::Neg => Value::number(-f),
            UnopKind::Sign => Value::number(f),
            UnopKind::Bang => todo!("error"),
        }
    }

    fn eval_object_binop(&mut self, op: BinopKind, lhs: ObjPtr, rhs: ObjPtr) -> Value {
        match (&*lhs, &*rhs) {
            (Obj::String(s), Obj::String(u)) => match op {
                BinopKind::Add => {
                    let ptr = s.concat(u);
                    self.to_free.push(ptr);
                    Value::obj(ptr)
                }
                BinopKind::Eq => Value::bool(s == u),
                BinopKind::Ne => Value::bool(s != u),
                _ => todo!("error"),
            },
        }
    }

    fn eval_bool_unop(&mut self, op: UnopKind, b: bool) -> Value {
        match op {
            UnopKind::Neg => Value::bool(!b),
            _ => todo!("error"),
        }
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

    fn eval_bool_binop(&mut self, op: BinopKind, lhs: bool, rhs: bool) -> Value {
        match op {
            BinopKind::Eq => Value::bool(lhs == rhs),
            BinopKind::Ne => Value::bool(lhs != rhs),
            _ => todo!("error"),
        }
    }

    fn eval_home_dir(&mut self, _ast: &Ast, _home_dir: &HomeDir) -> Value {
        let path = dirs::home_dir().unwrap().into_boxed_path();
        // It's okay to leak the box here because we clean it up later
        let home = Box::leak(path).to_str().unwrap();
        let ptr = Obj::string(home);
        self.to_free.push(ptr);
        Value::obj(ptr)
    }

    fn make_cmd(&mut self, ast: &Ast, cmd: &ast::Command) -> pipeline::Command {
        let exprs = cmd.exprs.deref(ast);
        let cmd = self
            .eval_expr(ast, exprs.first(ast).unwrap().deref(ast))
            .to_string();
        pipeline::Command::new(&cmd).args(
            exprs
                .iter(ast)
                .skip(1)
                .map(|expr| self.eval_expr(ast, expr.deref(ast)).to_string()),
        )
    }
}

impl Drop for Shell {
    fn drop(&mut self) {
        for val in &self.to_free {
            unsafe { val.free() }
        }
    }
}
