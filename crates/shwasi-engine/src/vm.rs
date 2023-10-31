use shwasi_parser::InitExpr;

use crate::{
    store::{Addr, GlobalInst},
    values::Value,
};

pub fn eval_const_expr(globals: &[GlobalInst], module_globals: &[Addr], expr: &InitExpr) -> Value {
    match expr {
        InitExpr::I32Const(i32) => Value::I32(*i32),
        InitExpr::I64Const(i64) => Value::I64(*i64),
        InitExpr::F32Const(f32) => Value::F32(f32::from_bits(f32.raw())),
        InitExpr::F64Const(f64) => Value::F64(f64::from_bits(f64.raw())),
        InitExpr::ConstGlobalGet(idx) => globals[module_globals[*idx as usize]].value,
        InitExpr::RefNull(t) => Value::NullRef(*t),
        InitExpr::RefFunc(idx) => Value::Ref(*idx as usize),
    }
}
