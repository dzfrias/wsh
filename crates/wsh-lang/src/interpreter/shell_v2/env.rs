use std::collections::HashMap;

use crate::{shell_v2::value::Value, v2::ast::Ident};

pub struct Env {
    vars: HashMap<Ident, Value>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    pub fn get_var(&self, ident: &Ident) -> Option<&Value> {
        self.vars.get(ident)
    }

    pub fn set_var(&mut self, ident: Ident, value: Value) {
        self.vars.insert(ident, value);
    }
}
