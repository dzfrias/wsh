use std::collections::HashMap;

use crate::{
    shell_v2::value::Value,
    v2::ast::{Ast, Ident},
};

pub struct Env {
    vars: HashMap<Ident, Value>,
    aliases: HashMap<String, Ast>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            aliases: HashMap::new(),
        }
    }

    pub fn get_alias(&mut self, ident: &str) -> Option<Ast> {
        self.aliases.remove(ident)
    }

    pub fn set_alias(&mut self, ident: String, ast: Ast) {
        self.aliases.insert(ident, ast);
    }

    pub fn get_var(&self, ident: &Ident) -> Option<&Value> {
        self.vars.get(ident)
    }

    pub fn set_var(&mut self, ident: Ident, value: Value) {
        self.vars.insert(ident, value);
    }
}
