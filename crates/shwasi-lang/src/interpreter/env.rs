#![allow(dead_code)]

use std::collections::HashMap;

use smol_str::SmolStr;

use crate::{ast::Pipeline, interpreter::value::Value, Ident};

#[derive(Debug, Default)]
pub struct Env {
    env: HashMap<Ident, Value>,
    aliases: HashMap<SmolStr, Pipeline>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
            aliases: HashMap::new(),
        }
    }

    pub fn get(&self, sym: &Ident) -> Option<&Value> {
        self.env.get(sym)
    }

    pub fn set(&mut self, sym: Ident, value: Value) {
        self.env.insert(sym, value);
    }

    pub fn set_alias(&mut self, name: SmolStr, expr: Pipeline) {
        self.aliases.insert(name, expr);
    }

    pub fn get_alias(&self, name: &str) -> Option<&Pipeline> {
        self.aliases.get(name)
    }

    pub fn remove_alias(&mut self, name: &str) -> Option<Pipeline> {
        self.aliases.remove(name)
    }
}
