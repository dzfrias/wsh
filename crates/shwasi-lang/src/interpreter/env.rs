#![allow(dead_code)]

use std::collections::HashMap;

use crate::{interpreter::value::Value, parser::Symbol};

#[derive(Debug, Default)]
pub struct Env {
    env: HashMap<Symbol, Value>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
        }
    }

    pub fn get(&self, sym: Symbol) -> Option<&Value> {
        self.env.get(&sym)
    }

    pub fn set(&mut self, sym: Symbol, value: Value) {
        self.env.insert(sym, value);
    }
}
