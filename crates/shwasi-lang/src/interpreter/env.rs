#![allow(dead_code)]

use std::collections::HashMap;

use shwasi_engine::{Instance, Store, WasmFuncUntyped};
use smol_str::SmolStr;

use crate::{ast::Pipeline, interpreter::value::Value, Ident};

#[derive(Debug, Default)]
pub struct Env {
    env: HashMap<Ident, Value>,
    aliases: HashMap<SmolStr, Pipeline>,
    store: Store,
    modules: Vec<Instance>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
            aliases: HashMap::new(),
            modules: Vec::new(),
            store: Store::default(),
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

    pub fn register_module(&mut self, instance: Instance) {
        self.modules.push(instance);
    }

    pub fn unload_modules(&mut self) -> usize {
        let len = self.modules.len();
        self.modules.clear();
        self.store.clear();
        len
    }

    pub fn get_module_func(&self, name: &str) -> Option<WasmFuncUntyped> {
        self.modules
            .iter()
            .find_map(|m| m.get_func_untyped(&self.store, name).ok())
    }

    pub fn store(&self) -> &Store {
        &self.store
    }

    pub fn store_mut(&mut self) -> &mut Store {
        &mut self.store
    }
}
