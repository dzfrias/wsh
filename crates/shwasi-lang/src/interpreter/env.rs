use std::collections::HashMap;

use shwasi_engine::{Instance, Store, WasmFuncUntyped};
use shwasi_wasi::WasiCtx;
use smol_str::SmolStr;

use crate::{ast::Pipeline, interpreter::value::Value, Ident};

pub struct Env {
    env: HashMap<Ident, Value>,
    aliases: HashMap<SmolStr, Pipeline>,
    #[allow(dead_code)]
    wasi_ctx: WasiCtx,
    store: Store,
    modules: Vec<Instance>,
}

impl Env {
    pub fn new() -> Self {
        let mut store = Store::default();
        let mut wasi_ctx = shwasi_wasi::WasiCtxBuilder::new().inherit_stdio().build();
        // Link the WASI preview 1 snapshot into the store, so we can use it in our modules.
        shwasi_wasi::sync::snapshots::preview_1::link(&mut store, &mut wasi_ctx);
        Self {
            env: HashMap::new(),
            aliases: HashMap::new(),
            modules: Vec::new(),
            wasi_ctx,
            store,
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

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}
