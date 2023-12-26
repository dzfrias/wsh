use std::{collections::HashMap, fmt};

use crate::parser::lexer::Ident;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(u32);

#[derive(Default, Clone)]
pub struct SymbolTable {
    symbols: HashMap<Ident, Symbol>,
    current_id: u32,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn intern(&mut self, ident: Ident) -> Symbol {
        if let Some(&sym) = self.symbols.get(&ident) {
            return sym;
        }

        let sym = Symbol(self.current_id);
        self.current_id += 1;
        self.symbols.insert(ident, sym);
        sym
    }
}

impl fmt::Debug for SymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut sorted = self.symbols.iter().collect::<Vec<_>>();
        sorted.sort_by_key(|(_, sym)| sym.0);
        f.debug_map().entries(sorted).finish()
    }
}
