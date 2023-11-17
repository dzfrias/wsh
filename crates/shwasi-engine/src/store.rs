use std::{collections::HashMap, fmt};

use shwasi_parser::{Code, FuncType, GlobalType, Limit, MemoryType, RefType, TableType};

use crate::{
    instance::Instance,
    value::{Ref, Value, ValueUntyped},
    vm::Vm,
    IntoHostFunc, PAGE_SIZE,
};

/// A WebAssembly store, holding all global data of given module.
#[derive(Debug, Default)]
pub struct Store {
    pub(crate) data: StoreData,
    pub(crate) mut_: StoreMut,
}

#[derive(Debug, Default)]
pub(crate) struct StoreData {
    pub functions: Vec<Func>,
    pub datas: Vec<Data>,
    pub instances: HashMap<String, Instance>,
    pub hosts: HashMap<(String, String), ExternVal>,
}

#[derive(Debug, Default)]
pub(crate) struct StoreMut {
    pub memories: Vec<Memory>,
    pub globals: Vec<Global>,
    pub elems: Vec<Element>,
    pub tables: Vec<Table>,
}

impl Store {
    /// Create a new, empty [`Store`].
    pub fn new() -> Self {
        Self::default()
    }

    /// Drop all items in the store.
    ///
    /// This will clear all items in the store, but will not free the store allocations themselves.
    /// The allocations can be used for future items.
    pub fn clear(&mut self) {
        self.data.functions.clear();
        self.data.datas.clear();
        self.mut_.memories.clear();
        self.mut_.globals.clear();
        self.mut_.elems.clear();
        self.mut_.tables.clear();
    }

    pub fn define(&mut self, module: &str, field: &str, val: impl HostValue) {
        let extern_val = val.store(self);
        self.data
            .hosts
            .insert((module.to_owned(), field.to_owned()), extern_val);
    }

    pub(crate) fn resolve(&self, module: &str, field: &str) -> Option<ExternVal> {
        self.resolve_host(module, field)
            .or_else(|| self.resolve_module(module, field))
    }

    fn resolve_host(&self, module: &str, field: &str) -> Option<ExternVal> {
        self.data
            .hosts
            .get(&(module.to_owned(), field.to_owned()))
            .copied()
    }

    fn resolve_module(&self, module: &str, field: &str) -> Option<ExternVal> {
        let instance = self.data.instances.get(module)?;
        instance
            .exports()
            .iter()
            .find_map(|Export { name, reference }| {
                if name == field {
                    Some(*reference)
                } else {
                    None
                }
            })
    }
}

/// A function defined outside of the WebAssembly module, imported into the store.
pub struct HostFunc {
    pub(crate) ty: FuncType,
    #[allow(clippy::type_complexity)]
    pub(crate) code: Box<dyn Fn(&mut Vm) -> Vec<ValueUntyped>>,
}

impl HostFunc {
    pub fn wrap<Params, Results>(f: impl IntoHostFunc<Params, Results>) -> Self {
        let host = f.into_host_func();
        Self {
            ty: host.ty,
            code: host.code,
        }
    }
}

/// An instance of a WebAssembly function.
#[derive(Debug)]
pub(crate) enum Func {
    Host(HostFunc),
    Module(ModuleFunc),
}

/// A function defined inside of the WebAssembly module.
#[derive(Debug)]
pub(crate) struct ModuleFunc {
    pub ty: FuncType,
    pub code: Code,
    pub inst: Instance,
}

/// An instance of a WebAssembly table.
#[derive(Debug)]
pub struct Table {
    pub ty: TableType,
    pub elements: Vec<Ref>,
}

impl Table {
    pub fn new(limit: Limit, ty: RefType) -> Self {
        Self {
            ty: TableType {
                limit: limit.clone(),
                elem_type: ty,
            },
            elements: vec![None; limit.initial as usize],
        }
    }

    #[inline]
    pub fn size(&self) -> usize {
        self.elements.len()
    }
}

/// An instance of WebAssembly memory.
#[derive(Debug)]
pub struct Memory {
    pub ty: MemoryType,
    pub data: Vec<u8>,
}

impl Memory {
    pub fn new(limit: Limit) -> Self {
        Self {
            ty: MemoryType {
                limit: limit.clone(),
            },
            data: vec![0; limit.initial as usize * PAGE_SIZE],
        }
    }

    /// Get the size of the memory in pages.
    pub fn size(&self) -> usize {
        self.data.len() / PAGE_SIZE
    }

    pub fn grow(&mut self, new: usize) -> Option<usize> {
        let sz = self.data.len() / PAGE_SIZE;
        if let Some(max) = self.ty.limit.max {
            if (max as usize) < sz + new {
                return None;
            }
        }
        // 4GB limit
        if sz + new > ((1u64 << 32) / PAGE_SIZE as u64) as usize {
            return None;
        }
        self.data.resize((sz + new) * PAGE_SIZE, 0);
        self.ty.limit.initial = (sz + new) as u32;
        Some(sz)
    }
}

/// An instance of a WebAssembly global.
#[derive(Debug)]
pub struct Global {
    pub value: Value,
    pub mutable: bool,
}

impl Global {
    pub fn new(value: Value, mutable: bool) -> Self {
        Self { value, mutable }
    }
}

/// An instance of a WebAssembly element.
#[derive(Debug)]
pub(crate) struct Element {
    pub elems: Vec<Ref>,
}

/// An instance of a field exported from a WebAssembly module.
#[derive(Debug)]
pub(crate) struct Export {
    pub name: String,
    pub reference: ExternVal,
}

/// An instance of a WebAssembly data segment.
#[derive(Debug)]
pub(crate) struct Data(pub Vec<u8>);

/// An address into a [`Store`].
pub(crate) type Addr = usize;

/// A reference to a value in the store, but not in the module.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash, Copy)]
pub enum ExternVal {
    Func(Addr),
    Table(Addr),
    Mem(Addr),
    Global(Addr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExternType {
    Func,
    Table,
    Memory,
    Global,
}

/// A value that can be imported or exported.
#[derive(Debug, Clone, PartialEq)]
pub enum Extern {
    Func(FuncType),
    Table(TableType),
    Mem(MemoryType),
    Global(GlobalType),
}

mod private {
    use super::*;

    pub trait Sealed {}
    impl Sealed for HostFunc {}
    impl Sealed for Global {}
    impl Sealed for Memory {}
    impl Sealed for Table {}
}

pub trait HostValue: private::Sealed {
    #[doc(hidden)]
    fn store(self, store: &mut Store) -> ExternVal;
}

impl HostValue for HostFunc {
    fn store(self, store: &mut Store) -> ExternVal {
        store.data.functions.push(Func::Host(self));
        ExternVal::Func(store.data.functions.len() - 1)
    }
}

impl HostValue for Global {
    fn store(self, store: &mut Store) -> ExternVal {
        store.mut_.globals.push(Global {
            mutable: self.mutable,
            value: self.value,
        });
        let addr = store.mut_.globals.len() - 1;
        ExternVal::Global(addr)
    }
}

impl HostValue for Memory {
    fn store(self, store: &mut Store) -> ExternVal {
        let addr = store.mut_.memories.len();
        store.mut_.memories.push(self);
        ExternVal::Mem(addr)
    }
}

impl HostValue for Table {
    fn store(self, store: &mut Store) -> ExternVal {
        let addr = store.mut_.tables.len();
        store.mut_.tables.push(self);
        ExternVal::Table(addr)
    }
}

impl Element {
    #[inline]
    pub fn elem_drop(&mut self) {
        self.elems.clear();
    }
}

impl fmt::Display for ExternVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExternVal::Func(addr) => write!(f, "func at {addr}"),
            ExternVal::Table(addr) => write!(f, "table at {addr}"),
            ExternVal::Mem(addr) => write!(f, "mem at {addr}"),
            ExternVal::Global(addr) => write!(f, "global at {addr}"),
        }
    }
}

impl fmt::Display for ExternType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExternType::Func => write!(f, "func"),
            ExternType::Table => write!(f, "table"),
            ExternType::Memory => write!(f, "memory"),
            ExternType::Global => write!(f, "global"),
        }
    }
}

impl fmt::Display for Extern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Extern::Func(ty) => write!(f, "func: {ty}"),
            Extern::Table(ty) => write!(f, "table: {ty}"),
            Extern::Mem(ty) => write!(f, "mem: {ty}"),
            Extern::Global(ty) => write!(f, "global: {ty}"),
        }
    }
}

impl fmt::Debug for HostFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("HostFunc")
            .field("ty", &self.ty)
            .field("code", &"fn(...)")
            .finish()
    }
}

impl Func {
    pub fn ty(&self) -> &FuncType {
        match self {
            Func::Host(h) => &h.ty,
            Func::Module(m) => &m.ty,
        }
    }
}

impl Extern {
    pub fn matches(&self, other: &Extern) -> bool {
        match (self, other) {
            (Extern::Func(a), Extern::Func(b)) => a.matches(b),
            (Extern::Table(a), Extern::Table(b)) => a.matches(b),
            (Extern::Mem(a), Extern::Mem(b)) => a.matches(b),
            (Extern::Global(a), Extern::Global(b)) => a.matches(b),
            _ => false,
        }
    }

    pub fn ty(&self) -> ExternType {
        match self {
            Extern::Func(_) => ExternType::Func,
            Extern::Table(_) => ExternType::Table,
            Extern::Mem(_) => ExternType::Memory,
            Extern::Global(_) => ExternType::Global,
        }
    }
}

impl ExternVal {
    pub fn ty(&self) -> ExternType {
        match self {
            ExternVal::Func(_) => ExternType::Func,
            ExternVal::Table(_) => ExternType::Table,
            ExternVal::Mem(_) => ExternType::Memory,
            ExternVal::Global(_) => ExternType::Global,
        }
    }
}
