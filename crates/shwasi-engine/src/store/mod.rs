mod store_field;

use std::{collections::HashMap, fmt};

use shwasi_parser::{Code, FuncType, GlobalType, Limit, MemoryType, TableType};

use crate::{
    instance::Instance,
    value::{Ref, Value, ValueUntyped},
    vm::Vm,
    IntoHostFunc, Trap, PAGE_SIZE,
};
pub(crate) use store_field::*;

/// A WebAssembly store, holding all global data of given module.
#[derive(Debug, Default)]
pub struct Store {
    pub(crate) functions: StoreField<Func>,
    pub(crate) datas: StoreField<Data>,
    pub(crate) memories: StoreField<Memory>,
    pub(crate) globals: StoreField<Global>,
    pub(crate) elems: StoreField<Element>,
    pub(crate) tables: StoreField<Table>,

    pub(crate) instances: HashMap<String, Instance>,
    pub(crate) hosts: HashMap<(String, String), ExternVal>,
}

impl Store {
    /// Create a new, empty [`Store`].
    pub fn new() -> Self {
        Self::default()
    }

    /// Drop all items in the store.
    ///
    /// This will clear all items in the store, but will not free the store allocations themselves.
    /// The allocations can be used for future items. As such, it is useful for reusing memory.
    pub fn clear(&mut self) {
        self.functions.clear();
        self.datas.clear();
        self.memories.clear();
        self.globals.clear();
        self.elems.clear();
        self.tables.clear();
        self.instances.clear();
        self.hosts.clear();
    }

    pub fn define(&mut self, module: &str, field: &str, val: impl HostValue) {
        let extern_val = val.store(self);
        self.hosts
            .insert((module.to_owned(), field.to_owned()), extern_val);
    }

    pub(crate) fn resolve(&self, module: &str, field: &str) -> Option<ExternVal> {
        self.resolve_host(module, field)
            .or_else(|| self.resolve_module(module, field))
    }

    fn resolve_host(&self, module: &str, field: &str) -> Option<ExternVal> {
        self.hosts
            .get(&(module.to_owned(), field.to_owned()))
            .copied()
    }

    fn resolve_module(&self, module: &str, field: &str) -> Option<ExternVal> {
        let instance = self.instances.get(module)?;
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

/// An instance of a WebAssembly function.
#[derive(Debug)]
pub(crate) enum Func {
    Host(HostFunc),
    Module(ModuleFunc),
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
    ty: TableType,
    elements: Vec<Ref>,
}

impl Table {
    pub fn new(ty: TableType) -> Self {
        Self {
            elements: vec![None; ty.limit.initial as usize],
            ty,
        }
    }

    pub fn grow(&mut self, grow_by: u32, init: Ref) -> Option<u32> {
        let old_size = self.size();
        // We use u64 here to avoid potential overflow
        let new_size = self.size() as u64 + grow_by as u64;
        if let Some(max) = self.ty.limit.max {
            if max < new_size as u32 {
                return None;
            }
        }
        if new_size >= 2u64.pow(32) {
            return None;
        }
        self.elements.resize(new_size as usize, init);
        self.ty.limit.initial = new_size as u32;
        Some(old_size)
    }

    pub fn get(&self, idx: u32) -> Result<Ref, Trap> {
        self.elements
            .get(idx as usize)
            .copied()
            .ok_or(Trap::TableGetOutOfBounds {
                table_size: self.size(),
                index: idx,
            })
    }

    pub fn set(&mut self, idx: u32, val: Ref) -> Result<(), Trap> {
        self.elements
            .get_mut(idx as usize)
            .map(|e| *e = val)
            .ok_or(Trap::TableGetOutOfBounds {
                table_size: self.size(),
                index: idx,
            })
    }

    pub fn fill(&mut self, start: u32, len: u32, val: Ref) -> Result<(), Trap> {
        if start.saturating_add(len) > self.size() {
            return Err(Trap::TableGetOutOfBounds {
                table_size: self.size(),
                index: start.saturating_add(len),
            });
        }
        self.elements[start as usize..(start + len) as usize].fill(val);
        Ok(())
    }

    pub fn copy(&mut self, other: &Table, dst: u32, src: u32, len: u32) -> Result<(), Trap> {
        if src.saturating_add(len) > other.size() {
            return Err(Trap::TableGetOutOfBounds {
                index: src.saturating_add(len),
                table_size: other.size(),
            });
        }
        if dst.saturating_add(len) > self.size() {
            return Err(Trap::TableGetOutOfBounds {
                index: dst.saturating_add(len),
                table_size: self.size(),
            });
        }

        self.elements[dst as usize..(dst + len) as usize]
            .copy_from_slice(&other.elements[src as usize..(src + len) as usize]);

        Ok(())
    }

    pub fn copy_within(&mut self, dst: u32, src: u32, len: u32) -> Result<(), Trap> {
        if src.saturating_add(len) > self.size() {
            return Err(Trap::TableGetOutOfBounds {
                index: src.saturating_add(len),
                table_size: self.size(),
            });
        }
        if dst.saturating_add(len) > self.size() {
            return Err(Trap::TableGetOutOfBounds {
                index: dst.saturating_add(len),
                table_size: self.size(),
            });
        }

        self.elements
            .copy_within(src as usize..(src + len) as usize, dst as usize);

        Ok(())
    }

    pub fn init(&mut self, elem: &Element, dst: u32, src: u32, len: u32) -> Result<(), Trap> {
        if src.saturating_add(len) > elem.elems.len() as u32 {
            return Err(Trap::TableGetOutOfBounds {
                index: src.saturating_add(len),
                table_size: elem.elems.len() as u32,
            });
        }
        if dst.saturating_add(len) > self.size() {
            return Err(Trap::TableGetOutOfBounds {
                index: dst.saturating_add(len),
                table_size: self.size(),
            });
        }

        self.elements[dst as usize..(dst + len) as usize]
            .copy_from_slice(&elem.elems[src as usize..(src + len) as usize]);

        Ok(())
    }

    pub fn size(&self) -> u32 {
        self.elements.len() as u32
    }

    pub fn is_empty(&self) -> bool {
        self.size() == 0
    }

    pub fn ty(&self) -> &TableType {
        &self.ty
    }

    pub fn elements(&self) -> &[Ref] {
        self.elements.as_ref()
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

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
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
pub struct Element {
    pub elems: Vec<Ref>,
}

/// An instance of a field exported from a WebAssembly module.
#[derive(Debug)]
pub struct Export {
    pub name: String,
    pub(crate) reference: ExternVal,
}

/// An instance of a WebAssembly data segment.
#[derive(Debug)]
pub struct Data(pub Vec<u8>);

impl Data {
    pub fn data_drop(&mut self) {
        self.0.clear();
    }
}

/// A reference to a value in the store, but not in the module.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash, Copy)]
pub(crate) enum ExternVal {
    Func(Addr<Func>),
    Table(Addr<Table>),
    Mem(Addr<Memory>),
    Global(Addr<Global>),
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

pub trait HostValue {
    #[doc(hidden)]
    #[allow(private_interfaces)]
    fn store(self, store: &mut Store) -> ExternVal;
}

impl HostValue for HostFunc {
    #[allow(private_interfaces)]
    fn store(self, store: &mut Store) -> ExternVal {
        ExternVal::Func(store.functions.alloc(Func::Host(self)))
    }
}

impl HostValue for Global {
    #[allow(private_interfaces)]
    fn store(self, store: &mut Store) -> ExternVal {
        let addr = store.globals.alloc(Global {
            mutable: self.mutable,
            value: self.value,
        });
        ExternVal::Global(addr)
    }
}

impl HostValue for Memory {
    #[allow(private_interfaces)]
    fn store(self, store: &mut Store) -> ExternVal {
        ExternVal::Mem(store.memories.alloc(self))
    }
}

impl HostValue for Table {
    #[allow(private_interfaces)]
    fn store(self, store: &mut Store) -> ExternVal {
        ExternVal::Table(store.tables.alloc(self))
    }
}

impl Element {
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
