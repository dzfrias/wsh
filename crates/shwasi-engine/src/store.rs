#![allow(dead_code)]

use std::{collections::HashMap, fmt, rc::Rc};

use shwasi_parser::{Code, FuncType, GlobalType, Memory, RefType, TableType};

use crate::{
    instance::Instance,
    value::{Ref, Value},
    PAGE_SIZE,
};

/// A WebAssembly store, holding all global data of given module.
#[derive(Debug, Default)]
pub struct Store<'a> {
    pub data: StoreData<'a>,
    pub mut_: StoreMut,
}

#[derive(Debug, Default)]
pub struct StoreData<'a> {
    pub functions: Vec<FuncInst>,
    pub datas: Vec<DataInst<'a>>,

    pub(crate) types: HashMap<ExternVal, Extern>,
}

#[derive(Debug, Default)]
pub struct StoreMut {
    pub memories: Vec<MemInst>,
    pub globals: Vec<GlobalInst>,
    pub elems: Vec<ElemInst>,
    pub tables: Vec<TableInst>,
}

impl Store<'_> {
    /// Create a new, empty [`Store`].
    pub fn new() -> Self {
        Self::default()
    }
}

/// An instance of a WebAssembly function.
#[derive(Debug)]
pub enum FuncInst {
    Host(HostFunc),
    Module(ModuleFunc),
}

/// A function defined outside of the WebAssembly module, imported into the store.
pub struct HostFunc {
    pub ty: FuncType,
    pub code: HostFuncInner,
}

/// The inner function type of a host function.
///
/// See [`HostFunc`] for more information.
// TODO: fill this in
pub type HostFuncInner = Box<dyn Fn()>;

/// A function defined inside of the WebAssembly module.
#[derive(Debug)]
pub struct ModuleFunc {
    pub ty: FuncType,
    pub code: Code,
    pub inst: Rc<Instance>,
}

/// An instance of a WebAssembly table.
#[derive(Debug)]
pub struct TableInst {
    pub ty: TableType,
    pub elements: Vec<Option<Ref>>,
}

/// An instance of WebAssembly memory.
#[derive(Debug)]
pub struct MemInst {
    pub ty: Memory,
    pub data: Vec<u8>,
}

impl MemInst {
    /// Get the size of the memory in pages.
    #[inline]
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
        Some(sz)
    }
}

impl TableInst {
    #[inline]
    pub fn size(&self) -> usize {
        self.elements.len()
    }
}

/// An instance of a WebAssembly global.
#[derive(Debug)]
pub struct GlobalInst {
    pub ty: GlobalType,
    pub value: Value,
}

/// An instance of a WebAssembly element.
#[derive(Debug)]
pub struct ElemInst {
    pub ty: RefType,
    pub elems: Vec<Ref>,
}

/// An instance of field exported from a WebAssembly module.
#[derive(Debug)]
pub struct ExportInst {
    pub name: String,
    pub reference: ExternVal,
}

/// An instance of a WebAssembly data segment.
#[derive(Debug)]
pub struct DataInst<'a>(pub &'a [u8]);

/// An address into a [`Store`].
pub type Addr = usize;

/// A reference to a value in the store, but not in the module.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash, Copy)]
pub enum ExternVal {
    Func(Addr),
    Table(Addr),
    Mem(Addr),
    Global(Addr),
}

/// A value that can be imported or exported.
#[derive(Debug, Clone, PartialEq)]
pub enum Extern {
    Func(FuncType),
    Table(TableType),
    Mem(Memory),
    Global(GlobalType),
}

impl ElemInst {
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
