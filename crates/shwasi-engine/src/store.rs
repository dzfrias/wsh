#![allow(dead_code)]

use std::{collections::HashMap, fmt, rc::Rc};

use shwasi_parser::{Code, FuncType, GlobalType, Memory, RefType, TableType};

use crate::instance::Instance;

#[derive(Debug, Default)]
pub struct Store {
    pub functions: Vec<FuncInst>,
    pub tables: Vec<TableInst>,
    pub memories: Vec<MemInst>,
    pub globals: Vec<GlobalInst>,
    pub elems: Vec<ElemInst>,
    pub datas: Vec<DataInst>,

    pub types: HashMap<ExternVal, Extern>,
}

impl Store {
    /// Create a new, empty [`Store`].
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Debug)]
pub enum FuncInst {
    Host(HostFunc),
    Module(ModuleFunc),
}

#[derive(Debug)]
pub struct HostFunc {
    pub ty: FuncType,
    pub code: HostFuncInner,
}

// TODO: fill this in
pub type HostFuncInner = fn();

#[derive(Debug)]
pub struct ModuleFunc {
    pub ty: FuncType,
    pub body: Code,
    pub inst: Rc<Instance>,
}

#[derive(Debug)]
pub struct TableInst {
    pub ty: TableType,
    pub elements: Vec<Option<Ref>>,
}

#[derive(Debug)]
pub struct MemInst {
    pub ty: Memory,
    pub data: Vec<u8>,
}

#[derive(Debug)]
pub struct GlobalInst {
    pub ty: GlobalType,
    pub value: Value,
}

#[derive(Debug)]
pub struct ElemInst {
    pub ty: RefType,
    pub elems: Vec<Ref>,
}

#[derive(Debug)]
pub struct ExportInst {
    pub name: String,
    pub reference: ExternVal,
}

#[derive(Debug)]
pub struct DataInst(pub Vec<u8>);

/// An address into a [`Store`].
pub type Addr = usize;
/// A reference
pub type Ref = Addr;

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

/// A WebAssembly value.
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Value {
    I32(u32),
    I64(u64),
    F32(f32),
    F64(f64),
    NullRef(RefType),
    /// A reference to a function.
    Ref(Ref),
}

impl Value {
    /// Attempt to convert the [`Value`] into a [`u32`].
    ///
    /// This function will return [`Option::None`] if it is not of the variant [`Value::I32`].
    pub fn to_u32(self) -> Option<u32> {
        match self {
            Self::I32(i32) => Some(i32),
            _ => None,
        }
    }

    /// Attempt to convert the [`Value`] into a [`Ref`].
    ///
    /// This function will return [`Option::None`] if it is not of the variant [`Value::Ref`].
    pub fn to_ref(self) -> Option<Ref> {
        match self {
            Self::Ref(ref_) => Some(ref_),
            _ => None,
        }
    }
}

impl ElemInst {
    pub fn drop(&mut self) {
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
