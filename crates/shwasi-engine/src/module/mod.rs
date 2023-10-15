#![allow(non_upper_case_globals)]

use num_enum::TryFromPrimitive;

mod instr;
mod parser;

pub use instr::*;
pub use parser::Parser;

/// The type that is being [imported](`Import`) or [exported](`Export`).
#[derive(Debug, Clone, PartialEq, Copy, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum ExternalKind {
    /// Function external.
    Function = 0x00,
    /// Table external.
    Table = 0x01,
    /// Memory external.
    Memory = 0x02,
    /// Global external.
    Global = 0x03,
}

/// The type that is being [imported](`Import`).
#[derive(Debug, PartialEq, Clone)]
pub enum ImportKind {
    /// Function import.
    Function(FuncIdx),
    /// Table import.
    Table(TableType),
    /// Memory import.
    Memory(Memory),
    /// Global import.
    Global(GlobalType),
}

/// A reference type.
///
/// Note that the gc proposal is not supported.
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum RefType {
    /// A function refrence.
    Func = 0x70,
    /// An external refrence.
    Extern = 0x6F,
}

/// A number type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum NumType {
    /// A 32-bit signed integer.
    I32 = 0x7F,
    /// A 64-bit signed integer.
    I64 = 0x7E,
    /// A 32-bit IEEE-754 number.
    F32 = 0x7D,
    /// A 64-bit IEEE-754 number.
    F64 = 0x7C,
}

/// A global type.
///
/// Declared in the `globals` section of the [`Module`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalType {
    /// The type of the global.
    pub content_type: ValType,
    /// Determines whether the global is mutable or not.
    pub mutable: bool,
}

/// A value type.
///
/// Encompasses both [`NumType`] and [`RefType`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum ValType {
    /// A 32-bit signed integer.
    I32 = 0x7F,
    /// A 64-bit signed integer.
    I64 = 0x7E,
    /// A 32-bit IEEE-754 number.
    F32 = 0x7D,
    /// A 64-bit IEEE-754 number.
    F64 = 0x7C,
    /// A function refrence.
    Func = 0x70,
    /// An external refrence.
    Extern = 0x6F,
}

/// A function type.
#[derive(Debug, Clone)]
pub struct FuncType(
    /// The parameters of the function.
    pub Vec<ValType>,
    /// The return values of the function.
    pub Vec<ValType>,
);

/// An import.
#[derive(Debug, Clone, PartialEq)]
pub struct Import<'a> {
    /// The module being imported from.
    pub module: &'a str,
    /// The export name of the module.
    pub field: &'a str,
    /// The type of the import.
    pub kind: ImportKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    /// The index of the [`FuncType`] of the function.
    pub index: TypeIdx,
}

/// The type of a table.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TableType {
    /// The elements that the table contains.
    pub elem_type: RefType,
    /// The size of the table.
    pub limit: Limit,
}

/// A potentially unbounded integer interval.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Limit {
    /// The left side of the interval
    pub initial: u32,
    /// The maximum value in the interval.
    pub max: Option<u32>,
}

/// The type of a memory block.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Memory {
    /// The size of the memory block.
    pub limit: Limit,
}

/// A global.
#[derive(Debug, Clone, PartialEq)]
pub struct Global {
    /// The global's type.
    pub kind: GlobalType,
    /// The initialization expression of the global.
    pub init: InitExpr,
}

/// An exported field of a module.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Export<'a> {
    /// The field name.
    pub field: &'a str,
    /// The kind of the field.
    pub kind: ExternalKind,
    /// The field's index (based on kind).
    pub external_idx: u32,
}

/// The start section of a [`Module`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Start {
    /// The function that the start section refrences.
    pub func_idx: u32,
}

/// An element.
#[derive(Debug, PartialEq, Clone)]
pub struct Element {
    /// The element kind.
    pub kind: ElementKind,
    /// The type that the element refrences.
    pub types: RefType,
    /// The initial elements of the segment.
    pub elems: ElementItems,
}

/// The kind of an [`Element`].
#[derive(Debug, Clone, PartialEq)]
pub enum ElementKind {
    /// The element is passive.
    Passive,
    /// The element is declarative.
    Declarative,
    /// The element is active.
    Active {
        /// The table index of the element.
        tbl_idx: TableIdx,
        /// The offset into the table.
        offset: InitExpr,
    },
}

/// The items of an [element segment](Element).
#[derive(Debug, Clone, PartialEq)]
pub enum ElementItems {
    /// Function indices.
    Functions(Vec<FuncIdx>),
    /// Initialization expressions for element items.
    Elems(Vec<InitExpr>),
}

/// A code segment.
#[derive(Debug, Clone)]
pub struct Code {
    /// The local types that the code segment contains.
    pub locals: Vec<NumLocals>,
    /// The instructions.
    pub body: InstrBuffer,
}

/// A data segment.
#[derive(Debug, Clone, PartialEq)]
pub struct Data<'a> {
    /// The index into memory of the data.
    pub index: MemIdx,
    /// The offset of the data in memory.
    pub offset: Option<InitExpr>,
    /// The actual data.
    pub data: &'a [u8],
}

/// An initialization instruction.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum InitExpr {
    /// A constant i32.
    I32Const(u32),
    /// A constant i64.
    I64Const(u64),
    /// A constant f32.
    F32Const(u32),
    /// A constant i64.
    F64Const(u64),
    /// The index of a global.
    ConstGlobalGet(u32),
}

/// Represents the number of locals of a given type.
///
/// Used in the [`Code`] section
#[derive(Debug, Clone, PartialEq)]
pub struct NumLocals {
    /// The number of locals.
    pub num: u32,
    /// The type of each local.
    pub locals_type: ValType,
}

pub type MemIdx = u32;
pub type TypeIdx = u32;
pub type FuncIdx = u32;
pub type TableIdx = u32;
pub type DataIdx = u32;
pub type ElemIdx = u32;

/// A WebAssembly module.
#[derive(Debug, Default, Clone)]
pub struct Module<'a> {
    pub types: Vec<FuncType>,
    pub imports: Vec<Import<'a>>,
    pub functions: Vec<Function>,
    pub tables: Vec<TableType>,
    pub memories: Vec<Memory>,
    pub globals: Vec<Global>,
    pub exports: Vec<Export<'a>>,
    pub start: Option<FuncIdx>,
    pub elements: Vec<Element>,
    pub codes: Vec<Code>,
    pub datas: Vec<Data<'a>>,
    pub data_count: Option<u32>,
}
