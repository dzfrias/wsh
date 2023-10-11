use wasmparser::Operator;

#[derive(Debug, Clone, PartialEq, Copy, Hash)]
pub enum ExternalKind {
    Function,
    Table,
    Memory,
    Global,
}

#[derive(Debug, Clone)]
pub enum RefType {
    Func,
    Extern,
}

#[derive(Debug, Clone, PartialEq, Copy, Hash)]
pub enum ElemType {
    Anyfunc,
}

#[derive(Debug, Clone)]
pub enum NumType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug)]
pub struct GlobalType {
    pub content_type: NumType,
    pub mutable: bool,
}

#[derive(Debug, Clone)]
pub enum ValType {
    Num(NumType),
    Vec(u128),
    Ref(RefType),
}

#[derive(Debug, Clone)]
pub struct Custom<'a> {
    pub name: WasmString<'a>,
    pub payload: &'a [u8],
}

#[derive(Debug, Clone)]
pub struct FuncType(Vec<ValType>, Vec<ValType>);

#[derive(Debug, Clone)]
pub struct Import<'a> {
    pub module: WasmString<'a>,
    pub field: WasmString<'a>,
    pub kind: ExternalKind,
    pub index: u32,
}

#[derive(Debug)]
pub struct Function {
    pub index: TypeIdx,
}

#[derive(Debug)]
pub struct Table {
    pub elem_type: ElemType,
    pub limit: Limit,
}

#[derive(Debug, Clone)]
pub struct Limit {
    pub initial: u32,
    pub max: Option<u32>,
}

#[derive(Debug)]
pub struct Memory {
    pub limit: Limit,
}

#[derive(Debug)]
pub struct Global {
    pub kind: GlobalType,
    pub init: InitExpr,
}

#[derive(Debug)]
pub struct Export<'a> {
    pub field: WasmString<'a>,
    pub kind: ExternalKind,
    pub external_idx: u32,
}

#[derive(Debug)]
pub struct Start {
    pub func_idx: u32,
}

#[derive(Debug)]
pub struct Element {
    pub tbl_index: TableIdx,
    pub offset: InitExpr,
    pub elems: Vec<u32>,
}

#[derive(Debug)]
pub struct Code<'a> {
    pub locals: Vec<ValType>,
    // TODO: custom type to control memory better
    pub body: Vec<Operator<'a>>,
}

#[derive(Debug)]
pub struct Data<'a> {
    pub index: MemIdx,
    pub offset: InitExpr,
    pub data: &'a [u8],
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum InitExpr {
    I32Const(i32),
    I64Const(i64),
    F32Const(f32),
    F64Const(f64),
    ConstGlobalGet(u32),
}

pub type MemIdx = u32;
pub type TypeIdx = u32;
pub type FuncIdx = u32;
pub type TableIdx = u32;
pub type WasmString<'a> = &'a [u8];

#[derive(Debug)]
pub struct Module<'a> {
    pub customs: Vec<Custom<'a>>,
    pub types: Vec<FuncType>,
    pub imports: Vec<Import<'a>>,
    pub functions: Vec<Function>,
    pub tables: Vec<Table>,
    pub memories: Vec<Memory>,
    pub globals: Vec<Global>,
    pub export: Vec<Export<'a>>,
    pub start: Option<FuncIdx>,
    pub elements: Vec<Element>,
    pub codes: Vec<Code<'a>>,
    pub datas: Vec<Data<'a>>,
}
