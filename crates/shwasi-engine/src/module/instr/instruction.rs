use std::fmt;

use crate::module::{
    BlockType, BrTable, DataIdx, ElemIdx, FuncIdx, MemArg, Opcode, RefType, TableIdx, TypeIdx,
    ValType, F32, F64,
};

#[derive(Debug)]
pub enum Instruction {
    Unreachable,
    Nop,
    Else,
    End,
    Return,
    Drop,
    Select,
    I32Eqz,
    I32Eq,
    I32Ne,
    I32LtS,
    I32LtU,
    I32GtS,
    I32GtU,
    I32LeS,
    I32LeU,
    I32GeS,
    I32GeU,
    I64Eqz,
    I64Eq,
    I64Ne,
    I64LtS,
    I64LtU,
    I64GtS,
    I64GtU,
    I64LeS,
    I64LeU,
    I64GeS,
    I64GeU,
    F32Eq,
    F32Ne,
    F32Lt,
    F32Gt,
    F32Le,
    F32Ge,
    F64Eq,
    F64Ne,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,
    I32Clz,
    I32Ctz,
    I32Popcnt,
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    I32RemS,
    I32RemU,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrS,
    I32ShrU,
    I32Rotl,
    I32Rotr,
    I64Clz,
    I64Ctz,
    I64Popcnt,
    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    I64DivU,
    I64RemS,
    I64RemU,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrS,
    I64ShrU,
    I64Rotl,
    I64Rotr,
    F32Abs,
    F32Neg,
    F32Ceil,
    F32Floor,
    F32Trunc,
    F32Nearest,
    F32Sqrt,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Min,
    F32Max,
    F32Copysign,
    F64Abs,
    F64Neg,
    F64Ceil,
    F64Floor,
    F64Trunc,
    F64Nearest,
    F64Sqrt,
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Min,
    F64Max,
    F64Copysign,
    I32WrapI64,
    I32TruncF32S,
    I32TruncF32U,
    I32TruncF64S,
    I32TruncF64U,
    I64ExtendI32S,
    I64ExtendI32U,
    I64TruncF32S,
    I64TruncF32U,
    I64TruncF64S,
    I64TruncF64U,
    F32ConvertI32S,
    F32ConvertI32U,
    F32ConvertI64S,
    F32ConvertI64U,
    F32DemoteF64,
    F64ConvertI32S,
    F64ConvertI32U,
    F64ConvertI64S,
    F64ConvertI64U,
    F64PromoteF32,
    I32ReinterpretF32,
    I64ReinterpretF64,
    F32ReinterpretI32,
    F64ReinterpretI64,
    MemorySize,
    MemoryGrow,
    MemoryCopy,
    RefIsNull,
    MemoryFill,
    I32TruncSatF32S,
    I32TruncSatF32U,
    I32TruncSatF64S,
    I32TruncSatF64U,
    I64TruncSatF32S,
    I64TruncSatF32U,
    I64TruncSatF64S,
    I64TruncSatF64U,
    I32Extend8S,
    I32Extend16S,
    I64Extend8S,
    I64Extend16S,
    I64Extend32S,

    I32Load(MemArg),
    I64Load(MemArg),
    F32Load(MemArg),
    F64Load(MemArg),
    I32Load8S(MemArg),
    I32Load8U(MemArg),
    I32Load16S(MemArg),
    I32Load16U(MemArg),
    I64Load8S(MemArg),
    I64Load8U(MemArg),
    I64Load16S(MemArg),
    I64Load16U(MemArg),
    I64Load32S(MemArg),
    I64Load32U(MemArg),
    I32Store(MemArg),
    I64Store(MemArg),
    F32Store(MemArg),
    F64Store(MemArg),
    I32Store8(MemArg),
    I32Store16(MemArg),
    I64Store8(MemArg),
    I64Store16(MemArg),
    I64Store32(MemArg),

    Block(BlockType),
    Loop(BlockType),
    If(BlockType),
    Br {
        depth: u32,
    },
    BrIf {
        depth: u32,
    },
    Call {
        func_idx: FuncIdx,
    },
    LocalGet {
        idx: u32,
    },
    LocalSet {
        idx: u32,
    },
    LocalTee {
        idx: u32,
    },
    GlobalGet {
        idx: u32,
    },
    GlobalSet {
        idx: u32,
    },
    DataDrop {
        data_idx: DataIdx,
    },
    ElemDrop {
        elem_idx: ElemIdx,
    },
    I32Const(i32),
    F32Const(F32),
    MemoryInit {
        data_idx: DataIdx,
    },
    RefFunc {
        func_idx: FuncIdx,
    },
    TableGet {
        table: TableIdx,
    },
    TableSet {
        table: TableIdx,
    },
    TableGrow {
        table: TableIdx,
    },
    TableSize {
        table: TableIdx,
    },
    TableFill {
        table: TableIdx,
    },
    RefNull {
        ty: RefType,
    },
    I64Const(i64),
    F64Const(F64),
    CallIndirect {
        type_idx: TypeIdx,
        table_idx: TableIdx,
    },
    TableCopy {
        src_table: TableIdx,
        dst_table: TableIdx,
    },
    TableInit {
        elem_idx: ElemIdx,
        table_idx: TableIdx,
    },
    BrTable(BrTable),
}

impl Instruction {
    pub fn opcode(&self) -> Opcode {
        match self {
            Instruction::Unreachable => Opcode::Unreachable,
            Instruction::Nop => Opcode::Nop,
            Instruction::Else => Opcode::Else,
            Instruction::End => Opcode::End,
            Instruction::Return => Opcode::Return,
            Instruction::Drop => Opcode::Drop,
            Instruction::Select => Opcode::Select,
            Instruction::I32Eqz => Opcode::I32Eqz,
            Instruction::I32Eq => Opcode::I32Eq,
            Instruction::I32Ne => Opcode::I32Ne,
            Instruction::I32LtS => Opcode::I32LtS,
            Instruction::I32LtU => Opcode::I32LtU,
            Instruction::I32GtS => Opcode::I32GtS,
            Instruction::I32GtU => Opcode::I32GtU,
            Instruction::I32LeS => Opcode::I32LeS,
            Instruction::I32LeU => Opcode::I32LeU,
            Instruction::I32GeS => Opcode::I32GeS,
            Instruction::I32GeU => Opcode::I32GeU,
            Instruction::I64Eqz => Opcode::I64Eqz,
            Instruction::I64Eq => Opcode::I64Eq,
            Instruction::I64Ne => Opcode::I64Ne,
            Instruction::I64LtS => Opcode::I64LtS,
            Instruction::I64LtU => Opcode::I64LtU,
            Instruction::I64GtS => Opcode::I64GtS,
            Instruction::I64GtU => Opcode::I64GtU,
            Instruction::I64LeS => Opcode::I64LeS,
            Instruction::I64LeU => Opcode::I64LeU,
            Instruction::I64GeS => Opcode::I64GeS,
            Instruction::I64GeU => Opcode::I64GeU,
            Instruction::F32Eq => Opcode::F32Eq,
            Instruction::F32Ne => Opcode::F32Ne,
            Instruction::F32Lt => Opcode::F32Lt,
            Instruction::F32Gt => Opcode::F32Gt,
            Instruction::F32Le => Opcode::F32Le,
            Instruction::F32Ge => Opcode::F32Ge,
            Instruction::F64Eq => Opcode::F64Eq,
            Instruction::F64Ne => Opcode::F64Ne,
            Instruction::F64Lt => Opcode::F64Lt,
            Instruction::F64Gt => Opcode::F64Gt,
            Instruction::F64Le => Opcode::F64Le,
            Instruction::F64Ge => Opcode::F64Ge,
            Instruction::I32Clz => Opcode::I32Clz,
            Instruction::I32Ctz => Opcode::I32Ctz,
            Instruction::I32Popcnt => Opcode::I32Popcnt,
            Instruction::I32Add => Opcode::I32Add,
            Instruction::I32Sub => Opcode::I32Sub,
            Instruction::I32Mul => Opcode::I32Mul,
            Instruction::I32DivS => Opcode::I32DivS,
            Instruction::I32DivU => Opcode::I32DivU,
            Instruction::I32RemS => Opcode::I32RemS,
            Instruction::I32RemU => Opcode::I32RemU,
            Instruction::I32And => Opcode::I32And,
            Instruction::I32Or => Opcode::I32Or,
            Instruction::I32Xor => Opcode::I32Xor,
            Instruction::I32Shl => Opcode::I32Shl,
            Instruction::I32ShrS => Opcode::I32ShrS,
            Instruction::I32ShrU => Opcode::I32ShrU,
            Instruction::I32Rotl => Opcode::I32Rotl,
            Instruction::I32Rotr => Opcode::I32Rotr,
            Instruction::I64Clz => Opcode::I64Clz,
            Instruction::I64Ctz => Opcode::I64Ctz,
            Instruction::I64Popcnt => Opcode::I64Popcnt,
            Instruction::I64Add => Opcode::I64Add,
            Instruction::I64Sub => Opcode::I64Sub,
            Instruction::I64Mul => Opcode::I64Mul,
            Instruction::I64DivS => Opcode::I64DivS,
            Instruction::I64DivU => Opcode::I64DivU,
            Instruction::I64RemS => Opcode::I64RemS,
            Instruction::I64RemU => Opcode::I64RemU,
            Instruction::I64And => Opcode::I64And,
            Instruction::I64Or => Opcode::I64Or,
            Instruction::I64Xor => Opcode::I64Xor,
            Instruction::I64Shl => Opcode::I64Shl,
            Instruction::I64ShrS => Opcode::I64ShrS,
            Instruction::I64ShrU => Opcode::I64ShrU,
            Instruction::I64Rotl => Opcode::I64Rotl,
            Instruction::I64Rotr => Opcode::I64Rotr,
            Instruction::F32Abs => Opcode::F32Abs,
            Instruction::F32Neg => Opcode::F32Neg,
            Instruction::F32Ceil => Opcode::F32Ceil,
            Instruction::F32Floor => Opcode::F32Floor,
            Instruction::F32Trunc => Opcode::F32Trunc,
            Instruction::F32Nearest => Opcode::F32Nearest,
            Instruction::F32Sqrt => Opcode::F32Sqrt,
            Instruction::F32Add => Opcode::F32Add,
            Instruction::F32Sub => Opcode::F32Sub,
            Instruction::F32Mul => Opcode::F32Mul,
            Instruction::F32Div => Opcode::F32Div,
            Instruction::F32Min => Opcode::F32Min,
            Instruction::F32Max => Opcode::F32Max,
            Instruction::F32Copysign => Opcode::F32Copysign,
            Instruction::F64Abs => Opcode::F64Abs,
            Instruction::F64Neg => Opcode::F64Neg,
            Instruction::F64Ceil => Opcode::F64Ceil,
            Instruction::F64Floor => Opcode::F64Floor,
            Instruction::F64Trunc => Opcode::F64Trunc,
            Instruction::F64Nearest => Opcode::F64Nearest,
            Instruction::F64Sqrt => Opcode::F64Sqrt,
            Instruction::F64Add => Opcode::F64Add,
            Instruction::F64Sub => Opcode::F64Sub,
            Instruction::F64Mul => Opcode::F64Mul,
            Instruction::F64Div => Opcode::F64Div,
            Instruction::F64Min => Opcode::F64Min,
            Instruction::F64Max => Opcode::F64Max,
            Instruction::F64Copysign => Opcode::F64Copysign,
            Instruction::I32WrapI64 => Opcode::I32WrapI64,
            Instruction::I32TruncF32S => Opcode::I32TruncF32S,
            Instruction::I32TruncF32U => Opcode::I32TruncF32U,
            Instruction::I32TruncF64S => Opcode::I32TruncF64S,
            Instruction::I32TruncF64U => Opcode::I32TruncF64U,
            Instruction::I64ExtendI32S => Opcode::I64ExtendI32S,
            Instruction::I64ExtendI32U => Opcode::I64ExtendI32U,
            Instruction::I64TruncF32S => Opcode::I64TruncF32S,
            Instruction::I64TruncF32U => Opcode::I64TruncF32U,
            Instruction::I64TruncF64S => Opcode::I64TruncF64S,
            Instruction::I64TruncF64U => Opcode::I64TruncF64U,
            Instruction::F32ConvertI32S => Opcode::F32ConvertI32S,
            Instruction::F32ConvertI32U => Opcode::F32ConvertI32U,
            Instruction::F32ConvertI64S => Opcode::F32ConvertI64S,
            Instruction::F32ConvertI64U => Opcode::F32ConvertI64U,
            Instruction::F32DemoteF64 => Opcode::F32DemoteF64,
            Instruction::F64ConvertI32S => Opcode::F64ConvertI32S,
            Instruction::F64ConvertI32U => Opcode::F64ConvertI32U,
            Instruction::F64ConvertI64S => Opcode::F64ConvertI64S,
            Instruction::F64ConvertI64U => Opcode::F64ConvertI64U,
            Instruction::F64PromoteF32 => Opcode::F64PromoteF32,
            Instruction::I32ReinterpretF32 => Opcode::I32ReinterpretF32,
            Instruction::I64ReinterpretF64 => Opcode::I64ReinterpretF64,
            Instruction::F32ReinterpretI32 => Opcode::F32ReinterpretI32,
            Instruction::F64ReinterpretI64 => Opcode::F64ReinterpretI64,
            Instruction::MemorySize => Opcode::MemorySize,
            Instruction::MemoryGrow => Opcode::MemoryGrow,
            Instruction::MemoryCopy => Opcode::MemoryCopy,
            Instruction::RefIsNull => Opcode::RefIsNull,
            Instruction::MemoryFill => Opcode::MemoryFill,
            Instruction::I32Load(_) => Opcode::I32Load,
            Instruction::I64Load(_) => Opcode::I64Load,
            Instruction::F32Load(_) => Opcode::F32Load,
            Instruction::F64Load(_) => Opcode::F64Load,
            Instruction::I32Load8S(_) => Opcode::I32Load8S,
            Instruction::I32Load8U(_) => Opcode::I32Load8U,
            Instruction::I32Load16S(_) => Opcode::I32Load16S,
            Instruction::I32Load16U(_) => Opcode::I32Load16U,
            Instruction::I64Load8S(_) => Opcode::I64Load8S,
            Instruction::I64Load8U(_) => Opcode::I64Load8U,
            Instruction::I64Load16S(_) => Opcode::I64Load16S,
            Instruction::I64Load16U(_) => Opcode::I64Load16U,
            Instruction::I64Load32S(_) => Opcode::I64Load32S,
            Instruction::I64Load32U(_) => Opcode::I64Load32U,
            Instruction::I32Store(_) => Opcode::I32Store,
            Instruction::I64Store(_) => Opcode::I64Store,
            Instruction::F32Store(_) => Opcode::F32Store,
            Instruction::F64Store(_) => Opcode::F64Store,
            Instruction::I32Store8(_) => Opcode::I32Store8,
            Instruction::I32Store16(_) => Opcode::I32Store16,
            Instruction::I64Store8(_) => Opcode::I64Store8,
            Instruction::I64Store16(_) => Opcode::I64Store16,
            Instruction::I64Store32(_) => Opcode::I64Store32,
            Instruction::Block(_) => Opcode::Block,
            Instruction::Loop(_) => Opcode::Loop,
            Instruction::If(_) => Opcode::If,
            Instruction::Br { .. } => Opcode::Br,
            Instruction::BrIf { .. } => Opcode::BrIf,
            Instruction::Call { .. } => Opcode::Call,
            Instruction::LocalGet { .. } => Opcode::LocalGet,
            Instruction::LocalSet { .. } => Opcode::LocalSet,
            Instruction::LocalTee { .. } => Opcode::LocalTee,
            Instruction::GlobalGet { .. } => Opcode::GlobalGet,
            Instruction::GlobalSet { .. } => Opcode::GlobalSet,
            Instruction::DataDrop { .. } => Opcode::DataDrop,
            Instruction::ElemDrop { .. } => Opcode::ElemDrop,
            Instruction::I32Const(_) => Opcode::I32Const,
            Instruction::F32Const(_) => Opcode::F32Const,
            Instruction::MemoryInit { .. } => Opcode::MemoryInit,
            Instruction::RefFunc { .. } => Opcode::RefFunc,
            Instruction::TableGet { .. } => Opcode::TableGet,
            Instruction::TableSet { .. } => Opcode::TableSet,
            Instruction::TableGrow { .. } => Opcode::TableGrow,
            Instruction::TableSize { .. } => Opcode::TableSize,
            Instruction::TableFill { .. } => Opcode::TableFill,
            Instruction::RefNull { .. } => Opcode::RefNull,
            Instruction::I64Const(_) => Opcode::I64Const,
            Instruction::F64Const(_) => Opcode::F64Const,
            Instruction::CallIndirect { .. } => Opcode::CallIndirect,
            Instruction::TableCopy { .. } => Opcode::TableCopy,
            Instruction::TableInit { .. } => Opcode::TableInit,
            Instruction::BrTable(_) => Opcode::BrTable,
            Instruction::I32TruncSatF32S => Opcode::I32TruncSatF32S,
            Instruction::I32TruncSatF32U => Opcode::I32TruncSatF32U,
            Instruction::I32TruncSatF64S => Opcode::I32TruncSatF64S,
            Instruction::I32TruncSatF64U => Opcode::I32TruncSatF64U,
            Instruction::I64TruncSatF32S => Opcode::I64TruncSatF32S,
            Instruction::I64TruncSatF32U => Opcode::I64TruncSatF32U,
            Instruction::I64TruncSatF64S => Opcode::I64TruncSatF64S,
            Instruction::I64TruncSatF64U => Opcode::I64TruncSatF64U,
            Instruction::I32Extend8S => Opcode::I32Extend8S,
            Instruction::I32Extend16S => Opcode::I32Extend16S,
            Instruction::I64Extend8S => Opcode::I64Extend8S,
            Instruction::I64Extend16S => Opcode::I64Extend16S,
            Instruction::I64Extend32S => Opcode::I64Extend32S,
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.opcode())?;
        match self {
            Instruction::Unreachable
            | Instruction::Nop
            | Instruction::Else
            | Instruction::End
            | Instruction::Return
            | Instruction::Drop
            | Instruction::Select
            | Instruction::I32Eqz
            | Instruction::I32Eq
            | Instruction::I32Ne
            | Instruction::I32LtS
            | Instruction::I32LtU
            | Instruction::I32GtS
            | Instruction::I32GtU
            | Instruction::I32LeS
            | Instruction::I32LeU
            | Instruction::I32GeS
            | Instruction::I32GeU
            | Instruction::I64Eqz
            | Instruction::I64Eq
            | Instruction::I64Ne
            | Instruction::I64LtS
            | Instruction::I64LtU
            | Instruction::I64GtS
            | Instruction::I64GtU
            | Instruction::I64LeS
            | Instruction::I64LeU
            | Instruction::I64GeS
            | Instruction::I64GeU
            | Instruction::F32Eq
            | Instruction::F32Ne
            | Instruction::F32Lt
            | Instruction::F32Gt
            | Instruction::F32Le
            | Instruction::F32Ge
            | Instruction::F64Eq
            | Instruction::F64Ne
            | Instruction::F64Lt
            | Instruction::F64Gt
            | Instruction::F64Le
            | Instruction::F64Ge
            | Instruction::I32Clz
            | Instruction::I32Ctz
            | Instruction::I32Popcnt
            | Instruction::I32Add
            | Instruction::I32Sub
            | Instruction::I32Mul
            | Instruction::I32DivS
            | Instruction::I32DivU
            | Instruction::I32RemS
            | Instruction::I32RemU
            | Instruction::I32And
            | Instruction::I32Or
            | Instruction::I32Xor
            | Instruction::I32Shl
            | Instruction::I32ShrS
            | Instruction::I32ShrU
            | Instruction::I32Rotl
            | Instruction::I32Rotr
            | Instruction::I64Clz
            | Instruction::I64Ctz
            | Instruction::I64Popcnt
            | Instruction::I64Add
            | Instruction::I64Sub
            | Instruction::I64Mul
            | Instruction::I64DivS
            | Instruction::I64DivU
            | Instruction::I64RemS
            | Instruction::I64RemU
            | Instruction::I64And
            | Instruction::I64Or
            | Instruction::I64Xor
            | Instruction::I64Shl
            | Instruction::I64ShrS
            | Instruction::I64ShrU
            | Instruction::I64Rotl
            | Instruction::I64Rotr
            | Instruction::F32Abs
            | Instruction::F32Neg
            | Instruction::F32Ceil
            | Instruction::F32Floor
            | Instruction::F32Trunc
            | Instruction::F32Nearest
            | Instruction::F32Sqrt
            | Instruction::F32Add
            | Instruction::F32Sub
            | Instruction::F32Mul
            | Instruction::F32Div
            | Instruction::F32Min
            | Instruction::F32Max
            | Instruction::F32Copysign
            | Instruction::F64Abs
            | Instruction::F64Neg
            | Instruction::F64Ceil
            | Instruction::F64Floor
            | Instruction::F64Trunc
            | Instruction::F64Nearest
            | Instruction::F64Sqrt
            | Instruction::F64Add
            | Instruction::F64Sub
            | Instruction::F64Mul
            | Instruction::F64Div
            | Instruction::F64Min
            | Instruction::F64Max
            | Instruction::F64Copysign
            | Instruction::I32WrapI64
            | Instruction::I32TruncF32S
            | Instruction::I32TruncF32U
            | Instruction::I32TruncF64S
            | Instruction::I32TruncF64U
            | Instruction::I64ExtendI32S
            | Instruction::I64ExtendI32U
            | Instruction::I64TruncF32S
            | Instruction::I64TruncF32U
            | Instruction::I64TruncF64S
            | Instruction::I64TruncF64U
            | Instruction::F32ConvertI32S
            | Instruction::F32ConvertI32U
            | Instruction::F32ConvertI64S
            | Instruction::F32ConvertI64U
            | Instruction::F32DemoteF64
            | Instruction::F64ConvertI32S
            | Instruction::F64ConvertI32U
            | Instruction::F64ConvertI64S
            | Instruction::F64ConvertI64U
            | Instruction::F64PromoteF32
            | Instruction::I32ReinterpretF32
            | Instruction::I64ReinterpretF64
            | Instruction::F32ReinterpretI32
            | Instruction::F64ReinterpretI64
            | Instruction::MemorySize
            | Instruction::MemoryGrow
            | Instruction::MemoryCopy
            | Instruction::RefIsNull
            | Instruction::I32TruncSatF32S
            | Instruction::I32TruncSatF32U
            | Instruction::I32TruncSatF64S
            | Instruction::I32TruncSatF64U
            | Instruction::I64TruncSatF32S
            | Instruction::I64TruncSatF32U
            | Instruction::I64TruncSatF64S
            | Instruction::I64TruncSatF64U
            | Instruction::MemoryFill
            | Instruction::I32Extend8S
            | Instruction::I32Extend16S
            | Instruction::I64Extend8S
            | Instruction::I64Extend16S
            | Instruction::I64Extend32S => Ok(()),

            Instruction::I32Load(memarg)
            | Instruction::I64Load(memarg)
            | Instruction::F32Load(memarg)
            | Instruction::F64Load(memarg)
            | Instruction::I32Load8S(memarg)
            | Instruction::I32Load8U(memarg)
            | Instruction::I32Load16S(memarg)
            | Instruction::I32Load16U(memarg)
            | Instruction::I64Load8S(memarg)
            | Instruction::I64Load8U(memarg)
            | Instruction::I64Load16S(memarg)
            | Instruction::I64Load16U(memarg)
            | Instruction::I64Load32S(memarg)
            | Instruction::I64Load32U(memarg)
            | Instruction::I32Store(memarg)
            | Instruction::I64Store(memarg)
            | Instruction::F32Store(memarg)
            | Instruction::F64Store(memarg)
            | Instruction::I32Store8(memarg)
            | Instruction::I32Store16(memarg)
            | Instruction::I64Store8(memarg)
            | Instruction::I64Store16(memarg)
            | Instruction::I64Store32(memarg) => {
                write!(f, " {} (align) {} (offset)", memarg.align, memarg.offset)
            }

            Instruction::Block(blockty) | Instruction::Loop(blockty) | Instruction::If(blockty) => {
                match blockty {
                    BlockType::Empty => Ok(()),
                    BlockType::Type(val_type) => {
                        let write = match val_type {
                            ValType::I32 => "i32",
                            ValType::I64 => "i64",
                            ValType::F32 => "f32",
                            ValType::F64 => "f64",
                            ValType::Func => "funcref",
                            ValType::Extern => "externref",
                        };
                        write!(f, " {write}")
                    }
                    BlockType::FuncType(functype) => write!(f, " {functype} (functype)"),
                }
            }

            Instruction::Br { depth } | Instruction::BrIf { depth } => {
                write!(f, " {depth} (depth)")
            }
            Instruction::LocalGet { idx }
            | Instruction::LocalSet { idx }
            | Instruction::LocalTee { idx }
            | Instruction::Call { func_idx: idx }
            | Instruction::DataDrop { data_idx: idx }
            | Instruction::ElemDrop { elem_idx: idx }
            | Instruction::MemoryInit { data_idx: idx }
            | Instruction::RefFunc { func_idx: idx }
            | Instruction::GlobalGet { idx }
            | Instruction::TableGet { table: idx }
            | Instruction::TableSet { table: idx }
            | Instruction::TableGrow { table: idx }
            | Instruction::TableSize { table: idx }
            | Instruction::TableFill { table: idx }
            | Instruction::GlobalSet { idx } => {
                write!(f, " {idx}")
            }
            Instruction::I32Const(idx) => write!(f, " {idx}"),

            Instruction::F32Const(idx) => {
                write!(f, " {idx}")
            }
            Instruction::F64Const(idx) => {
                write!(f, " {idx}")
            }

            Instruction::RefNull { ty } => {
                let write = match ty {
                    RefType::Func => "funcref",
                    RefType::Extern => "externref",
                };
                write!(f, " {write}")
            }

            Instruction::I64Const(val) => write!(f, " {val}"),

            Instruction::CallIndirect {
                type_idx,
                table_idx,
            } => write!(f, " {type_idx} (type) {table_idx} (table)"),

            Instruction::TableCopy {
                src_table,
                dst_table,
            } => write!(f, " {src_table} (src) {dst_table} (dst)"),

            Instruction::TableInit {
                elem_idx,
                table_idx,
            } => write!(f, " {elem_idx} (elem) {table_idx} (table)"),

            Instruction::BrTable(br_table) => {
                write!(f, " {} (depths)", br_table.depths.len())?;
                for depth in &br_table.depths {
                    write!(f, " {depth}")?;
                }
                write!(f, "{} (default)", br_table.default_depth)
            }
        }
    }
}
