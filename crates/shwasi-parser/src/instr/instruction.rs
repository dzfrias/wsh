use std::fmt;

use crate::{
    BlockType, BrTable, DataIdx, ElemIdx, FuncIdx, MemArg, Opcode, RefType, TableIdx, TypeIdx,
    ValType, F32, F64,
};

#[derive(Debug, PartialEq)]
#[repr(u8)]
pub enum Instruction {
    Unreachable = 0,
    Nop = 1,
    Else = 2,
    End = 3,
    Return = 4,
    Drop = 5,
    Select = 6,
    I32Eqz = 7,
    I32Eq = 8,
    I32Ne = 9,
    I32LtS = 10,
    I32LtU = 11,
    I32GtS = 12,
    I32GtU = 13,
    I32LeS = 14,
    I32LeU = 15,
    I32GeS = 16,
    I32GeU = 17,
    I64Eqz = 18,
    I64Eq = 19,
    I64Ne = 20,
    I64LtS = 21,
    I64LtU = 22,
    I64GtS = 23,
    I64GtU = 24,
    I64LeS = 25,
    I64LeU = 26,
    I64GeS = 27,
    I64GeU = 28,
    F32Eq = 29,
    F32Ne = 30,
    F32Lt = 31,
    F32Gt = 32,
    F32Le = 33,
    F32Ge = 34,
    F64Eq = 35,
    F64Ne = 36,
    F64Lt = 37,
    F64Gt = 38,
    F64Le = 39,
    F64Ge = 40,
    I32Clz = 41,
    I32Ctz = 42,
    I32Popcnt = 43,
    I32Add = 44,
    I32Sub = 45,
    I32Mul = 46,
    I32DivS = 47,
    I32DivU = 48,
    I32RemS = 49,
    I32RemU = 50,
    I32And = 51,
    I32Or = 52,
    I32Xor = 53,
    I32Shl = 54,
    I32ShrS = 55,
    I32ShrU = 56,
    I32Rotl = 57,
    I32Rotr = 58,
    I64Clz = 59,
    I64Ctz = 60,
    I64Popcnt = 61,
    I64Add = 62,
    I64Sub = 63,
    I64Mul = 64,
    I64DivS = 65,
    I64DivU = 66,
    I64RemS = 67,
    I64RemU = 68,
    I64And = 69,
    I64Or = 70,
    I64Xor = 71,
    I64Shl = 72,
    I64ShrS = 73,
    I64ShrU = 74,
    I64Rotl = 75,
    I64Rotr = 76,
    F32Abs = 77,
    F32Neg = 78,
    F32Ceil = 79,
    F32Floor = 80,
    F32Trunc = 81,
    F32Nearest = 82,
    F32Sqrt = 83,
    F32Add = 84,
    F32Sub = 85,
    F32Mul = 86,
    F32Div = 87,
    F32Min = 88,
    F32Max = 89,
    F32Copysign = 90,
    F64Abs = 91,
    F64Neg = 92,
    F64Ceil = 93,
    F64Floor = 94,
    F64Trunc = 95,
    F64Nearest = 96,
    F64Sqrt = 97,
    F64Add = 98,
    F64Sub = 99,
    F64Mul = 100,
    F64Div = 101,
    F64Min = 102,
    F64Max = 103,
    F64Copysign = 104,
    I32WrapI64 = 105,
    I32TruncF32S = 106,
    I32TruncF32U = 107,
    I32TruncF64S = 108,
    I32TruncF64U = 109,
    I64ExtendI32S = 110,
    I64ExtendI32U = 111,
    I64TruncF32S = 112,
    I64TruncF32U = 113,
    I64TruncF64S = 114,
    I64TruncF64U = 115,
    F32ConvertI32S = 116,
    F32ConvertI32U = 117,
    F32ConvertI64S = 118,
    F32ConvertI64U = 119,
    F32DemoteF64 = 120,
    F64ConvertI32S = 121,
    F64ConvertI32U = 122,
    F64ConvertI64S = 123,
    F64ConvertI64U = 124,
    F64PromoteF32 = 125,
    I32ReinterpretF32 = 126,
    I64ReinterpretF64 = 127,
    F32ReinterpretI32 = 128,
    F64ReinterpretI64 = 129,
    MemorySize = 130,
    MemoryGrow = 131,
    MemoryCopy = 132,
    RefIsNull = 133,
    MemoryFill = 134,
    I32TruncSatF32S = 135,
    I32TruncSatF32U = 136,
    I32TruncSatF64S = 137,
    I32TruncSatF64U = 138,
    I64TruncSatF32S = 139,
    I64TruncSatF32U = 140,
    I64TruncSatF64S = 141,
    I64TruncSatF64U = 142,
    I32Extend8S = 143,
    I32Extend16S = 144,
    I64Extend8S = 145,
    I64Extend16S = 146,
    I64Extend32S = 147,
    SelectT(ValType) = 148,
    I32Load(MemArg) = 149,
    I64Load(MemArg) = 150,
    F32Load(MemArg) = 151,
    F64Load(MemArg) = 152,
    I32Load8S(MemArg) = 153,
    I32Load8U(MemArg) = 154,
    I32Load16S(MemArg) = 155,
    I32Load16U(MemArg) = 156,
    I64Load8S(MemArg) = 157,
    I64Load8U(MemArg) = 158,
    I64Load16S(MemArg) = 159,
    I64Load16U(MemArg) = 160,
    I64Load32S(MemArg) = 161,
    I64Load32U(MemArg) = 162,
    I32Store(MemArg) = 163,
    I64Store(MemArg) = 164,
    F32Store(MemArg) = 165,
    F64Store(MemArg) = 166,
    I32Store8(MemArg) = 167,
    I32Store16(MemArg) = 168,
    I64Store8(MemArg) = 169,
    I64Store16(MemArg) = 170,
    I64Store32(MemArg) = 171,
    Block(BlockType) = 172,
    Loop(BlockType) = 173,
    If(BlockType) = 174,
    Br {
        depth: u32,
    } = 175,
    BrIf {
        depth: u32,
    } = 176,
    Call {
        func_idx: FuncIdx,
    } = 177,
    LocalGet {
        idx: u32,
    } = 178,
    LocalSet {
        idx: u32,
    } = 179,
    LocalTee {
        idx: u32,
    } = 180,
    GlobalGet {
        idx: u32,
    } = 181,
    GlobalSet {
        idx: u32,
    } = 182,
    DataDrop {
        data_idx: DataIdx,
    } = 183,
    ElemDrop {
        elem_idx: ElemIdx,
    } = 184,
    I32Const(u32) = 185,
    F32Const(F32) = 186,
    MemoryInit {
        data_idx: DataIdx,
    } = 187,
    RefFunc {
        func_idx: FuncIdx,
    } = 188,
    TableGet {
        table: TableIdx,
    } = 189,
    TableSet {
        table: TableIdx,
    } = 190,
    TableGrow {
        table: TableIdx,
    } = 191,
    TableSize {
        table: TableIdx,
    } = 192,
    TableFill {
        table: TableIdx,
    } = 193,
    RefNull {
        ty: RefType,
    } = 194,
    I64Const(u64) = 195,
    F64Const(F64) = 196,
    CallIndirect {
        type_idx: TypeIdx,
        table_idx: TableIdx,
    } = 197,
    TableCopy {
        src_table: TableIdx,
        dst_table: TableIdx,
    } = 198,
    TableInit {
        elem_idx: ElemIdx,
        table_idx: TableIdx,
    } = 199,
    BrTable(BrTable) = 200,
}

impl Instruction {
    /// Returns a unique number corresponding to the discriminant of this instruction. Useful in
    /// operimizations that involve storing these intructions in an `InstrBuffer`.
    pub(crate) fn discriminant(&self) -> u8 {
        // SAFETY: Because `Instruction` is marked `repr(u8)`, its layout is a `repr(C)` `union`
        // between `repr(C)` structs, each of which has the `u8` discriminant as its first
        // field, so we can read the discriminant without offsetting the pointer.
        unsafe { *<*const _>::from(self).cast::<u8>() }
    }

    /// Returns the opcode of this instruction.
    #[inline]
    pub fn opcode(&self) -> Opcode {
        // SAFETY: Since this is an instruction, it has a valid opcode.
        unsafe { Opcode::from_instr_discriminant(self.discriminant()) }
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
                for depth in br_table.depths.iter() {
                    write!(f, " {depth}")?;
                }
                write!(f, "{} (default)", br_table.default_depth)
            }
            Instruction::SelectT(ty) => {
                write!(f, " {ty}")
            }
        }
    }
}
