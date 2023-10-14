#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(super) enum OperandsType {
    MemArg,
    FourBytes,
    EightBytes,
    BlockType,
    Empty,
    RefType,
    BrTable,
    U64,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Opcode {
    Unreachable,
    Nop,
    Block,
    Loop,
    If,
    Else,
    End,
    Br,
    BrIf,
    BrTable,
    Return,
    Call,
    CallIndirect,
    Drop,
    // TODO: add SelectT
    Select,
    LocalGet,
    LocalSet,
    LocalTee,
    GlobalGet,
    GlobalSet,
    I32Load,
    I64Load,
    F32Load,
    F64Load,
    I32Load8S,
    I32Load8U,
    I32Load16S,
    I32Load16U,
    I64Load8S,
    I64Load8U,
    I64Load16S,
    I64Load16U,
    I64Load32S,
    I64Load32U,
    I32Store,
    I64Store,
    F32Store,
    F64Store,
    I32Store8,
    I32Store16,
    I64Store8,
    I64Store16,
    I64Store32,
    MemorySize,
    MemoryGrow,
    I32Const,
    I64Const,
    F32Const,
    F64Const,
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
    MemoryInit,
    DataDrop,
    MemoryCopy,
    MemoryFill,
    TableInit,
    TableCopy,
    TableGet,
    TableSet,
    TableGrow,
    TableSize,
    TableFill,
    ElemDrop,
    RefNull,
    RefIsNull,
    RefFunc,
}

impl Opcode {
    pub(super) fn operand_type(&self) -> OperandsType {
        match self {
            Opcode::Unreachable
            | Opcode::Nop
            | Opcode::Else
            | Opcode::End
            | Opcode::Return
            | Opcode::Drop
            | Opcode::Select
            | Opcode::I32Eqz
            | Opcode::I32Eq
            | Opcode::I32Ne
            | Opcode::I32LtS
            | Opcode::I32LtU
            | Opcode::I32GtS
            | Opcode::I32GtU
            | Opcode::I32LeS
            | Opcode::I32LeU
            | Opcode::I32GeS
            | Opcode::I32GeU
            | Opcode::I64Eqz
            | Opcode::I64Eq
            | Opcode::I64Ne
            | Opcode::I64LtS
            | Opcode::I64LtU
            | Opcode::I64GtS
            | Opcode::I64GtU
            | Opcode::I64LeS
            | Opcode::I64LeU
            | Opcode::I64GeS
            | Opcode::I64GeU
            | Opcode::F32Eq
            | Opcode::F32Ne
            | Opcode::F32Lt
            | Opcode::F32Gt
            | Opcode::F32Le
            | Opcode::F32Ge
            | Opcode::F64Eq
            | Opcode::F64Ne
            | Opcode::F64Lt
            | Opcode::F64Gt
            | Opcode::F64Le
            | Opcode::F64Ge
            | Opcode::I32Clz
            | Opcode::I32Ctz
            | Opcode::I32Popcnt
            | Opcode::I32Add
            | Opcode::I32Sub
            | Opcode::I32Mul
            | Opcode::I32DivS
            | Opcode::I32DivU
            | Opcode::I32RemS
            | Opcode::I32RemU
            | Opcode::I32And
            | Opcode::I32Or
            | Opcode::I32Xor
            | Opcode::I32Shl
            | Opcode::I32ShrS
            | Opcode::I32ShrU
            | Opcode::I32Rotl
            | Opcode::I32Rotr
            | Opcode::I64Clz
            | Opcode::I64Ctz
            | Opcode::I64Popcnt
            | Opcode::I64Add
            | Opcode::I64Sub
            | Opcode::I64Mul
            | Opcode::I64DivS
            | Opcode::I64DivU
            | Opcode::I64RemS
            | Opcode::I64RemU
            | Opcode::I64And
            | Opcode::I64Or
            | Opcode::I64Xor
            | Opcode::I64Shl
            | Opcode::I64ShrS
            | Opcode::I64ShrU
            | Opcode::I64Rotl
            | Opcode::I64Rotr
            | Opcode::F32Abs
            | Opcode::F32Neg
            | Opcode::F32Ceil
            | Opcode::F32Floor
            | Opcode::F32Trunc
            | Opcode::F32Nearest
            | Opcode::F32Sqrt
            | Opcode::F32Add
            | Opcode::F32Sub
            | Opcode::F32Mul
            | Opcode::F32Div
            | Opcode::F32Min
            | Opcode::F32Max
            | Opcode::F32Copysign
            | Opcode::F64Abs
            | Opcode::F64Neg
            | Opcode::F64Ceil
            | Opcode::F64Floor
            | Opcode::F64Trunc
            | Opcode::F64Nearest
            | Opcode::F64Sqrt
            | Opcode::F64Add
            | Opcode::F64Sub
            | Opcode::F64Mul
            | Opcode::F64Div
            | Opcode::F64Min
            | Opcode::F64Max
            | Opcode::F64Copysign
            | Opcode::I32WrapI64
            | Opcode::I32TruncF32S
            | Opcode::I32TruncF32U
            | Opcode::I32TruncF64S
            | Opcode::I32TruncF64U
            | Opcode::I64ExtendI32S
            | Opcode::I64ExtendI32U
            | Opcode::I64TruncF32S
            | Opcode::I64TruncF32U
            | Opcode::I64TruncF64S
            | Opcode::I64TruncF64U
            | Opcode::F32ConvertI32S
            | Opcode::F32ConvertI32U
            | Opcode::F32ConvertI64S
            | Opcode::F32ConvertI64U
            | Opcode::F32DemoteF64
            | Opcode::F64ConvertI32S
            | Opcode::F64ConvertI32U
            | Opcode::F64ConvertI64S
            | Opcode::F64ConvertI64U
            | Opcode::F64PromoteF32
            | Opcode::I32ReinterpretF32
            | Opcode::I64ReinterpretF64
            | Opcode::F32ReinterpretI32
            | Opcode::F64ReinterpretI64
            | Opcode::MemorySize
            | Opcode::MemoryGrow
            | Opcode::MemoryCopy
            | Opcode::RefIsNull
            | Opcode::MemoryFill => OperandsType::Empty,

            Opcode::I32Load
            | Opcode::I64Load
            | Opcode::F32Load
            | Opcode::F64Load
            | Opcode::I32Load8S
            | Opcode::I32Load8U
            | Opcode::I32Load16S
            | Opcode::I32Load16U
            | Opcode::I64Load8S
            | Opcode::I64Load8U
            | Opcode::I64Load16S
            | Opcode::I64Load16U
            | Opcode::I64Load32S
            | Opcode::I64Load32U
            | Opcode::I32Store
            | Opcode::I64Store
            | Opcode::F32Store
            | Opcode::F64Store
            | Opcode::I32Store8
            | Opcode::I32Store16
            | Opcode::I64Store8
            | Opcode::I64Store16
            | Opcode::I64Store32 => OperandsType::MemArg,

            Opcode::Block | Opcode::Loop | Opcode::If => OperandsType::BlockType,

            Opcode::Br
            | Opcode::BrIf
            | Opcode::Call
            | Opcode::LocalGet
            | Opcode::LocalSet
            | Opcode::LocalTee
            | Opcode::GlobalGet
            | Opcode::GlobalSet
            | Opcode::DataDrop
            | Opcode::ElemDrop
            | Opcode::I32Const
            | Opcode::F32Const
            | Opcode::MemoryInit
            | Opcode::RefFunc
            | Opcode::TableGet
            | Opcode::TableSet
            | Opcode::TableGrow
            | Opcode::TableSize
            | Opcode::TableFill => OperandsType::FourBytes,

            Opcode::RefNull => OperandsType::RefType,

            Opcode::I64Const | Opcode::F64Const => OperandsType::U64,

            Opcode::CallIndirect | Opcode::TableCopy | Opcode::TableInit => {
                OperandsType::EightBytes
            }

            Opcode::BrTable => OperandsType::BrTable,
        }
    }

    pub fn try_from_byte(byte: u8) -> Option<Opcode> {
        Self::try_from_bytes(byte, 0)
    }

    pub fn try_from_bytes(prefix: u8, value: u32) -> Option<Opcode> {
        use Opcode as O;

        if is_prefix_byte(prefix) {
            let op = match value {
                0 => O::I32TruncF32S,
                1 => O::I32TruncF32U,
                2 => O::I32TruncF64S,
                3 => O::I32TruncF64U,
                4 => O::I64TruncF32S,
                5 => O::I64TruncF32U,
                6 => O::I64TruncF64S,
                7 => O::I64TruncF64U,
                8 => O::MemoryInit,
                9 => O::DataDrop,
                10 => O::MemoryCopy,
                11 => O::MemoryFill,
                12 => O::TableInit,
                13 => O::ElemDrop,
                14 => O::TableCopy,
                15 => O::TableGrow,
                16 => O::TableSize,
                17 => O::TableFill,
                _ => return None,
            };
            return Some(op);
        }
        assert!(value == 0);

        let op = match prefix {
            0x00 => O::Unreachable,
            0x01 => O::Nop,
            0x02 => O::Block,
            0x03 => O::Loop,
            0x04 => O::If,
            0x05 => O::Else,
            0x0b => O::End,
            0x0c => O::Br,
            0x0d => O::BrIf,
            0x0e => O::BrTable,
            0x0f => O::Return,
            0x10 => O::Call,
            0x1a => O::Drop,
            0x1b => O::Select,
            0x20 => O::LocalGet,
            0x21 => O::LocalSet,
            0x22 => O::LocalTee,
            0x23 => O::GlobalGet,
            0x24 => O::GlobalSet,
            0x25 => O::TableGet,
            0x26 => O::TableSet,
            0x28 => O::I32Load,
            0x29 => O::I64Load,
            0x2a => O::F32Load,
            0x2b => O::F64Load,
            0x2c => O::I32Load8S,
            0x2d => O::I32Load8U,
            0x2e => O::I32Load16S,
            0x2f => O::I32Load16U,
            0x30 => O::I64Load8S,
            0x31 => O::I64Load8U,
            0x32 => O::I64Load16S,
            0x33 => O::I64Load16U,
            0x34 => O::I64Load32S,
            0x35 => O::I64Load32U,
            0x36 => O::I32Store,
            0x37 => O::I64Store,
            0x38 => O::F32Store,
            0x39 => O::F64Store,
            0x3a => O::I32Store8,
            0x3b => O::I32Store16,
            0x3c => O::I64Store8,
            0x3d => O::I64Store16,
            0x3e => O::I64Store32,
            0x41 => O::I32Const,
            0x42 => O::I64Const,
            0x43 => O::F32Const,
            0x44 => O::F64Const,
            0x45 => O::I32Eqz,
            0x46 => O::I32Eq,
            0x47 => O::I32Ne,
            0x48 => O::I32LtS,
            0x49 => O::I32LtU,
            0x4a => O::I32GtS,
            0x4b => O::I32GtU,
            0x4c => O::I32LeS,
            0x4d => O::I32LeU,
            0x4e => O::I32GeS,
            0x4f => O::I32GeU,
            0x50 => O::I64Eqz,
            0x51 => O::I64Eq,
            0x52 => O::I64Ne,
            0x53 => O::I64LtS,
            0x54 => O::I64LtU,
            0x55 => O::I64GtS,
            0x56 => O::I64GtU,
            0x57 => O::I64LeS,
            0x58 => O::I64LeU,
            0x59 => O::I64GeS,
            0x5a => O::I64GeU,
            0x5b => O::F32Eq,
            0x5c => O::F32Ne,
            0x5d => O::F32Lt,
            0x5e => O::F32Gt,
            0x5f => O::F32Le,
            0x60 => O::F32Ge,
            0x61 => O::F64Eq,
            0x62 => O::F64Ne,
            0x63 => O::F64Lt,
            0x64 => O::F64Gt,
            0x65 => O::F64Le,
            0x66 => O::F64Ge,
            0x67 => O::I32Clz,
            0x68 => O::I32Ctz,
            0x69 => O::I32Popcnt,
            0x6a => O::I32Add,
            0x6b => O::I32Sub,
            0x6c => O::I32Mul,
            0x6d => O::I32DivS,
            0x6e => O::I32DivU,
            0x6f => O::I32RemS,
            0x70 => O::I32RemU,
            0x71 => O::I32And,
            0x72 => O::I32Or,
            0x73 => O::I32Xor,
            0x74 => O::I32Shl,
            0x75 => O::I32ShrS,
            0x76 => O::I32ShrU,
            0x77 => O::I32Rotl,
            0x78 => O::I32Rotr,
            0x79 => O::I64Clz,
            0x7a => O::I64Ctz,
            0x7b => O::I64Popcnt,
            0x7c => O::I64Add,
            0x7d => O::I64Sub,
            0x7e => O::I64Mul,
            0x7f => O::I64DivS,
            0x80 => O::I64DivU,
            0x81 => O::I64RemS,
            0x82 => O::I64RemU,
            0x83 => O::I64And,
            0x84 => O::I64Or,
            0x85 => O::I64Xor,
            0x86 => O::I64Shl,
            0x87 => O::I64ShrS,
            0x88 => O::I64ShrU,
            0x89 => O::I64Rotl,
            0x8a => O::I64Rotr,
            0x8b => O::F32Abs,
            0x8c => O::F32Neg,
            0x8d => O::F32Ceil,
            0x8e => O::F32Floor,
            0x8f => O::F32Trunc,
            0x90 => O::F32Nearest,
            0x91 => O::F32Sqrt,
            0x92 => O::F32Add,
            0x93 => O::F32Sub,
            0x94 => O::F32Mul,
            0x95 => O::F32Div,
            0x96 => O::F32Min,
            0x97 => O::F32Max,
            0x98 => O::F32Copysign,
            0x99 => O::F64Abs,
            0x9a => O::F64Neg,
            0x9b => O::F64Ceil,
            0x9c => O::F64Floor,
            0x9d => O::F64Trunc,
            0x9e => O::F64Nearest,
            0x9f => O::F64Sqrt,
            0xa0 => O::F64Add,
            0xa1 => O::F64Sub,
            0xa2 => O::F64Mul,
            0xa3 => O::F64Div,
            0xa4 => O::F64Min,
            0xa5 => O::F64Max,
            0xa6 => O::F64Copysign,
            0xa7 => O::I32WrapI64,
            0xa8 => O::I32TruncF32S,
            0xa9 => O::I32TruncF32U,
            0xaa => O::I32TruncF64S,
            0xab => O::I32TruncF64U,
            0xac => O::I64ExtendI32S,
            0xad => O::I64ExtendI32U,
            0xae => O::I64TruncF32S,
            0xaf => O::I64TruncF32U,
            0xb0 => O::I64TruncF64S,
            0xb1 => O::I64TruncF64U,
            0xb2 => O::F32ConvertI32S,
            0xb3 => O::F32ConvertI32U,
            0xb4 => O::F32ConvertI64S,
            0xb5 => O::F32ConvertI64U,
            0xb6 => O::F32DemoteF64,
            0xb7 => O::F64ConvertI32S,
            0xb8 => O::F64ConvertI32U,
            0xb9 => O::F64ConvertI64S,
            0xba => O::F64ConvertI64U,
            0xbb => O::F64PromoteF32,
            0xbc => O::I32ReinterpretF32,
            0xbd => O::I64ReinterpretF64,
            0xbe => O::F32ReinterpretI32,
            0xbf => O::F64ReinterpretI64,
            0xd0 => O::RefNull,
            0xd1 => O::RefIsNull,
            0xd2 => O::RefFunc,
            _ => return None,
        };

        Some(op)
    }
}

pub fn is_prefix_byte(byte: u8) -> bool {
    byte == 0xfc
}
