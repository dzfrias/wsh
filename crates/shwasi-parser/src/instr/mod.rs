mod instruction;
mod opcode;

use std::{fmt, mem};

use num_enum::TryFromPrimitive;

pub use self::instruction::Instruction;
pub use self::opcode::{is_prefix_byte, Opcode};
use crate::module::{RefType, ValType, F32, F64};

#[derive(Debug, Clone, Copy, Default, Hash)]
pub struct InstrHandle(u32);

#[derive(Debug, Clone, Default)]
pub struct InstrBuffer {
    infos: Vec<InstrInfo>,
    br_tables: Vec<BrTable>,
    selects: Vec<Vec<ValType>>,
}

#[derive(Debug, Clone, Default)]
pub struct MemArg {
    pub offset: u32,
    pub align: u32,
}

#[derive(Debug, Clone)]
pub struct BrTable {
    pub depths: Vec<u32>,
    pub default_depth: u32,
}

#[derive(Debug)]
pub struct Instrs {
    cap: usize,
    current: u32,
}

#[derive(Debug, Clone)]
struct InstrInfo {
    discriminant: u8,
    payload: u64,
}

#[derive(Debug)]
pub enum Operands {
    MemArg(MemArg),
    FourBytes(u32),
    EightBytes(u32, u32),
    BlockType(BlockType),
    RefType(RefType),
    BrTable(BrTable),
    U64(u64),
    Empty,
}

#[derive(Debug, Clone)]
pub enum BlockType {
    Empty,
    Type(ValType),
    FuncType(u32),
}

impl InstrBuffer {
    pub fn new() -> Self {
        Self {
            infos: vec![],
            br_tables: vec![],
            selects: vec![],
        }
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            infos: Vec::with_capacity(cap),
            ..Self::new()
        }
    }

    pub fn shrink(&mut self) {
        self.infos.shrink_to_fit();
    }

    pub fn opcode(&self, instr: InstrHandle) -> Opcode {
        let info = self.get_info(instr);
        // SAFETY: the discriminant is a valid opcode because opcodes discriminants match up with
        // instruction discriminants, and info.discriminant must come from a valid instruction.
        unsafe { Opcode::from_instr_discriminant(info.discriminant) }
    }

    pub fn len(&self) -> usize {
        self.infos.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn instrs(&self) -> Instrs {
        Instrs {
            cap: self.len(),
            current: 0,
        }
    }

    pub fn last(&self) -> Option<InstrHandle> {
        if self.is_empty() {
            return None;
        }
        Some(InstrHandle(self.len() as u32 - 1))
    }

    pub fn first(&self) -> Option<InstrHandle> {
        if self.is_empty() {
            return None;
        }
        Some(InstrHandle(0))
    }

    pub fn get(&self, n: usize) -> Option<InstrHandle> {
        if n >= self.len() {
            return None;
        }

        Some(InstrHandle(n as u32))
    }

    pub fn add_instr(&mut self, instr: Instruction) {
        // The instruction's discriminant (a u8) is used to encode the instruction type. This will
        // differentiate between two instrucitons and their payloads. It can later be turned into
        // an Opcode through casting.
        let discriminant = instr.discriminant();

        let encoding = match instr {
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
            | Instruction::I32TruncSatF32S
            | Instruction::I32TruncSatF32U
            | Instruction::I32TruncSatF64S
            | Instruction::I32TruncSatF64U
            | Instruction::I64TruncSatF32S
            | Instruction::I64TruncSatF32U
            | Instruction::I64TruncSatF64S
            | Instruction::I64TruncSatF64U
            | Instruction::F64ReinterpretI64
            | Instruction::MemorySize
            | Instruction::MemoryGrow
            | Instruction::MemoryCopy
            | Instruction::RefIsNull
            | Instruction::MemoryFill
            | Instruction::I32Extend8S
            | Instruction::I32Extend16S
            | Instruction::I64Extend8S
            | Instruction::I64Extend16S
            | Instruction::I64Extend32S => 0,

            Instruction::I32Load(n)
            | Instruction::I64Load(n)
            | Instruction::F32Load(n)
            | Instruction::F64Load(n)
            | Instruction::I32Load8S(n)
            | Instruction::I32Load8U(n)
            | Instruction::I32Load16S(n)
            | Instruction::I32Load16U(n)
            | Instruction::I64Load8S(n)
            | Instruction::I64Load8U(n)
            | Instruction::I64Load16S(n)
            | Instruction::I64Load16U(n)
            | Instruction::I64Load32S(n)
            | Instruction::I64Load32U(n)
            | Instruction::I32Store(n)
            | Instruction::I64Store(n)
            | Instruction::F32Store(n)
            | Instruction::F64Store(n)
            | Instruction::I32Store8(n)
            | Instruction::I32Store16(n)
            | Instruction::I64Store8(n)
            | Instruction::I64Store16(n)
            | Instruction::I64Store32(n) => unsafe { mem::transmute(n) },

            Instruction::Block(block) | Instruction::Loop(block) | Instruction::If(block) => unsafe {
                mem::transmute(block)
            },

            Instruction::Br { depth: n }
            | Instruction::BrIf { depth: n }
            | Instruction::Call { func_idx: n }
            | Instruction::LocalGet { idx: n }
            | Instruction::LocalSet { idx: n }
            | Instruction::LocalTee { idx: n }
            | Instruction::GlobalGet { idx: n }
            | Instruction::GlobalSet { idx: n }
            | Instruction::DataDrop { data_idx: n }
            | Instruction::ElemDrop { elem_idx: n }
            | Instruction::MemoryInit { data_idx: n }
            | Instruction::RefFunc { func_idx: n }
            | Instruction::TableGet { table: n }
            | Instruction::TableSet { table: n }
            | Instruction::TableGrow { table: n }
            | Instruction::TableSize { table: n }
            | Instruction::TableFill { table: n } => n as u64,

            Instruction::F32Const(n) => n.raw() as u64,
            Instruction::F64Const(n) => n.raw(),

            Instruction::RefNull { ty } => ty as u64,

            // SAFETY: an u32 can be interpreted as an i32, just as long
            // as the u32 is only being used as an encoding key (which it is)
            Instruction::I32Const(n) => (unsafe { std::mem::transmute::<_, u32>(n) }) as u64,
            Instruction::I64Const(n) => unsafe { std::mem::transmute(n) },

            Instruction::CallIndirect {
                type_idx: b1,
                table_idx: b2,
            }
            | Instruction::TableCopy {
                src_table: b1,
                dst_table: b2,
            }
            | Instruction::TableInit {
                elem_idx: b1,
                table_idx: b2,
            } => (b1 as u64) << 32 | (b2 as u64),

            Instruction::BrTable(br_table) => {
                self.br_tables.push(br_table);
                self.br_tables.len() as u64 - 1
            }
            Instruction::SelectT(types) => {
                self.selects.push(types);
                self.selects.len() as u64 - 1
            }
        };

        self.infos.push(InstrInfo {
            discriminant,
            payload: encoding,
        });
    }

    pub fn instruction(&self, instr: InstrHandle) -> Instruction {
        let info = self.get_info(instr);

        // SAFETY: the discriminant is a valid opcode because opcodes discriminants match up with
        // instruction discriminants, and info.discriminant must come from a valid instruction.
        match unsafe { Opcode::from_instr_discriminant(info.discriminant) } {
            Opcode::Unreachable => Instruction::Unreachable,
            Opcode::Nop => Instruction::Nop,
            Opcode::Else => Instruction::Else,
            Opcode::End => Instruction::End,
            Opcode::Return => Instruction::Return,
            Opcode::Drop => Instruction::Drop,
            Opcode::Select => Instruction::Select,
            Opcode::I32Eqz => Instruction::I32Eqz,
            Opcode::I32Eq => Instruction::I32Eq,
            Opcode::I32Ne => Instruction::I32Ne,
            Opcode::I32LtS => Instruction::I32LtS,
            Opcode::I32LtU => Instruction::I32LtU,
            Opcode::I32GtS => Instruction::I32GtS,
            Opcode::I32GtU => Instruction::I32GtU,
            Opcode::I32LeS => Instruction::I32LeS,
            Opcode::I32LeU => Instruction::I32LeU,
            Opcode::I32GeS => Instruction::I32GeS,
            Opcode::I32GeU => Instruction::I32GeU,
            Opcode::I64Eqz => Instruction::I64Eqz,
            Opcode::I64Eq => Instruction::I64Eq,
            Opcode::I64Ne => Instruction::I64Ne,
            Opcode::I64LtS => Instruction::I64LtS,
            Opcode::I64LtU => Instruction::I64LtU,
            Opcode::I64GtS => Instruction::I64GtS,
            Opcode::I64GtU => Instruction::I64GtU,
            Opcode::I64LeS => Instruction::I64LeS,
            Opcode::I64LeU => Instruction::I64LeU,
            Opcode::I64GeS => Instruction::I64GeS,
            Opcode::I64GeU => Instruction::I64GeU,
            Opcode::F32Eq => Instruction::F32Eq,
            Opcode::F32Ne => Instruction::F32Ne,
            Opcode::F32Lt => Instruction::F32Lt,
            Opcode::F32Gt => Instruction::F32Gt,
            Opcode::F32Le => Instruction::F32Le,
            Opcode::F32Ge => Instruction::F32Ge,
            Opcode::F64Eq => Instruction::F64Eq,
            Opcode::F64Ne => Instruction::F64Ne,
            Opcode::F64Lt => Instruction::F64Lt,
            Opcode::F64Gt => Instruction::F64Gt,
            Opcode::F64Le => Instruction::F64Le,
            Opcode::F64Ge => Instruction::F64Ge,
            Opcode::I32Clz => Instruction::I32Clz,
            Opcode::I32Ctz => Instruction::I32Ctz,
            Opcode::I32Popcnt => Instruction::I32Popcnt,
            Opcode::I32Add => Instruction::I32Add,
            Opcode::I32Sub => Instruction::I32Sub,
            Opcode::I32Mul => Instruction::I32Mul,
            Opcode::I32DivS => Instruction::I32DivS,
            Opcode::I32DivU => Instruction::I32DivU,
            Opcode::I32RemS => Instruction::I32RemS,
            Opcode::I32RemU => Instruction::I32RemU,
            Opcode::I32And => Instruction::I32And,
            Opcode::I32Or => Instruction::I32Or,
            Opcode::I32Xor => Instruction::I32Xor,
            Opcode::I32Shl => Instruction::I32Shl,
            Opcode::I32ShrS => Instruction::I32ShrS,
            Opcode::I32ShrU => Instruction::I32ShrU,
            Opcode::I32Rotl => Instruction::I32Rotl,
            Opcode::I32Rotr => Instruction::I32Rotr,
            Opcode::I64Clz => Instruction::I64Clz,
            Opcode::I64Ctz => Instruction::I64Ctz,
            Opcode::I64Popcnt => Instruction::I64Popcnt,
            Opcode::I64Add => Instruction::I64Add,
            Opcode::I64Sub => Instruction::I64Sub,
            Opcode::I64Mul => Instruction::I64Mul,
            Opcode::I64DivS => Instruction::I64DivS,
            Opcode::I64DivU => Instruction::I64DivU,
            Opcode::I64RemS => Instruction::I64RemS,
            Opcode::I64RemU => Instruction::I64RemU,
            Opcode::I64And => Instruction::I64And,
            Opcode::I64Or => Instruction::I64Or,
            Opcode::I64Xor => Instruction::I64Xor,
            Opcode::I64Shl => Instruction::I64Shl,
            Opcode::I64ShrS => Instruction::I64ShrS,
            Opcode::I64ShrU => Instruction::I64ShrU,
            Opcode::I64Rotl => Instruction::I64Rotl,
            Opcode::I64Rotr => Instruction::I64Rotr,
            Opcode::F32Abs => Instruction::F32Abs,
            Opcode::F32Neg => Instruction::F32Neg,
            Opcode::F32Ceil => Instruction::F32Ceil,
            Opcode::F32Floor => Instruction::F32Floor,
            Opcode::F32Trunc => Instruction::F32Trunc,
            Opcode::F32Nearest => Instruction::F32Nearest,
            Opcode::F32Sqrt => Instruction::F32Sqrt,
            Opcode::F32Add => Instruction::F32Add,
            Opcode::F32Sub => Instruction::F32Sub,
            Opcode::F32Mul => Instruction::F32Mul,
            Opcode::F32Div => Instruction::F32Div,
            Opcode::F32Min => Instruction::F32Min,
            Opcode::F32Max => Instruction::F32Max,
            Opcode::F32Copysign => Instruction::F32Copysign,
            Opcode::F64Abs => Instruction::F64Abs,
            Opcode::F64Neg => Instruction::F64Neg,
            Opcode::F64Ceil => Instruction::F64Ceil,
            Opcode::F64Floor => Instruction::F64Floor,
            Opcode::F64Trunc => Instruction::F64Trunc,
            Opcode::F64Nearest => Instruction::F64Nearest,
            Opcode::F64Sqrt => Instruction::F64Sqrt,
            Opcode::F64Add => Instruction::F64Add,
            Opcode::F64Sub => Instruction::F64Sub,
            Opcode::F64Mul => Instruction::F64Mul,
            Opcode::F64Div => Instruction::F64Div,
            Opcode::F64Min => Instruction::F64Min,
            Opcode::F64Max => Instruction::F64Max,
            Opcode::F64Copysign => Instruction::F64Copysign,
            Opcode::I32WrapI64 => Instruction::I32WrapI64,
            Opcode::I32TruncF32S => Instruction::I32TruncF32S,
            Opcode::I32TruncF32U => Instruction::I32TruncF32U,
            Opcode::I32TruncF64S => Instruction::I32TruncF64S,
            Opcode::I32TruncF64U => Instruction::I32TruncF64U,
            Opcode::I64ExtendI32S => Instruction::I64ExtendI32S,
            Opcode::I64ExtendI32U => Instruction::I64ExtendI32U,
            Opcode::I64TruncF32S => Instruction::I64TruncF32S,
            Opcode::I64TruncF32U => Instruction::I64TruncF32U,
            Opcode::I64TruncF64S => Instruction::I64TruncF64S,
            Opcode::I64TruncF64U => Instruction::I64TruncF64U,
            Opcode::F32ConvertI32S => Instruction::F32ConvertI32S,
            Opcode::F32ConvertI32U => Instruction::F32ConvertI32U,
            Opcode::F32ConvertI64S => Instruction::F32ConvertI64S,
            Opcode::F32ConvertI64U => Instruction::F32ConvertI64U,
            Opcode::F32DemoteF64 => Instruction::F32DemoteF64,
            Opcode::F64ConvertI32S => Instruction::F64ConvertI32S,
            Opcode::F64ConvertI32U => Instruction::F64ConvertI32U,
            Opcode::F64ConvertI64S => Instruction::F64ConvertI64S,
            Opcode::F64ConvertI64U => Instruction::F64ConvertI64U,
            Opcode::F64PromoteF32 => Instruction::F64PromoteF32,
            Opcode::I32ReinterpretF32 => Instruction::I32ReinterpretF32,
            Opcode::I64ReinterpretF64 => Instruction::I64ReinterpretF64,
            Opcode::F32ReinterpretI32 => Instruction::F32ReinterpretI32,
            Opcode::F64ReinterpretI64 => Instruction::F64ReinterpretI64,
            Opcode::MemorySize => Instruction::MemorySize,
            Opcode::MemoryGrow => Instruction::MemoryGrow,
            Opcode::MemoryCopy => Instruction::MemoryCopy,
            Opcode::RefIsNull => Instruction::RefIsNull,
            Opcode::MemoryFill => Instruction::MemoryFill,
            Opcode::I32TruncSatF32S => Instruction::I32TruncSatF32S,
            Opcode::I32TruncSatF32U => Instruction::I32TruncSatF32U,
            Opcode::I32TruncSatF64S => Instruction::I32TruncSatF64S,
            Opcode::I32TruncSatF64U => Instruction::I32TruncSatF64U,
            Opcode::I64TruncSatF32S => Instruction::I64TruncSatF32S,
            Opcode::I64TruncSatF32U => Instruction::I64TruncSatF32U,
            Opcode::I64TruncSatF64S => Instruction::I64TruncSatF64S,
            Opcode::I64TruncSatF64U => Instruction::I64TruncSatF64U,

            Opcode::Block => Instruction::Block(unsafe { mem::transmute(info.payload) }),
            Opcode::Loop => Instruction::Loop(unsafe { mem::transmute(info.payload) }),
            Opcode::If => Instruction::If(unsafe { mem::transmute(info.payload) }),

            Opcode::I32Load => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::I32Load(mem_arg)
            }
            Opcode::I64Load => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::I64Load(mem_arg)
            }
            Opcode::F32Load => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::F32Load(mem_arg)
            }
            Opcode::F64Load => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::F64Load(mem_arg)
            }
            Opcode::I32Load8S => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::I32Load8S(mem_arg)
            }
            Opcode::I32Load8U => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::I32Load8U(mem_arg)
            }
            Opcode::I32Load16S => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::I32Load16S(mem_arg)
            }
            Opcode::I32Load16U => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::I32Load16U(mem_arg)
            }
            Opcode::I64Load8S => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::I64Load8S(mem_arg)
            }
            Opcode::I64Load8U => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::I64Load8U(mem_arg)
            }
            Opcode::I64Load16S => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::I64Load16S(mem_arg)
            }
            Opcode::I64Load16U => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::I64Load16U(mem_arg)
            }
            Opcode::I64Load32S => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::I64Load32S(mem_arg)
            }
            Opcode::I64Load32U => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::I64Load32U(mem_arg)
            }
            Opcode::I32Store => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::I32Store(mem_arg)
            }
            Opcode::I64Store => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::I64Store(mem_arg)
            }
            Opcode::F32Store => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::F32Store(mem_arg)
            }
            Opcode::F64Store => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::F64Store(mem_arg)
            }
            Opcode::I32Store8 => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::I32Store8(mem_arg)
            }
            Opcode::I32Store16 => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::I32Store16(mem_arg)
            }
            Opcode::I64Store8 => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::I64Store8(mem_arg)
            }
            Opcode::I64Store16 => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::I64Store16(mem_arg)
            }
            Opcode::I64Store32 => {
                let mem_arg = unsafe { mem::transmute(info.payload) };
                Instruction::I64Store32(mem_arg)
            }

            Opcode::Br => Instruction::Br {
                depth: info.payload as u32,
            },
            Opcode::BrIf => Instruction::BrIf {
                depth: info.payload as u32,
            },
            Opcode::Call => Instruction::Call {
                func_idx: info.payload as u32,
            },

            Opcode::BrTable => {
                let br_table = &self.br_tables[info.payload as usize];
                Instruction::BrTable(br_table.clone())
            }

            Opcode::LocalGet => Instruction::LocalGet {
                idx: info.payload as u32,
            },
            Opcode::LocalSet => Instruction::LocalSet {
                idx: info.payload as u32,
            },
            Opcode::LocalTee => Instruction::LocalTee {
                idx: info.payload as u32,
            },
            Opcode::GlobalGet => Instruction::GlobalGet {
                idx: info.payload as u32,
            },
            Opcode::GlobalSet => Instruction::GlobalSet {
                idx: info.payload as u32,
            },
            Opcode::DataDrop => Instruction::DataDrop {
                data_idx: info.payload as u32,
            },
            Opcode::ElemDrop => Instruction::ElemDrop {
                elem_idx: info.payload as u32,
            },
            // SAFETY: the u32 can be easily decoded back into an i32
            Opcode::I32Const => {
                Instruction::I32Const(unsafe { std::mem::transmute(info.payload as u32) })
            }
            Opcode::F32Const => Instruction::F32Const(F32::new(info.payload as u32)),
            Opcode::MemoryInit => Instruction::MemoryInit {
                data_idx: info.payload as u32,
            },
            Opcode::RefFunc => Instruction::RefFunc {
                func_idx: info.payload as u32,
            },
            Opcode::TableGet => Instruction::TableGet {
                table: info.payload as u32,
            },
            Opcode::TableSet => Instruction::TableSet {
                table: info.payload as u32,
            },
            Opcode::TableGrow => Instruction::TableGrow {
                table: info.payload as u32,
            },
            Opcode::TableSize => Instruction::TableSize {
                table: info.payload as u32,
            },
            Opcode::TableFill => Instruction::TableFill {
                table: info.payload as u32,
            },

            Opcode::CallIndirect => {
                let u64 = info.payload;
                let type_idx = (u64 >> 32) as u32;
                let table_idx = (u64 & 0xFFFFFFFF) as u32;
                Instruction::CallIndirect {
                    type_idx,
                    table_idx,
                }
            }
            // SAFETY: the u64 can be easily decoded back into an i64
            Opcode::I64Const => Instruction::I64Const(unsafe { std::mem::transmute(info.payload) }),
            Opcode::F64Const => Instruction::F64Const(F64::new(info.payload)),
            Opcode::TableInit => {
                let u64 = info.payload;
                let elem_idx = (u64 >> 32) as u32;
                let table_idx = (u64 & 0xFFFFFFFF) as u32;
                Instruction::TableInit {
                    elem_idx,
                    table_idx,
                }
            }
            Opcode::TableCopy => {
                let u64 = info.payload;
                let src = (u64 >> 32) as u32;
                let dst = (u64 & 0xFFFFFFFF) as u32;
                Instruction::TableCopy {
                    src_table: src,
                    dst_table: dst,
                }
            }
            Opcode::RefNull => {
                let ty = RefType::try_from_primitive(info.payload as u8).unwrap();
                Instruction::RefNull { ty }
            }
            Opcode::I32Extend8S => Instruction::I32Extend8S,
            Opcode::I32Extend16S => Instruction::I32Extend16S,
            Opcode::I64Extend8S => Instruction::I64Extend8S,
            Opcode::I64Extend16S => Instruction::I64Extend16S,
            Opcode::I64Extend32S => Instruction::I64Extend32S,
            Opcode::SelectT => {
                let types = &self.selects[info.payload as usize];
                Instruction::SelectT(types.clone())
            }
        }
    }

    fn get_info(&self, instr: InstrHandle) -> &InstrInfo {
        &self.infos[instr.0 as usize]
    }
}

impl Iterator for Instrs {
    type Item = InstrHandle;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cap == self.current as usize {
            return None;
        }

        let instr = InstrHandle(self.current);
        self.current += 1;
        Some(instr)
    }
}

impl fmt::Display for InstrBuffer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut indent = String::new();
        for instr in self.instrs() {
            let instruction = self.instruction(instr);

            if self.opcode(instr) == Opcode::End {
                indent.truncate(indent.len().saturating_sub(2));
            }

            writeln!(f, "{indent}{instruction}")?;

            if matches!(
                self.opcode(instr),
                Opcode::If | Opcode::Block | Opcode::Loop
            ) {
                indent.push_str("  ");
            }
        }

        Ok(())
    }
}
