mod instruction;
mod opcode;

use std::{fmt, iter::FusedIterator, mem, ops::Range, sync::Arc};

use num_enum::TryFromPrimitive;

pub use self::instruction::Instruction;
pub use self::opcode::{is_prefix_byte, Opcode};
use crate::{module::ValType, RefType, F32, F64};

/// A buffer containing [`Instruction`]s.
#[derive(Clone, Default)]
pub struct InstrBuffer {
    infos: Vec<InstrInfo>,
    br_tables: Vec<BrTable>,
    u64s: Vec<u64>,
    blocks: Vec<Block>,
    block_elses: Vec<(Block, Option<usize>)>,
}

/// A memory argument for a load or store instruction.
///
/// See [the spec](https://webassembly.github.io/spec/core/binary/instructions.html#memory-instructions)
/// for more information on the instructions that use this.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct MemArg {
    pub offset: u32,
    pub align: u32,
}

/// The immediates of a [`Instruction::BrTable`].
#[derive(Debug, Clone, PartialEq)]
pub struct BrTable {
    pub depths: Arc<[u32]>,
    pub default_depth: u32,
}

/// An iterator over the instructions in an [`InstrBuffer`].
#[derive(Debug)]
pub struct InstrsIter<'a> {
    buffer: &'a InstrBuffer,
    cap: usize,
    current: usize,
}

/// The type of a block-related instruction.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BlockType {
    Empty,
    Type(ValType),
    FuncType(u32),
}

/// The immediate of a [`Instruction::Block`], [`Instruction::Loop`], or [`Instruction::If`].
///
/// Note that `end` does not appear in the spec. It is used to keep track of the end of the block,
/// and is an index into the [`InstrBuffer`].
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub ty: BlockType,
    pub end: usize,
}

/// Internally used in order to encode instructions efficiently in the buffer.
#[derive(Debug, Clone)]
pub struct InstrInfo {
    pub opcode: Opcode,
    /// The payload of the instruction. This is used to encode the immediates of an instruction.
    ///
    /// Instructions with no immediates will have a payload of 0. Instructions with a **four-bytes**
    /// or less immediate will have their payload be the immediate.
    ///
    /// Otherwise, `payload` is a pointer to a different vector in the buffer that contains the
    /// information.
    pub payload: u32,
}

impl InstrBuffer {
    /// Create a new, empty [`InstrBuffer`].
    pub fn new() -> Self {
        Self {
            infos: vec![],
            br_tables: vec![],
            u64s: vec![],
            blocks: vec![],
            block_elses: vec![],
        }
    }

    /// Create a new [`InstrBuffer`] with the given [`Instruction`] capacity.
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            infos: Vec::with_capacity(cap),
            ..Self::new()
        }
    }

    /// Shrink the capacity of the buffer to fit its length.
    pub fn shrink(&mut self) {
        self.infos.shrink_to_fit();
    }

    /// Get the number of instructions in the buffer.
    #[inline]
    pub fn len(&self) -> usize {
        self.infos.len()
    }

    /// Check if the buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Create an iterator over the instructions in the buffer.
    pub fn iter(&self) -> InstrsIter {
        InstrsIter {
            buffer: self,
            cap: self.len(),
            current: 0,
        }
    }

    /// Get the last instruction in the buffer.
    pub fn last(&self) -> Option<Instruction> {
        self.get(self.len() - 1)
    }

    /// Get the first instruction in the buffer.
    pub fn first(&self) -> Option<Instruction> {
        self.get(0)
    }

    /// Push an instruction into to the buffer.
    pub fn push(&mut self, instr: Instruction) {
        // The instruction's discriminant (a u8) is used to encode the instruction type. This will
        // differentiate between two instrucitons and their payloads. It can later be turned into
        // an Opcode through casting.
        let opcode = instr.opcode();

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
            | Instruction::I64Store32(n) => unsafe { self.alloc_u64(mem::transmute(n)) },

            Instruction::Block(block) | Instruction::Loop(block) => self.alloc_block(block),
            Instruction::If { block, else_ } => {
                self.block_elses.push((block, else_));
                self.block_elses.len() as u32 - 1
            }

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
            | Instruction::TableFill { table: n }
            | Instruction::I32Const(n) => n,

            Instruction::F32Const(n) => n.raw(),
            Instruction::F64Const(n) => self.alloc_u64(n.raw()),

            Instruction::RefNull { ty } => ty as u32,

            Instruction::I64Const(n) => self.alloc_u64(n),

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
            } => {
                let u64 = (b1 as u64) << 32 | (b2 as u64);
                self.alloc_u64(u64)
            }

            Instruction::BrTable(br_table) => {
                self.br_tables.push(br_table);
                self.br_tables.len() as u32 - 1
            }
            Instruction::SelectT(ty) => ty as u32,
        };

        self.infos.push(InstrInfo {
            opcode,
            payload: encoding,
        });
    }

    /// Get an instruction at the given index.
    ///
    /// This method will return [`None`] if the index is out of bounds.
    pub fn get(&self, idx: usize) -> Option<Instruction> {
        if idx >= self.len() {
            return None;
        }
        // SAFETY: we just verified that the index is within the bounds
        Some(unsafe { self.get_unchecked(idx) })
    }

    /// This is not the recommended way to access items in the buffer, and only should be used for
    /// the most critical performance cases. See `get` and `get_unchecked` for variants that return
    /// `Instruction`s.
    ///
    /// # Safety
    /// This function has undefined behavior when `idx >= self.len()`.
    #[inline(always)]
    pub unsafe fn get_info_unchecked(&self, idx: usize) -> &InstrInfo {
        unsafe { self.infos.get_unchecked(idx) }
    }

    /// Get an instruction from the buffer.
    ///
    /// # Safety
    /// This function has undefined behavior when `idx >= self.len()`.
    pub unsafe fn get_unchecked(&self, idx: usize) -> Instruction {
        let info = self.infos.get_unchecked(idx);

        match info.opcode {
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

            Opcode::Block => Instruction::Block(self.blocks[info.payload as usize].clone()),
            Opcode::Loop => Instruction::Loop(self.blocks[info.payload as usize].clone()),
            Opcode::If => {
                let (block, else_) = self.block_elses[info.payload as usize].clone();
                Instruction::If { block, else_ }
            }

            Opcode::I32Load => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::I32Load(mem_arg)
            }
            Opcode::I64Load => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::I64Load(mem_arg)
            }
            Opcode::F32Load => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::F32Load(mem_arg)
            }
            Opcode::F64Load => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::F64Load(mem_arg)
            }
            Opcode::I32Load8S => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::I32Load8S(mem_arg)
            }
            Opcode::I32Load8U => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::I32Load8U(mem_arg)
            }
            Opcode::I32Load16S => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::I32Load16S(mem_arg)
            }
            Opcode::I32Load16U => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::I32Load16U(mem_arg)
            }
            Opcode::I64Load8S => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::I64Load8S(mem_arg)
            }
            Opcode::I64Load8U => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::I64Load8U(mem_arg)
            }
            Opcode::I64Load16S => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::I64Load16S(mem_arg)
            }
            Opcode::I64Load16U => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::I64Load16U(mem_arg)
            }
            Opcode::I64Load32S => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::I64Load32S(mem_arg)
            }
            Opcode::I64Load32U => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::I64Load32U(mem_arg)
            }
            Opcode::I32Store => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::I32Store(mem_arg)
            }
            Opcode::I64Store => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::I64Store(mem_arg)
            }
            Opcode::F32Store => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::F32Store(mem_arg)
            }
            Opcode::F64Store => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::F64Store(mem_arg)
            }
            Opcode::I32Store8 => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::I32Store8(mem_arg)
            }
            Opcode::I32Store16 => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::I32Store16(mem_arg)
            }
            Opcode::I64Store8 => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::I64Store8(mem_arg)
            }
            Opcode::I64Store16 => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::I64Store16(mem_arg)
            }
            Opcode::I64Store32 => {
                let mem_arg = unsafe { mem::transmute(self.u64s[info.payload as usize]) };
                Instruction::I64Store32(mem_arg)
            }

            Opcode::Br => Instruction::Br {
                depth: info.payload,
            },
            Opcode::BrIf => Instruction::BrIf {
                depth: info.payload,
            },
            Opcode::Call => Instruction::Call {
                func_idx: info.payload,
            },

            Opcode::BrTable => {
                let br_table = &self.br_tables[info.payload as usize];
                Instruction::BrTable(br_table.clone())
            }

            Opcode::LocalGet => Instruction::LocalGet { idx: info.payload },
            Opcode::LocalSet => Instruction::LocalSet { idx: info.payload },
            Opcode::LocalTee => Instruction::LocalTee { idx: info.payload },
            Opcode::GlobalGet => Instruction::GlobalGet { idx: info.payload },
            Opcode::GlobalSet => Instruction::GlobalSet { idx: info.payload },
            Opcode::DataDrop => Instruction::DataDrop {
                data_idx: info.payload,
            },
            Opcode::ElemDrop => Instruction::ElemDrop {
                elem_idx: info.payload,
            },
            // SAFETY: the u32 can be easily decoded back into an i32
            Opcode::I32Const => Instruction::I32Const(info.payload),
            Opcode::F32Const => Instruction::F32Const(F32::new(info.payload)),
            Opcode::MemoryInit => Instruction::MemoryInit {
                data_idx: info.payload,
            },
            Opcode::RefFunc => Instruction::RefFunc {
                func_idx: info.payload,
            },
            Opcode::TableGet => Instruction::TableGet {
                table: info.payload,
            },
            Opcode::TableSet => Instruction::TableSet {
                table: info.payload,
            },
            Opcode::TableGrow => Instruction::TableGrow {
                table: info.payload,
            },
            Opcode::TableSize => Instruction::TableSize {
                table: info.payload,
            },
            Opcode::TableFill => Instruction::TableFill {
                table: info.payload,
            },

            Opcode::CallIndirect => {
                let u64 = self.u64s[info.payload as usize];
                let type_idx = (u64 >> 32) as u32;
                let table_idx = (u64 & 0xFFFFFFFF) as u32;
                Instruction::CallIndirect {
                    type_idx,
                    table_idx,
                }
            }
            Opcode::I64Const => Instruction::I64Const(self.u64s[info.payload as usize]),
            Opcode::F64Const => Instruction::F64Const(F64::new(self.u64s[info.payload as usize])),
            Opcode::TableInit => {
                let u64 = self.u64s[info.payload as usize];
                let elem_idx = (u64 >> 32) as u32;
                let table_idx = (u64 & 0xFFFFFFFF) as u32;
                Instruction::TableInit {
                    elem_idx,
                    table_idx,
                }
            }
            Opcode::TableCopy => {
                let u64 = self.u64s[info.payload as usize];
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
                let ty = ValType::try_from_primitive(info.payload as u8).unwrap();
                Instruction::SelectT(ty)
            }
        }
    }

    pub fn slice(&self, range: Range<usize>) -> InstrBufferRef {
        InstrBufferRef::new(self, range)
    }

    pub fn get_ref(&self) -> InstrBufferRef {
        self.slice(0..self.len())
    }

    /// Patch the `end` pointer of an instruction, by index.
    ///
    /// # Panics
    /// Panics if the instruction at `idx` is not an [`Instruction::Block`], [`Instruction::If`],
    /// or [`Instruction::Loop`].
    pub fn patch_end(&mut self, idx: usize, new_end: usize) {
        let info = &self.infos[idx];
        match info.opcode {
            Opcode::Block | Opcode::Loop => {
                let block = &mut self.blocks[info.payload as usize];
                block.end = new_end;
            }
            Opcode::If => {
                let (block, _) = &mut self.block_elses[info.payload as usize];
                block.end = new_end;
            }
            _ => panic!("cannot patch end of {}", info.opcode),
        }
    }

    /// Patch the `else` pointer of an instruction, by index.
    ///
    /// # Panics
    /// Panics if the instruction at `idx` is not an [`Instruction::If`].
    pub fn patch_else(&mut self, idx: usize, new_else_: usize) {
        let info = &self.infos[idx];
        match info.opcode {
            Opcode::If => {
                let (_, else_) = &mut self.block_elses[info.payload as usize];
                *else_ = Some(new_else_);
            }
            _ => panic!("cannot patch else of {}", info.opcode),
        }
    }

    /// # Safety
    /// This function should only be called with memory instructions.
    #[inline(always)]
    pub unsafe fn get_memarg(&self, payload: u32) -> MemArg {
        unsafe { std::mem::transmute(*self.u64s.get_unchecked(payload as usize)) }
    }

    /// # Safety
    /// This function should only be called with block instructions.
    #[inline(always)]
    pub unsafe fn get_block(&self, payload: u32) -> &Block {
        unsafe { self.blocks.get_unchecked(payload as usize) }
    }

    /// # Safety
    /// This function should only be called with the [`Opcode::If`] instruction.
    #[inline(always)]
    pub unsafe fn get_block_else(&self, payload: u32) -> &(Block, Option<usize>) {
        unsafe { self.block_elses.get_unchecked(payload as usize) }
    }

    /// # Safety
    /// This function should only be called with instructions that involve two u32's as their data.
    #[inline(always)]
    pub unsafe fn get_pair(&self, payload: u32) -> (u32, u32) {
        let u64 = self.u64s[payload as usize];
        let p1 = (u64 >> 32) as u32;
        let p2 = (u64 & 0xFFFFFFFF) as u32;
        (p2, p1)
    }

    /// # Safety
    /// This function should only be called with the [`Opcode::BrTable`] instruction.
    #[inline(always)]
    pub unsafe fn get_br_table(&self, payload: u32) -> BrTable {
        unsafe { self.br_tables.get_unchecked(payload as usize) }.clone()
    }

    /// # Safety
    /// This function should only be called with u64 instructions.
    #[inline(always)]
    pub unsafe fn get_u64(&self, payload: u32) -> u64 {
        unsafe { *self.u64s.get_unchecked(payload as usize) }
    }

    #[inline(always)]
    fn alloc_u64(&mut self, u64: u64) -> u32 {
        self.u64s.push(u64);
        self.u64s.len() as u32 - 1
    }

    #[inline(always)]
    fn alloc_block(&mut self, block: Block) -> u32 {
        self.blocks.push(block);
        self.blocks.len() as u32 - 1
    }
}

#[derive(Clone)]
pub struct InstrBufferRef<'a> {
    buffer: &'a InstrBuffer,
    range: Range<usize>,
}

impl<'a> InstrBufferRef<'a> {
    fn new(buffer: &'a InstrBuffer, range: Range<usize>) -> Self {
        Self { buffer, range }
    }

    pub fn len(&self) -> usize {
        self.range.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn get(&self, idx: usize) -> Option<Instruction> {
        self.buffer.get(self.range.start + idx)
    }

    /// Get an instruction from the buffer.
    ///
    /// # Safety
    /// This function has undefined behavior when `idx >= self.len()`.
    pub unsafe fn get_unchecked(&self, idx: usize) -> Instruction {
        self.buffer.get_unchecked(self.range.start + idx)
    }

    pub fn inner(&self) -> &'a InstrBuffer {
        self.buffer
    }

    pub fn start(&self) -> usize {
        self.range.start
    }

    pub fn slice(&self, range: Range<usize>) -> InstrBufferRef {
        let start = self.range.start + range.start;
        let end = self.range.start + range.end;
        InstrBufferRef::new(self.buffer, start..end)
    }

    pub fn first(&self) -> Option<Instruction> {
        self.get(0)
    }

    pub fn last(&self) -> Option<Instruction> {
        self.get(self.len() - 1)
    }

    pub fn iter(&self) -> InstrsIter<'a> {
        InstrsIter {
            buffer: self.buffer,
            cap: self.range.end,
            current: self.range.start,
        }
    }
}

impl fmt::Debug for InstrBuffer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list()
            .entries((0..self.len()).map(|i| self.get(i).unwrap()))
            .finish()
    }
}

impl fmt::Debug for InstrBufferRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list()
            .entries((0..self.len()).map(|i| self.get(i).unwrap()))
            .finish()
    }
}

impl<'a> Iterator for InstrsIter<'a> {
    type Item = Instruction;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cap == self.current {
            return None;
        }

        let instr = self.current;
        self.current += 1;
        // SAFETY: we know that the length could not have changed, so instr is within the bounds.
        Some(unsafe { self.buffer.get_unchecked(instr) })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl FusedIterator for InstrsIter<'_> {}

impl ExactSizeIterator for InstrsIter<'_> {
    fn len(&self) -> usize {
        self.cap - self.current
    }
}

impl DoubleEndedIterator for InstrsIter<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.cap == self.current {
            return None;
        }

        self.cap -= 1;
        // SAFETY: this will be within the bounds, as self.current can never exceed the length, and
        // cap can never exceed it either (since we know the length is unchanged since the creation
        // of this struct).
        Some(unsafe { self.buffer.get_unchecked(self.cap) })
    }
}

impl<'a> IntoIterator for &'a InstrBufferRef<'a> {
    type Item = Instruction;
    type IntoIter = InstrsIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a> IntoIterator for &'a InstrBuffer {
    type Item = Instruction;
    type IntoIter = InstrsIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a> IntoIterator for &'a mut InstrBuffer {
    type Item = Instruction;
    type IntoIter = InstrsIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl FromIterator<Instruction> for InstrBuffer {
    fn from_iter<T: IntoIterator<Item = Instruction>>(iter: T) -> Self {
        let iter = iter.into_iter();
        let size_hint = iter.size_hint();
        let mut buffer = Self::with_capacity(size_hint.1.unwrap_or(size_hint.0));
        for instr in iter {
            buffer.push(instr);
        }
        buffer
    }
}

impl fmt::Display for InstrBuffer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut indent = String::new();
        for instruction in self.iter() {
            if instruction.opcode() == Opcode::End {
                indent.truncate(indent.len().saturating_sub(2));
            }

            writeln!(f, "{indent}{instruction}")?;

            if matches!(
                instruction.opcode(),
                Opcode::If | Opcode::Block | Opcode::Loop
            ) {
                indent.push_str("  ");
            }
        }

        Ok(())
    }
}
