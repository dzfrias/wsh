use std::io::Cursor;

use anyhow::{ensure, Context, Result};
use byteorder::{LittleEndian, ReadBytesExt};
use num_enum::TryFromPrimitive;
use tracing::{debug, instrument, trace};

use crate::{
    is_prefix_byte, Block, BlockType, BrTable, InstrBuffer, Instruction, MemArg, Module, Opcode,
    RefType, ValType, F32, F64,
};

#[derive(Debug)]
pub struct InstrReader<'a> {
    module: &'a Module<'a>,
    buf: Cursor<&'a [u8]>,
}

impl<'a> InstrReader<'a> {
    pub fn new(buffer: &'a [u8], module: &'a Module<'a>) -> Self {
        Self {
            module,
            buf: Cursor::new(buffer),
        }
    }

    #[inline]
    fn read_u32(&mut self) -> Result<u32> {
        self.buf
            .read_u32::<LittleEndian>()
            .context("failed to read u32")
    }

    #[inline]
    fn read_u64(&mut self) -> Result<u64> {
        self.buf
            .read_u64::<LittleEndian>()
            .context("failed to read u64")
    }

    #[inline]
    fn read_u8(&mut self) -> Result<u8> {
        self.buf.read_u8().context("failed to read byte")
    }

    #[inline]
    pub fn offset(&self) -> u64 {
        self.buf.position()
    }

    #[inline]
    fn read_u32_leb128(&mut self) -> Result<u32> {
        // No context is attached to this one... although this provides a bit worse error handling
        // just this speeds up the program by 7% when parsing spidermonkey...
        Ok(wasm_leb128::read_u32_leb128(&mut self.buf)?)
    }

    #[inline]
    fn read_s32_leb128(&mut self) -> Result<i32> {
        wasm_leb128::read_s32_leb128(&mut self.buf).context("failed to read leb128 s32")
    }

    #[inline]
    fn read_s33_leb128(&mut self) -> Result<i64> {
        wasm_leb128::read_s33_leb128(&mut self.buf).context("failed to read leb128 s32")
    }

    #[inline]
    fn read_s64_leb128(&mut self) -> Result<i64> {
        wasm_leb128::read_s64_leb128(&mut self.buf).context("failed to read leb128 s64")
    }

    #[inline]
    fn read_opcode(&mut self) -> Result<Opcode> {
        let byte = self.read_u8()?;
        // Some opcodes have a prefix byte
        if is_prefix_byte(byte) {
            let sub_value = self.read_u32_leb128()?;
            return Opcode::try_from_bytes(byte, sub_value)
                .with_context(|| format!("unknown opcode, {byte:#x}"));
        }

        // .with_context() is a shortcut to this, but it ended up being around 5% slower when
        // parsing spidermonkey.
        Opcode::try_from_byte(byte).ok_or_else(|| anyhow::anyhow!("unknown opcode, {byte:#x}"))
    }

    #[inline]
    fn read_memarg(&mut self) -> Result<MemArg> {
        let align = self.read_u32_leb128()?;
        let offset = self.read_u32_leb128()?;

        Ok(MemArg { offset, align })
    }

    fn read_blocktype(&mut self) -> Result<BlockType> {
        let ty = self.read_u8()?;
        if ty == 0x40 {
            return Ok(BlockType::Empty);
        }
        if let Ok(valtype) = ValType::try_from_primitive(ty) {
            return Ok(BlockType::Type(valtype));
        }

        self.buf.set_position(self.offset() - 1);
        let func_ty = self.read_s33_leb128()?;

        Ok(BlockType::FuncType(
            u32::try_from(func_ty).context("invalid function index")?,
        ))
    }

    fn read_br_table(&mut self) -> Result<BrTable> {
        let num_targets = self.read_u32_leb128()?;
        let mut targets = vec![];

        for _ in 0..num_targets {
            let depth = self.read_u32_leb128()?;
            targets.push(depth);
        }

        let default = self.read_u32_leb128()?;
        Ok(BrTable {
            depths: targets.into(),
            default_depth: default,
        })
    }

    fn read_type(&mut self) -> Result<ValType> {
        let valtype =
            ValType::try_from_primitive(self.read_u8()?).context("invalid valtype found")?;
        debug!("read type: {valtype:?}");
        Ok(valtype)
    }

    fn read_reftype(&mut self) -> Result<RefType> {
        let reftype = RefType::try_from_primitive(self.read_u8()?).context("invalid reftype")?;
        debug!("read reftype: {reftype:?}");
        Ok(reftype)
    }

    pub fn has_leftover(&self) -> bool {
        self.offset() < self.buf.get_ref().len() as u64
    }

    #[instrument(skip(self), level = "debug")]
    pub fn read_instrs(&mut self, known_end: bool) -> Result<InstrBuffer> {
        let mut buffer = if known_end {
            // This uses a bit of domain knowledge, but on average, if there are n bytes to read, then
            // there is around n / 2 instructions. This results in around a substantial speed increase
            // when parsing spidermonkey.
            InstrBuffer::with_capacity(
                ((self.buf.get_ref().len() as u64 - self.offset()) / 2) as usize,
            )
        } else {
            // If there is no known end, then the assumption above will get us in quite a bit of
            // memory trouble... so just make an empty (default) buffer.
            InstrBuffer::new()
        };
        let mut stack = vec![];

        while self.offset() < self.buf.get_ref().len() as u64 {
            let opcode = self
                .read_opcode()
                .context("error reading opcode in instructions")?;

            let instr = match opcode {
                Opcode::I32Load => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::I32Load(mem_arg)
                }
                Opcode::I64Load => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::I64Load(mem_arg)
                }
                Opcode::F32Load => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::F32Load(mem_arg)
                }
                Opcode::F64Load => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::F64Load(mem_arg)
                }
                Opcode::I32Load8S => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::I32Load8S(mem_arg)
                }
                Opcode::I32Load8U => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::I32Load8U(mem_arg)
                }
                Opcode::I32Load16S => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::I32Load16S(mem_arg)
                }
                Opcode::I32Load16U => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::I32Load16U(mem_arg)
                }
                Opcode::I64Load8S => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::I64Load8S(mem_arg)
                }
                Opcode::I64Load8U => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::I64Load8U(mem_arg)
                }
                Opcode::I64Load16S => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::I64Load16S(mem_arg)
                }
                Opcode::I64Load16U => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::I64Load16U(mem_arg)
                }
                Opcode::I64Load32S => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::I64Load32S(mem_arg)
                }
                Opcode::I64Load32U => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::I64Load32U(mem_arg)
                }
                Opcode::I32Store => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::I32Store(mem_arg)
                }
                Opcode::I64Store => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::I64Store(mem_arg)
                }
                Opcode::F32Store => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::F32Store(mem_arg)
                }
                Opcode::F64Store => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::F64Store(mem_arg)
                }
                Opcode::I32Store8 => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::I32Store8(mem_arg)
                }
                Opcode::I32Store16 => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::I32Store16(mem_arg)
                }
                Opcode::I64Store8 => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::I64Store8(mem_arg)
                }
                Opcode::I64Store16 => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::I64Store16(mem_arg)
                }
                Opcode::I64Store32 => {
                    let mem_arg = self.read_memarg()?;
                    Instruction::I64Store32(mem_arg)
                }

                Opcode::Br => {
                    let depth = self.read_u32_leb128()?;
                    Instruction::Br { depth }
                }
                Opcode::BrIf => {
                    let depth = self.read_u32_leb128()?;
                    Instruction::BrIf { depth }
                }
                Opcode::Call => {
                    let func_idx = self.read_u32_leb128()?;
                    Instruction::Call { func_idx }
                }
                Opcode::LocalGet => {
                    let idx = self.read_u32_leb128()?;
                    Instruction::LocalGet { idx }
                }
                Opcode::LocalSet => {
                    let idx = self.read_u32_leb128()?;
                    Instruction::LocalSet { idx }
                }
                Opcode::GlobalGet => {
                    let idx = self.read_u32_leb128()?;
                    Instruction::GlobalGet { idx }
                }
                Opcode::GlobalSet => {
                    let idx = self.read_u32_leb128()?;
                    Instruction::GlobalSet { idx }
                }
                Opcode::LocalTee => {
                    let idx = self.read_u32_leb128()?;
                    Instruction::LocalTee { idx }
                }

                Opcode::DataDrop => {
                    let data_idx = self.read_u32_leb128()?;
                    ensure!(
                        self.module.data_count.is_some(),
                        "`data.drop` requires data count section"
                    );
                    Instruction::DataDrop { data_idx }
                }
                Opcode::ElemDrop => {
                    let elem_idx = self.read_u32_leb128()?;
                    Instruction::ElemDrop { elem_idx }
                }
                Opcode::I32Const => {
                    let val = self.read_s32_leb128()? as u32;
                    Instruction::I32Const(val)
                }
                Opcode::F32Const => {
                    let val = self.read_u32()?;
                    Instruction::F32Const(F32::new(val))
                }
                Opcode::MemoryInit => {
                    let data_idx = self.read_u32_leb128()?;
                    let placeholder = self.read_u8()?;
                    ensure!(placeholder == 0x00, "placeholder byte must be 0x00");
                    ensure!(
                        self.module.data_count.is_some(),
                        "`memory.init` requires data count section"
                    );
                    Instruction::MemoryInit { data_idx }
                }
                Opcode::RefFunc => {
                    let func_idx = self.read_u32_leb128()?;
                    Instruction::RefFunc { func_idx }
                }
                Opcode::TableGet => {
                    let table = self.read_u32_leb128()?;
                    Instruction::TableGet { table }
                }
                Opcode::TableSet => {
                    let table = self.read_u32_leb128()?;
                    Instruction::TableSet { table }
                }
                Opcode::TableGrow => {
                    let table = self.read_u32_leb128()?;
                    Instruction::TableGrow { table }
                }
                Opcode::TableSize => {
                    let table = self.read_u32_leb128()?;
                    Instruction::TableSize { table }
                }
                Opcode::TableFill => {
                    let table = self.read_u32_leb128()?;
                    Instruction::TableFill { table }
                }

                Opcode::F64Const => Instruction::F64Const(F64::new(self.read_u64()?)),
                Opcode::I64Const => Instruction::I64Const(self.read_s64_leb128()? as u64),

                Opcode::TableCopy => {
                    let b1 = self.read_u32_leb128()?;
                    let b2 = self.read_u32_leb128()?;
                    Instruction::TableCopy {
                        src_table: b2,
                        dst_table: b1,
                    }
                }
                Opcode::TableInit => {
                    let b1 = self.read_u32_leb128()?;
                    let b2 = self.read_u32_leb128()?;
                    Instruction::TableInit {
                        elem_idx: b1,
                        table_idx: b2,
                    }
                }
                Opcode::CallIndirect => {
                    let type_idx = self.read_u32_leb128()?;
                    let table_idx = self.read_u32_leb128()?;
                    Instruction::CallIndirect {
                        type_idx,
                        table_idx,
                    }
                }
                Opcode::RefNull => {
                    let reftype = self.read_reftype()?;
                    Instruction::RefNull { ty: reftype }
                }
                Opcode::If => {
                    stack.push((opcode, buffer.len()));
                    let block_type = self.read_blocktype()?;
                    Instruction::If {
                        block: Block {
                            ty: block_type,
                            end: 0,
                        },
                        else_: None,
                    }
                }
                Opcode::Block => {
                    stack.push((opcode, buffer.len()));
                    let block_type = self.read_blocktype()?;

                    Instruction::Block(Block {
                        ty: block_type,
                        end: 0,
                    })
                }
                Opcode::Loop => {
                    stack.push((opcode, buffer.len()));
                    let block_type = self.read_blocktype()?;
                    Instruction::Loop(Block {
                        ty: block_type,
                        // The end of the loop is the beginning of the loop. `br 0`, for example,
                        // is equivalent to a continue instruction for a loop.
                        end: buffer.len(),
                    })
                }
                Opcode::BrTable => {
                    let br_table = self.read_br_table()?;
                    Instruction::BrTable(br_table)
                }

                Opcode::End => {
                    match stack.pop() {
                        Some((opcode, idx)) if opcode != Opcode::Loop => {
                            // Because the parser is a single pass, we don't know the end of the
                            // block until we hit the end instruction. So, we need to patch it in
                            // the instruction buffer.
                            buffer.patch_end(idx, buffer.len());
                        }
                        None => {
                            trace!(
                                "returned early with end instruction, got buffer of size {}",
                                buffer.len()
                            );
                            buffer.shrink();
                            buffer.push(Instruction::End);
                            return Ok(buffer);
                        }
                        _ => {}
                    }
                    Instruction::End
                }

                Opcode::Else => {
                    ensure!(stack
                        .last()
                        .is_some_and(|(opcode, _)| *opcode == Opcode::If));
                    buffer.patch_else(stack.last().unwrap().1, buffer.len());
                    Instruction::Else
                }

                Opcode::MemorySize => {
                    let placeholder = self.read_u8()?;
                    ensure!(placeholder == 0x00, "placeholder byte must be 0x00");
                    Instruction::MemorySize
                }
                Opcode::MemoryGrow => {
                    let placeholder = self.read_u8()?;
                    ensure!(placeholder == 0x00, "placeholder byte must be 0x00");
                    Instruction::MemoryGrow
                }
                Opcode::MemoryFill => {
                    let placeholder = self.read_u8()?;
                    ensure!(placeholder == 0x00, "placeholder byte must be 0x00");
                    Instruction::MemoryFill
                }
                Opcode::MemoryCopy => {
                    let placeholder1 = self.read_u8()?;
                    let placeholder2 = self.read_u8()?;
                    ensure!(
                        placeholder1 == 0x00 && placeholder2 == 0x00,
                        "placeholder bytes must be 0x00"
                    );
                    Instruction::MemoryCopy
                }

                Opcode::Unreachable => Instruction::Unreachable,
                Opcode::Nop => Instruction::Nop,
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
                Opcode::RefIsNull => Instruction::RefIsNull,
                Opcode::I32TruncSatF32S => Instruction::I32TruncSatF32S,
                Opcode::I32TruncSatF32U => Instruction::I32TruncSatF32U,
                Opcode::I32TruncSatF64S => Instruction::I32TruncSatF64S,
                Opcode::I32TruncSatF64U => Instruction::I32TruncSatF64U,
                Opcode::I64TruncSatF32S => Instruction::I64TruncSatF32S,
                Opcode::I64TruncSatF32U => Instruction::I64TruncSatF32U,
                Opcode::I64TruncSatF64S => Instruction::I64TruncSatF64S,
                Opcode::I64TruncSatF64U => Instruction::I64TruncSatF64U,
                Opcode::I32Extend8S => Instruction::I32Extend8S,
                Opcode::I32Extend16S => Instruction::I32Extend16S,
                Opcode::I64Extend8S => Instruction::I64Extend8S,
                Opcode::I64Extend16S => Instruction::I64Extend16S,
                Opcode::I64Extend32S => Instruction::I64Extend32S,
                Opcode::SelectT => {
                    let num_types = self.read_u32_leb128()?;
                    ensure!(num_types == 1, "select only supports one type");
                    let ty = self.read_type()?;
                    Instruction::SelectT(ty)
                }
            };

            trace!("read instruction: `{instr}`");
            buffer.push(instr);
        }

        buffer.shrink();
        debug!("got {} instructions", buffer.len());
        Ok(buffer)
    }
}
