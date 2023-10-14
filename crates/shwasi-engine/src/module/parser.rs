use std::io::Cursor;

use anyhow::{bail, ensure, Context, Result};
use byteorder::{LittleEndian, ReadBytesExt};
use num_enum::TryFromPrimitive;
use tracing::trace;

use super::leb128;
use crate::{
    is_prefix_byte, BlockType, BrTable, Code, Data, Element, ElementItems, ElementKind, Export,
    ExternalKind, FuncType, Function, Global, GlobalType, Import, ImportKind, InitExpr,
    InstrBuffer, Limit, MemArg, Memory, Module, NumLocals, Opcode, Operands, RefType, TableType,
    ValType,
};

const MAGIC_VALUE: u32 = 0x6d73_6100;
const VERSION: u32 = 1;

#[derive(Debug)]
pub struct Parser<'a> {
    buf: Cursor<&'a [u8]>,
    bufsize: usize,
    module: Module<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(buf: &'a [u8]) -> Self {
        Self {
            buf: Cursor::new(buf),
            bufsize: buf.len(),
            module: Module::default(),
        }
    }

    pub fn read_module(mut self) -> Result<Module<'a>> {
        trace!("began reading module");

        let magic = self.read_u32()?;
        ensure!(magic == MAGIC_VALUE, "bad magic value: {magic}");
        let version = self.read_u32()?;
        ensure!(version == VERSION, "bad version: {version}");

        self.read_sections()
            .with_context(|| format!("error at offset {}", self.bufsize))?;

        trace!("finished reading module");
        Ok(self.module)
    }

    fn read_u32(&mut self) -> Result<u32> {
        self.buf
            .read_u32::<LittleEndian>()
            .context("failed to read u32")
    }

    fn read_u8(&mut self) -> Result<u8> {
        self.buf.read_u8().context("failed to read byte")
    }

    fn read_u32_leb128(&mut self) -> Result<u32> {
        leb128::read_u32_leb128(&mut self.buf).context("failed to read leb128 u32")
    }

    fn read_u64_leb128(&mut self) -> Result<u64> {
        leb128::read_u64_leb128(&mut self.buf).context("failed to read leb128 u32")
    }

    #[allow(dead_code)]
    fn read_s32_leb128(&mut self) -> Result<i32> {
        leb128::read_i32_leb128(&mut self.buf).context("failed to read leb128 s32")
    }

    fn slice(&mut self, n: usize) -> Result<&'a [u8]> {
        let buf = *self.buf.get_ref();
        let offset = self.offset() as usize;
        ensure!(
            offset + n <= self.bufsize,
            "attempted to read too many bytes... max is {}, got {}",
            self.bufsize,
            offset + n
        );
        let slice = &buf[offset..offset + n];
        self.buf.set_position((offset + n) as u64);
        Ok(slice)
    }

    fn read_str(&mut self) -> Result<&'a str> {
        let len = self.read_u32_leb128()?;
        ensure!(
            self.offset() + len as u64 != self.bufsize as u64,
            "string goes past end of buffer"
        );

        let contents = self.slice(len as usize)?;
        let s =
            std::str::from_utf8(contents).context("bytes contained invalid UTF-8 when reading str");
        trace!("finished reading str");
        s
    }

    fn offset(&self) -> u64 {
        self.buf.position()
    }

    fn read_sections(&mut self) -> Result<()> {
        trace!("began reading sections");

        while self.offset() < self.bufsize as u64 {
            let section_code: SectionCode =
                self.read_u8()?.try_into().context("invalid section code")?;
            let size = self.read_u32_leb128()?;
            let end = self.offset() + size as u64;
            ensure!(end <= self.bufsize as u64, "section extends past end");

            match section_code {
                SectionCode::Custom => {
                    trace!("began reading custom section");
                    // skip custom sections, for now
                    self.buf.set_position(end);
                    trace!("finished reading custom section");
                }
                SectionCode::Type => self
                    .read_type_section()
                    .context("error reading type section")?,
                SectionCode::Import => self
                    .read_import_section()
                    .context("error reading import section")?,
                SectionCode::Function => self
                    .read_function_section()
                    .context("error reading function section")?,
                SectionCode::Table => self
                    .read_table_section()
                    .context("error reading table section")?,
                SectionCode::Memory => self
                    .read_memory_section()
                    .context("error reading memory section")?,
                SectionCode::Global => self
                    .read_global_section()
                    .context("error reading global section")?,
                SectionCode::Export => self
                    .read_export_section()
                    .context("error reading export section")?,
                SectionCode::Start => self
                    .read_start_section()
                    .context("error reading start section")?,
                SectionCode::Elem => self
                    .read_elem_section()
                    .context("error reading elem section")?,
                SectionCode::Code => self
                    .read_code_section()
                    .context("error reading code section")?,
                SectionCode::Data => self
                    .read_data_section()
                    .context("error reading data section")?,
                SectionCode::DataCount if size != 0 => {
                    let amount = self.read_u32_leb128()?;
                    self.module.data_count = Some(amount);
                    trace!("got data count section, segments: {amount}");
                }
                SectionCode::DataCount => {}
            }

            ensure!(self.offset() == end, "did not finish reading section");
        }

        trace!("finished reading sections");
        Ok(())
    }

    fn read_type_section(&mut self) -> Result<()> {
        trace!("began reading type section");

        let signatures = self.read_u32_leb128()?;

        trace!("found {signatures} signatures");
        for _ in 0..signatures {
            let form = self.read_u8()?;
            ensure!(
                form == 0x60,
                "unexpected type form. The gc proposal is currently not supported!"
            );

            let params = {
                let num_params = self.read_u32_leb128()?;
                let mut params = Vec::with_capacity(num_params as usize);
                for _ in 0..num_params {
                    let valtype = self.read_type().context("error reading type result")?;
                    params.push(valtype);
                }
                params
            };

            let results = {
                let num_results = self.read_u32_leb128()?;
                let mut results = Vec::with_capacity(num_results as usize);
                for _ in 0..num_results {
                    let valtype = self.read_type().context("error reading type result")?;
                    results.push(valtype);
                }
                results
            };

            trace!("read type {params:?} -> {results:?}");
            self.module.types.push(FuncType(params, results));
        }

        trace!("finished reading type section");
        Ok(())
    }

    fn read_import_section(&mut self) -> Result<()> {
        trace!("began reading import section");

        let num_imports = self.read_u32_leb128()?;
        trace!("found {num_imports} imports");

        for _ in 0..num_imports {
            let module = self.read_str().context("error reading module name")?;
            let field = self.read_str().context("error reading field name")?;

            let external_kind = self
                .read_external_kind()
                .context("error reading import external kind")?;

            let import_kind = match external_kind {
                ExternalKind::Function => {
                    let idx = self.read_u32_leb128()?;
                    ImportKind::Function(idx)
                }
                ExternalKind::Table => ImportKind::Table(
                    self.read_table_type()
                        .context("error reading table type for import")?,
                ),
                ExternalKind::Memory => ImportKind::Memory(
                    self.read_memory_type()
                        .context("error reading table type for import")?,
                ),
                ExternalKind::Global => ImportKind::Global(
                    self.read_global_type()
                        .context("error reading global type for global")?,
                ),
            };

            let import = Import {
                kind: import_kind,
                field,
                module,
            };
            trace!("read import: {import:?}");
            self.module.imports.push(import);
        }

        trace!("finished reading import section");
        Ok(())
    }

    fn read_function_section(&mut self) -> Result<()> {
        trace!("began reading function section");

        let signatures = self.read_u32_leb128()?;
        for _ in 0..signatures {
            let index = self.read_u32_leb128()?;
            let function = Function { index };
            trace!("read function that points to type index: {index}");
            self.module.functions.push(function);
        }

        trace!("finished reading function section");
        Ok(())
    }

    fn read_table_section(&mut self) -> Result<()> {
        trace!("began reading table section");

        let tables = self.read_u32_leb128()?;
        trace!("got {tables} tables");
        for _ in 0..tables {
            let table = self
                .read_table_type()
                .context("error reading table in table section")?;
            self.module.tables.push(table);
        }

        trace!("finished reading table section");
        Ok(())
    }

    fn read_memory_section(&mut self) -> Result<()> {
        trace!("began reading memory section");

        let memories = self.read_u32_leb128()?;
        trace!("got {memories} memories");
        for _ in 0..memories {
            let memory = self
                .read_memory_type()
                .context("error reading memory type in memory section")?;
            self.module.memories.push(memory);
        }

        trace!("finished reading memory section");
        Ok(())
    }

    fn read_global_section(&mut self) -> Result<()> {
        trace!("began reading global section");

        let globals = self.read_u32_leb128()?;
        trace!("got {globals} globals");
        for _ in 0..globals {
            let global_type = self
                .read_global_type()
                .context("error reading global type of global")?;
            let init_expr = self
                .read_init_expr()
                .context("error reading init expr of global")?;

            let global = Global {
                init: init_expr,
                kind: global_type,
            };
            trace!("read global: {global:?}");
            self.module.globals.push(global);
        }

        trace!("finished reading global section");
        Ok(())
    }

    fn read_export_section(&mut self) -> Result<()> {
        trace!("began reading export section");

        let exports = self.read_u32_leb128()?;
        trace!("got {exports} exports");
        for _ in 0..exports {
            let name = self.read_str().context("error reading export name")?;
            let kind = self
                .read_external_kind()
                .context("error reading external kind of export")?;
            let index = self.read_u32_leb128()?;

            let export = Export {
                field: name,
                kind,
                external_idx: index,
            };
            trace!("read export: {export:?}");

            self.module.exports.push(export);
        }

        trace!("finished reading export section");
        Ok(())
    }

    fn read_start_section(&mut self) -> Result<()> {
        trace!("began reading start section");

        let index = self.read_u32_leb128()?;
        self.module.start = Some(index);

        trace!("finished reading start section, got index: {index}");
        Ok(())
    }

    fn read_elem_section(&mut self) -> Result<()> {
        trace!("began reading element section");

        let elems = self.read_u32_leb128()?;
        trace!("found {elems} elements");
        for _ in 0..elems {
            let flags = self.read_u32_leb128()?;
            const PASSIVE: u32 = 0x1;
            const EXPLICIT_IDX: u32 = 0x2;
            const EXPRS: u32 = 0x4;
            ensure!(flags <= 7, "invalid flags: {flags:#b}");

            let kind = if flags & PASSIVE != 0 {
                if flags & EXPLICIT_IDX != 0 {
                    ElementKind::Declarative
                } else {
                    ElementKind::Passive
                }
            } else {
                let table_idx = if flags & EXPLICIT_IDX == 0 {
                    0
                } else {
                    self.read_u32_leb128()?
                };
                let offset_expr = self
                    .read_init_expr()
                    .context("error reading element init expr")?;
                ElementKind::Active {
                    tbl_idx: table_idx,
                    offset: offset_expr,
                }
            };

            let has_exprs = flags & EXPRS != 0;

            let ty = if flags & (PASSIVE | EXPLICIT_IDX) != 0 {
                let ty = self
                    .read_reftype()
                    .context("error reading element val type")?;
                if has_exprs {
                    ty
                } else {
                    ensure!(
                        ty == RefType::Func,
                        "only func refs are allowed in elements"
                    );
                    RefType::Func
                }
            } else {
                RefType::Func
            };

            let num_items = self.read_u32_leb128()?;
            let items = if !has_exprs {
                let mut functions = Vec::with_capacity(num_items as usize);
                for _ in 0..num_items {
                    functions.push(self.read_u32_leb128()?);
                }
                ElementItems::Functions(functions)
            } else {
                let mut elems = Vec::with_capacity(num_items as usize);
                for _ in 0..num_items {
                    elems.push(
                        self.read_init_expr()
                            .context("error reading element init expr")?,
                    );
                }
                ElementItems::Elems(elems)
            };

            let element = Element {
                kind,
                types: ty,
                elems: items,
            };
            self.module.elements.push(element);
        }

        trace!("finished reading element section");
        Ok(())
    }

    fn read_code_section(&mut self) -> Result<()> {
        trace!("began reading the code section");

        let function_bodies = self.read_u32_leb128()?;
        trace!("found {function_bodies} function bodies");
        ensure!(
            function_bodies as usize == self.module.types.len(),
            "should have the same amount of function bodies as types"
        );
        for _ in 0..function_bodies {
            let body_size = self.read_u32_leb128()?;
            let end_offset = self.offset() + body_size as u64;
            let local_decls = self.read_u32_leb128()?;
            let mut locals = Vec::with_capacity(local_decls as usize);
            for _ in 0..local_decls {
                let type_count = self.read_u32_leb128()?;
                let valtype = self.read_type().context("error reading local type")?;
                locals.push(NumLocals {
                    num: type_count,
                    locals_type: valtype,
                });
            }
            let body = self
                .read_instrs(end_offset)
                .context("error reading code section function")?;
            ensure!(self.offset() == end_offset, "did not read all instructions");
            let code = Code { locals, body };
            trace!("got code: {code:?}");
            self.module.codes.push(code);
        }

        trace!("finished reading the code section");
        Ok(())
    }

    fn read_data_section(&mut self) -> Result<()> {
        trace!("began reading data section");

        let data_segments = self.read_u32_leb128()?;
        trace!("got {data_segments} data segments");
        if let Some(count) = self.module.data_count {
            ensure!(
                count == data_segments,
                "data count section does not match amount of data segments"
            );
        }
        for _ in 0..data_segments {
            const PASSIVE: u32 = 0x1;
            const EXPLICIT_IDX: u32 = 0x2;

            let flags = self.read_u32_leb128()?;
            ensure!(
                flags < 3,
                "only passive and explicit index flags are valid in data section"
            );

            let mem_idx = if flags & EXPLICIT_IDX == EXPLICIT_IDX {
                self.read_u32_leb128()?
            } else {
                0
            };
            let init = if flags & PASSIVE == 0 {
                Some(
                    self.read_init_expr()
                        .context("error reading init expr in data segment")?,
                )
            } else {
                None
            };

            let data_size = self.read_u32_leb128()?;
            let data = self
                .slice(data_size as usize)
                .context("error reading data")?;

            let data = Data {
                index: mem_idx,
                offset: init,
                data,
            };
            trace!("got data segment {data:?}");
            self.module.datas.push(data);
        }

        trace!("finished reading data section");
        Ok(())
    }

    fn read_type(&mut self) -> Result<ValType> {
        let valtype =
            ValType::try_from_primitive(self.read_u8()?).context("invalid valtype found")?;
        trace!("read type: {valtype:?}");
        Ok(valtype)
    }

    fn read_reftype(&mut self) -> Result<RefType> {
        let reftype = RefType::try_from_primitive(self.read_u8()?).context("invalid reftype")?;
        trace!("read reftype: {reftype:?}");
        Ok(reftype)
    }

    fn read_table_type(&mut self) -> Result<TableType> {
        let reftype = self.read_reftype().context("error reading table reftype")?;
        let flags = self.read_u8()?;
        let has_max = match flags {
            0x00 => true,
            0x01 => false,
            _ => bail!("invalid table flag, only supports max flag"),
        };

        let initial = self.read_u32_leb128()?;
        let max = if has_max {
            Some(self.read_u32_leb128()?)
        } else {
            None
        };
        let table = TableType {
            elem_type: reftype,
            limit: Limit { initial, max },
        };

        trace!("read table type: {table:?}");
        Ok(table)
    }

    fn read_memory_type(&mut self) -> Result<Memory> {
        let flags = self.read_u8()?;
        let has_max = (flags & 0x01) == 0x01;
        if flags > 1 {
            bail!("non-max memory flags are currently not supported!");
        }

        let initial = self.read_u32_leb128()?;
        let max = if has_max {
            Some(self.read_u32_leb128()?)
        } else {
            None
        };
        let limit = Limit { initial, max };

        trace!("read memory type with limit: {limit:?}");
        Ok(Memory { limit })
    }

    fn read_global_type(&mut self) -> Result<GlobalType> {
        let global_type = self.read_type().context("error type of global")?;
        let mutable = match self.read_u8()? {
            0 => false,
            1 => true,
            n => bail!("global mutability must be either 0 or 1, not {n}"),
        };

        let global = GlobalType {
            content_type: global_type,
            mutable,
        };

        trace!("read global type: {global:?}");
        Ok(global)
    }

    fn read_init_expr(&mut self) -> Result<InitExpr> {
        let init_expr = self
            .read_instrs(self.bufsize as u64)
            .context("error reading init instructions")?;
        ensure!(
            init_expr.len() == 2,
            "init expr can only have two instructions"
        );
        let last = init_expr.get(1).unwrap();
        ensure!(
            init_expr.opcode(last) == Opcode::End,
            "init expr must end with an end instruction"
        );
        let instr = init_expr.get(0).unwrap();
        let expr = match init_expr.opcode(instr) {
            Opcode::I32Const => {
                let n = init_expr.four_byte_operand(instr);
                InitExpr::I32Const(n)
            }
            Opcode::I64Const => {
                let n = init_expr.u64_operand(instr);
                InitExpr::I64Const(n)
            }
            Opcode::F32Const => {
                let n = init_expr.four_byte_operand(instr);
                InitExpr::F32Const(n)
            }
            Opcode::F64Const => {
                let operand = init_expr.u64_operand(instr);
                InitExpr::F64Const(operand)
            }
            Opcode::GlobalGet => {
                let index = init_expr.four_byte_operand(instr);
                InitExpr::ConstGlobalGet(index)
            }
            _ => bail!("init expr instruction is not const-valid"),
        };
        Ok(expr)
    }

    fn read_external_kind(&mut self) -> Result<ExternalKind> {
        let external_kind =
            ExternalKind::try_from_primitive(self.read_u8()?).context("invalid external kind")?;
        trace!("read external kind: {external_kind:?}");
        Ok(external_kind)
    }

    fn read_opcode(&mut self) -> Result<Opcode> {
        let byte = self.read_u8()?;
        if is_prefix_byte(byte) {
            let sub_value = self.read_u32_leb128()?;
            return Opcode::try_from_bytes(byte, sub_value).context("unknown opcode");
        }

        Opcode::try_from_byte(byte).context("unkown opcode")
    }

    fn read_memarg(&mut self) -> Result<MemArg> {
        let align = self.read_u32_leb128()?;
        let offset = self.read_u32_leb128()?;

        Ok(MemArg { offset, align })
    }

    fn read_blocktype(&mut self) -> Result<BlockType> {
        let byte = self.read_u8()?;

        if byte == 0x40 {
            return Ok(BlockType::Empty);
        }
        if let Ok(valtype) = ValType::try_from_primitive(byte) {
            return Ok(BlockType::Type(valtype));
        }

        let idx = self.read_s32_leb128()?;
        Ok(BlockType::FuncType(
            idx.try_into().context("invalid function index")?,
        ))
    }

    fn read_br_table(&mut self) -> Result<BrTable> {
        let num_targets = self.read_u32_leb128()?;
        let mut targets = Vec::with_capacity(num_targets as usize);

        for _ in 0..num_targets {
            let depth = self.read_u32_leb128()?;
            targets.push(depth);
        }

        let default = self.read_u32_leb128()?;
        Ok(BrTable {
            depths: targets,
            default_depth: default,
        })
    }

    fn read_instrs(&mut self, end_offset: u64) -> Result<InstrBuffer> {
        trace!("began reading instrs");
        let mut buffer = InstrBuffer::new();
        let mut stack = vec![];

        while self.offset() < end_offset {
            let opcode = self
                .read_opcode()
                .context("error reading opcode in instructions")?;

            let operands = match opcode {
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
                | Opcode::I64Store32 => Operands::MemArg(self.read_memarg()?),

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
                | Opcode::TableFill => Operands::FourBytes(self.read_u32_leb128()?),

                Opcode::I64Const | Opcode::F64Const => Operands::U64(self.read_u64_leb128()?),

                Opcode::CallIndirect | Opcode::TableCopy | Opcode::TableInit => {
                    let b1 = self.read_u32_leb128()?;
                    let b2 = self.read_u32_leb128()?;
                    Operands::EightBytes(b1, b2)
                }

                Opcode::RefNull => {
                    let reftype = self.read_reftype()?;
                    Operands::RefType(reftype)
                }

                Opcode::Block | Opcode::Loop | Opcode::If => {
                    stack.push(opcode);
                    let block_type = self.read_blocktype()?;
                    Operands::BlockType(block_type)
                }

                Opcode::BrTable => {
                    let br_table = self.read_br_table()?;
                    Operands::BrTable(br_table)
                }

                Opcode::End => {
                    if stack.is_empty() {
                        trace!(
                            "returned early with end instruction, got buffer of size {}",
                            buffer.len()
                        );
                        buffer.add_instr(Opcode::End, Operands::Empty);
                        return Ok(buffer);
                    }
                    stack.pop();
                    Operands::Empty
                }

                Opcode::Else => {
                    ensure!(stack.last().is_some_and(|opcode| *opcode == Opcode::If));
                    Operands::Empty
                }

                Opcode::Unreachable
                | Opcode::Nop
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
                | Opcode::MemoryFill => Operands::Empty,
            };

            buffer.add_instr(opcode, operands);
        }

        trace!("finished reading instrs, got {} instructions", buffer.len());
        Ok(buffer)
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, PartialOrd, Eq, Ord, TryFromPrimitive)]
#[repr(u8)]
enum SectionCode {
    Custom = 0,
    Type = 1,
    Import = 2,
    Function = 3,
    Table = 4,
    Memory = 5,
    Global = 6,
    Export = 7,
    Start = 8,
    Elem = 9,
    Code = 10,
    Data = 11,
    DataCount = 12,
}
