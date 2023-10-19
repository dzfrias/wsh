use std::{fmt, io::Cursor, str};

use anyhow::{bail, ensure, Context, Result};
use bitflags::bitflags;
use byteorder::{LittleEndian, ReadBytesExt};
use num_enum::TryFromPrimitive;
use tracing::trace;

use crate::{
    is_prefix_byte, BlockType, BrTable, Code, Data, Element, ElementItems, ElementKind, Export,
    ExternalKind, FuncType, Function, Global, GlobalType, Import, ImportKind, InitExpr,
    InstrBuffer, Instruction, Limit, MemArg, Memory, Module, NumLocals, Opcode, RefType, TableType,
    ValType, F32, F64,
};

const MAGIC_VALUE: u32 = 0x6d73_6100;
const VERSION: u32 = 1;

#[derive(Debug)]
pub struct Parser<'a> {
    // A cursor into the input buffer
    buf: Cursor<&'a [u8]>,
    bufsize: usize,
    // The output module (this is mutated as we parse)
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
        ensure!(
            self.offset() == self.bufsize as u64,
            "did not finish reading module, still have {} bytes to read",
            self.bufsize as u64 - self.offset()
        );

        trace!("finished reading module");
        Ok(self.module)
    }

    fn read_u32(&mut self) -> Result<u32> {
        self.buf
            .read_u32::<LittleEndian>()
            .context("failed to read u32")
    }

    fn read_u64(&mut self) -> Result<u64> {
        self.buf
            .read_u64::<LittleEndian>()
            .context("failed to read u64")
    }

    fn read_u8(&mut self) -> Result<u8> {
        self.buf.read_u8().context("failed to read byte")
    }

    fn read_u32_leb128(&mut self) -> Result<u32> {
        wasm_leb128::read_u32_leb128(&mut self.buf).context("failed to read leb128 u32")
    }

    fn read_s32_leb128(&mut self) -> Result<i32> {
        wasm_leb128::read_s32_leb128(&mut self.buf).context("failed to read leb128 u32")
    }

    fn read_s64_leb128(&mut self) -> Result<i64> {
        wasm_leb128::read_s64_leb128(&mut self.buf).context("failed to read leb128 u32")
    }

    // Advance a parser n bytes, returning the slice that was advanced over
    fn slice(&mut self, n: usize) -> Result<&'a [u8]> {
        // Get a reference to the underlying buffer
        let buf = *self.buf.get_ref();
        let offset = self.offset() as usize;
        ensure!(
            offset + n <= self.bufsize,
            "attempted to read too many bytes... max is {}, got {}",
            self.bufsize,
            offset + n
        );
        // Slice to get data with a lifetime of 'a
        let slice = &buf[offset..offset + n];
        // Advance the buffer by our slice
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
            str::from_utf8(contents).context("bytes contained invalid UTF-8 when reading str")?;
        trace!("finished reading str, got {s}");
        Ok(s)
    }

    fn offset(&self) -> u64 {
        self.buf.position()
    }

    fn read_sections(&mut self) -> Result<()> {
        trace!("began reading sections");

        // The last section that was read. Used to ensure sections are in order and that duplicate
        // sections are not present.
        let mut last_section = SectionCode::Custom;
        while self.offset() < self.bufsize as u64 {
            let section_code: SectionCode =
                self.read_u8()?.try_into().context("invalid section code")?;
            let size = self.read_u32_leb128()?;
            let end = self.offset() + size as u64;
            ensure!(end <= self.bufsize as u64, "section extends past end");

            ensure!(
                last_section != section_code,
                "duplicate section: {section_code}"
            );
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

            if section_code != SectionCode::Custom {
                if section_code < last_section {
                    bail!("the {section_code} section is out of order");
                }

                last_section = section_code;
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
            // Functions are the only type form accepted as of right now
            let form = self.read_u8()?;
            ensure!(
                form == 0x60,
                "unexpected type form. The gc proposal is currently not supported!"
            );

            let params = {
                let num_params = self.read_u32_leb128()?;
                let mut params = vec![];
                for _ in 0..num_params {
                    let valtype = self.read_type().context("error reading type result")?;
                    params.push(valtype);
                }
                params
            };

            let results = {
                let num_results = self.read_u32_leb128()?;
                let mut results = vec![];
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
                    // Index of the type of the imported function
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
            // Type index
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
            // Index of the exported item -- corresponds to the external kind
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

        // Function index
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
            let flags_raw = self.read_u32_leb128()?;
            let flags = SegmentFlags::from_bits(flags_raw)
                .context("error reading element segment flags")?;

            let kind = if flags.intersects(SegmentFlags::PASSIVE) {
                // The kind isn't passive, so it is either declarative or active. If it has an
                // explicit index, it is declarative.
                if flags.intersects(SegmentFlags::EXPLICIT_IDX) {
                    ElementKind::Declarative
                } else {
                    ElementKind::Passive
                }
            } else {
                // The table that will be initialized
                let table_idx = if !flags.intersects(SegmentFlags::EXPLICIT_IDX) {
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

            let has_exprs = flags.intersects(SegmentFlags::EXPRS);

            let ty = if flags.intersects(SegmentFlags::PASSIVE | SegmentFlags::EXPLICIT_IDX) {
                if has_exprs {
                    self.read_reftype()
                        .context("error reading element val type")?
                } else {
                    // When there are no exprs, the type is implicit. This could even be a check
                    // for the 0x00 byte, but it can also be treated as an external kind.
                    let ty = self
                        .read_external_kind()
                        .context("error reading element external kind")?;
                    ensure!(
                        ty == ExternalKind::Function,
                        "only func refs are allowed in elements"
                    );
                    RefType::Func
                }
            } else {
                // No explicit reftype
                RefType::Func
            };

            let num_items = self.read_u32_leb128()?;
            let items = if !has_exprs {
                let mut functions = vec![];
                for _ in 0..num_items {
                    functions.push(self.read_u32_leb128()?);
                }
                ElementItems::Functions(functions)
            } else {
                let mut elems = vec![];
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
            function_bodies as usize == self.module.functions.len(),
            "should have the same amount of function bodies as types"
        );
        for _ in 0..function_bodies {
            let body_size = self.read_u32_leb128()?;
            // By the end of this function, we should hit this offset exactly.
            let end_offset = self.offset() + body_size as u64;
            // Number of local declarations
            let local_decls = self.read_u32_leb128()?;
            let mut locals = vec![];
            for _ in 0..local_decls {
                // Number of locals of this type
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
            let flags_raw = self.read_u32_leb128()?;
            ensure!(
                flags_raw < 3,
                "only passive and explicit index flags are valid in data section"
            );
            let flags =
                SegmentFlags::from_bits(flags_raw).context("error reading data segment flags")?;

            let mem_idx = if flags.intersects(SegmentFlags::EXPLICIT_IDX) {
                self.read_u32_leb128()?
            } else {
                0
            };
            let init = if !flags.intersects(SegmentFlags::PASSIVE) {
                Some(
                    self.read_init_expr()
                        .context("error reading init expr in data segment")?,
                )
            } else {
                None
            };
            ensure!(
                mem_idx == 0,
                "memory index should be 0, multi-memories is not supported"
            );

            let data_size = self.read_u32_leb128()?;
            // Get the raw data following the data initialization
            let data = self
                .slice(data_size as usize)
                .context("error reading data")?;

            let data = Data { offset: init, data };
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
        // Flags can only contain the max flag, which denotes if the limit has a maximum.
        let flags = self.read_u8()?;
        let has_max = match flags {
            0x01 => true,
            0x00 => false,
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
        // We just pass bufsize as the end offset of the init expr, since we aren't always given
        // the size. However, `read_instrs` will stop reading when it reaches and unmatched `end`
        // instruction.
        let init_expr = self
            .read_instrs(self.bufsize as u64)
            .context("error reading init instructions")?;
        ensure!(
            init_expr.len() >= 2,
            "init expr can must have 2+ instructions"
        );
        // This is just a sanity check to make sure we didn't read until the end of the buffer (the
        // only other case when `read_instrs` would stop).
        let last = init_expr.last().unwrap();
        ensure!(
            init_expr.opcode(last) == Opcode::End,
            "init expr must end with an end instruction"
        );
        let instr = init_expr.first().unwrap();
        let expr = match init_expr.instruction(instr) {
            Instruction::I32Const(val) => InitExpr::I32Const(val),
            Instruction::I64Const(val) => InitExpr::I64Const(val),
            Instruction::F32Const(val) => InitExpr::F32Const(val),
            Instruction::F64Const(val) => InitExpr::F64Const(val),
            Instruction::GlobalGet { idx } => InitExpr::ConstGlobalGet(idx),
            instr => bail!("init expr instruction `{instr}` is not const-valid"),
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
        // Some opcodes have a prefix byte
        if is_prefix_byte(byte) {
            let sub_value = self.read_u32_leb128()?;
            return Opcode::try_from_bytes(byte, sub_value)
                .with_context(|| format!("unknown opcode, {byte:#x}"));
        }

        Opcode::try_from_byte(byte).with_context(|| format!("unknown opcode, {byte:#x}"))
    }

    fn read_memarg(&mut self) -> Result<MemArg> {
        let align = self.read_u32_leb128()?;
        let offset = self.read_u32_leb128()?;

        Ok(MemArg { offset, align })
    }

    fn read_blocktype(&mut self) -> Result<BlockType> {
        let ty = self.read_s32_leb128()?;
        if ty == -0x40 {
            return Ok(BlockType::Empty);
        }
        if let Ok(valtype) = ValType::try_from_primitive((ty & 0xff) as u8) {
            return Ok(BlockType::Type(valtype));
        }

        // SAFETY: This value is just an i32, so it is safe to re-interpret it as a u32
        Ok(BlockType::FuncType(unsafe { std::mem::transmute(ty) }))
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
                    Instruction::DataDrop { data_idx }
                }
                Opcode::ElemDrop => {
                    let elem_idx = self.read_u32_leb128()?;
                    Instruction::ElemDrop { elem_idx }
                }
                Opcode::I32Const => {
                    let val = self.read_s32_leb128()?;
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
                Opcode::I64Const => Instruction::I64Const(self.read_s64_leb128()?),

                Opcode::TableCopy => {
                    let b1 = self.read_u32_leb128()?;
                    let b2 = self.read_u32_leb128()?;
                    Instruction::TableCopy {
                        src_table: b1,
                        dst_table: b2,
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
                    stack.push(opcode);
                    let block_type = self.read_blocktype()?;
                    Instruction::If(block_type)
                }
                Opcode::Block => {
                    stack.push(opcode);
                    let block_type = self.read_blocktype()?;
                    Instruction::Block(block_type)
                }
                Opcode::Loop => {
                    stack.push(opcode);
                    let block_type = self.read_blocktype()?;
                    Instruction::Loop(block_type)
                }
                Opcode::BrTable => {
                    let br_table = self.read_br_table()?;
                    Instruction::BrTable(br_table)
                }

                Opcode::End => {
                    if stack.is_empty() {
                        trace!(
                            "returned early with end instruction, got buffer of size {}",
                            buffer.len()
                        );
                        buffer.add_instr(Instruction::End);
                        return Ok(buffer);
                    }
                    stack.pop();
                    Instruction::End
                }

                Opcode::Else => {
                    ensure!(stack.last().is_some_and(|opcode| *opcode == Opcode::If));
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
            };

            trace!("read instruction: `{instr}`");
            buffer.add_instr(instr);
        }

        trace!("finished reading instrs, got {} instructions", buffer.len());
        Ok(buffer)
    }
}

/// A WebAssembly section. The u8 interpretation indicates its section number, but it `Ord` is
/// based on section ordering.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, TryFromPrimitive)]
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

impl fmt::Display for SectionCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let to_write = match self {
            SectionCode::Custom => "custom",
            SectionCode::Type => "type",
            SectionCode::Import => "import",
            SectionCode::Function => "function",
            SectionCode::Table => "table",
            SectionCode::Memory => "memory",
            SectionCode::Global => "global",
            SectionCode::Export => "export",
            SectionCode::Start => "start",
            SectionCode::Elem => "element",
            SectionCode::Code => "code",
            SectionCode::Data => "data",
            SectionCode::DataCount => "data count",
        };
        f.write_str(to_write)
    }
}

impl PartialOrd for SectionCode {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.section_ord().cmp(&other.section_ord()))
    }
}

impl Ord for SectionCode {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl SectionCode {
    /// A number corresponding to the correct ordering of sections, which doesn't match up with the
    /// u8 representation.
    fn section_ord(&self) -> u8 {
        match self {
            SectionCode::Custom => 0,
            SectionCode::Type => 1,
            SectionCode::Import => 2,
            SectionCode::Function => 3,
            SectionCode::Table => 4,
            SectionCode::Memory => 5,
            SectionCode::Global => 6,
            SectionCode::Export => 7,
            SectionCode::Start => 8,
            SectionCode::Elem => 9,
            SectionCode::DataCount => 10,
            SectionCode::Code => 11,
            SectionCode::Data => 12,
        }
    }
}

bitflags! {
    #[derive(Debug)]
    struct SegmentFlags: u32 {
        const PASSIVE = 0x01;
        const EXPLICIT_IDX = 0x02;
        const EXPRS = 0x04;
    }
}
