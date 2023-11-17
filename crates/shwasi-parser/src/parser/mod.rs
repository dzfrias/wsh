#![allow(non_upper_case_globals)]

mod instr_reader;

use std::{fmt, io::Cursor, str};

use anyhow::{bail, ensure, Context, Result};
use bitflags::bitflags;
use byteorder::{LittleEndian, ReadBytesExt};
use num_enum::TryFromPrimitive;
use rayon::prelude::*;
use tracing::{debug, info, instrument, trace};

use crate::{
    parser::instr_reader::InstrReader, Code, Data, Element, ElementKind, Export, ExternalKind,
    FuncType, Function, Global, GlobalType, Import, ImportKind, InitExpr, Instruction, Limit,
    MemoryType, Module, NumLocals, Opcode, RefType, TableType, ValType,
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

    #[instrument(skip(self), level = "debug")]
    pub fn read_module(mut self) -> Result<Module<'a>> {
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
        ensure!(
            self.module.functions.len() == self.module.codes.len(),
            "function signatures do not match up with code section"
        );
        info!("finished reading module");

        Ok(self.module)
    }

    #[inline]
    fn read_u32(&mut self) -> Result<u32> {
        self.buf
            .read_u32::<LittleEndian>()
            .context("failed to read u32")
    }

    #[inline]
    fn read_u8(&mut self) -> Result<u8> {
        self.buf.read_u8().context("failed to read byte")
    }

    #[inline]
    fn read_u32_leb128(&mut self) -> Result<u32> {
        // No context is attached to this one... although this provides a bit worse error handling
        // just this speeds up the program by 7% when parsing spidermonkey...
        Ok(wasm_leb128::read_u32_leb128(&mut self.buf)?)
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
        debug!("finished reading str, got {s}");
        Ok(s)
    }

    #[inline]
    fn offset(&self) -> u64 {
        self.buf.position()
    }

    #[instrument(skip(self), level = "debug")]
    fn read_sections(&mut self) -> Result<()> {
        // The last section that was read. Used to ensure sections are in order and that duplicate
        // sections are not present.
        let mut last_section = SectionCode::Custom;
        while self.offset() < self.bufsize as u64 {
            let section_code: SectionCode =
                self.read_u8()?.try_into().context("invalid section code")?;
            debug!("found section: {section_code}");
            let size = self.read_u32_leb128()?;
            let end = self.offset() + size as u64;
            ensure!(end <= self.bufsize as u64, "section extends past end");

            if section_code != SectionCode::Custom {
                ensure!(
                    last_section != section_code,
                    "duplicate section: {section_code}"
                );
            }
            match section_code {
                SectionCode::Custom => {
                    let name = self
                        .read_str()
                        .context("error reading custom section name")?;
                    debug!("got custom section name: {name}");
                    ensure!(
                        self.offset() <= end,
                        "custom section extends past section end"
                    );
                    // Skip custom sections, for now
                    self.buf.set_position(end);
                    debug!("skipped custom section");
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
                    debug!("got data count section, segments: {amount}");
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

        Ok(())
    }

    #[instrument(skip(self), level = "debug")]
    fn read_type_section(&mut self) -> Result<()> {
        let signatures = self.read_u32_leb128()?;
        debug!("found {signatures} signatures");
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

            debug!("read type {params:?} -> {results:?}");
            self.module.types.push(FuncType(params, results));
        }

        Ok(())
    }

    #[instrument(skip(self), level = "debug")]
    fn read_import_section(&mut self) -> Result<()> {
        let num_imports = self.read_u32_leb128()?;
        debug!("found {num_imports} imports");

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
            debug!("read import: {import:?}");
            self.module.imports.push(import);
        }

        Ok(())
    }

    #[instrument(skip(self), level = "debug")]
    fn read_function_section(&mut self) -> Result<()> {
        let signatures = self.read_u32_leb128()?;
        debug!("found {signatures} signatures");
        for _ in 0..signatures {
            // Type index
            let index = self.read_u32_leb128()?;
            let function = Function { index };
            debug!("read function that points to type index: {index}");
            self.module.functions.push(function);
        }

        Ok(())
    }

    #[instrument(skip(self), level = "debug")]
    fn read_table_section(&mut self) -> Result<()> {
        let tables = self.read_u32_leb128()?;
        debug!("got {tables} tables");
        for _ in 0..tables {
            let table = self
                .read_table_type()
                .context("error reading table in table section")?;
            self.module.tables.push(table);
        }

        Ok(())
    }

    #[instrument(skip(self), level = "debug")]
    fn read_memory_section(&mut self) -> Result<()> {
        let memories = self.read_u32_leb128()?;
        debug!("got {memories} memories");
        for _ in 0..memories {
            let memory = self
                .read_memory_type()
                .context("error reading memory type in memory section")?;
            self.module.memories.push(memory);
        }

        Ok(())
    }

    #[instrument(skip(self), level = "debug")]
    fn read_global_section(&mut self) -> Result<()> {
        let globals = self.read_u32_leb128()?;
        debug!("got {globals} globals");
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
            debug!("read global: {global:?}");
            self.module.globals.push(global);
        }

        Ok(())
    }

    #[instrument(skip(self), level = "debug")]
    fn read_export_section(&mut self) -> Result<()> {
        let exports = self.read_u32_leb128()?;
        debug!("got {exports} exports");
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
            debug!("read export: {export:?}");

            self.module.exports.push(export);
        }

        Ok(())
    }

    #[instrument(skip(self), level = "debug")]
    fn read_start_section(&mut self) -> Result<()> {
        // Function index
        let index = self.read_u32_leb128()?;
        self.module.start = Some(index);

        info!("got start section index: {index}");
        Ok(())
    }

    #[instrument(skip(self), level = "debug")]
    fn read_elem_section(&mut self) -> Result<()> {
        let elems = self.read_u32_leb128()?;
        debug!("found {elems} elements");
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
                    let func_idx = self.read_u32_leb128()?;
                    let init_expr = InitExpr::RefFunc(func_idx);
                    functions.push(init_expr);
                }
                functions
            } else {
                let mut elems = vec![];
                for _ in 0..num_items {
                    elems.push(
                        self.read_init_expr()
                            .context("error reading element init expr")?,
                    );
                }
                elems
            };

            let element = Element {
                kind,
                types: ty,
                elems: items,
            };
            self.module.elements.push(element);
        }

        Ok(())
    }

    #[instrument(skip(self), level = "debug")]
    fn read_code_section(&mut self) -> Result<()> {
        let function_bodies = self.read_u32_leb128()?;
        debug!("found {function_bodies} function bodies");
        ensure!(
            function_bodies as usize == self.module.functions.len(),
            "should have the same amount of function bodies as types"
        );
        let mut total_locals = 0u64;
        let mut bodies = vec![];
        for _ in 0..function_bodies {
            debug!("began reading function body");
            let body_size = self.read_u32_leb128()?;
            // By the end of this function, we should hit this offset exactly.
            let end_offset = self.offset() + body_size as u64;
            // Number of local declarations
            let local_decls = self.read_u32_leb128()?;
            let mut locals = vec![];
            for _ in 0..local_decls {
                // Number of locals of this type
                let type_count = self.read_u32_leb128()?;
                total_locals += type_count as u64;
                ensure!(
                    u32::try_from(total_locals).is_ok(),
                    "too many locals in function, got: {total_locals}"
                );
                let valtype = self.read_type().context("error reading local type")?;
                locals.push(NumLocals {
                    num: type_count,
                    locals_type: valtype,
                });
            }
            // We slice the buffer to the end of the function body, then pass it to the `bodies`
            // vector so it can be parsed in parallel.
            let body = self.slice((end_offset - self.offset()) as usize)?;
            bodies.push((locals, body));
        }
        let codes = bodies
            .into_par_iter()
            .map(|(locals, body)| {
                let mut reader = InstrReader::new(body, &self.module);
                // We pass in `true` because there is a known end to the function body.
                let buf = reader
                    .read_instrs(true)
                    .context("error reading code body")?;
                ensure!(!reader.has_leftover(), "did not read all instructions");
                ensure!(
                    buf.last() == Some(Instruction::End),
                    "function body did not end with `end` instruction"
                );
                let code = Code { locals, body: buf };
                debug!(
                    "got code with locals: {:?} and {} instrs",
                    code.locals,
                    code.body.len()
                );
                trace!("code body: {}", code.body);
                Ok(code)
            })
            .collect::<Result<Vec<_>>>()?;
        self.module.codes = codes;

        Ok(())
    }

    #[instrument(skip(self), level = "debug")]
    fn read_data_section(&mut self) -> Result<()> {
        let data_segments = self.read_u32_leb128()?;
        debug!("got {data_segments} data segments");
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
            debug!("got data segment with length {}", data.data.len());
            self.module.datas.push(data);
        }

        Ok(())
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

        debug!("read table type: {table:?}");
        Ok(table)
    }

    fn read_memory_type(&mut self) -> Result<MemoryType> {
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

        debug!("read memory type with limit: {limit:?}");
        Ok(MemoryType { limit })
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

        debug!("read global type: {global:?}");
        Ok(global)
    }

    fn read_init_expr(&mut self) -> Result<InitExpr> {
        let to_end = &self.buf.get_ref()[self.offset() as usize..];
        let mut init_expr_reader = InstrReader::new(to_end, &self.module);
        // We pass false, meaning that it has no expected end. It should run until an `end`
        // instruction.
        let init_expr = init_expr_reader
            .read_instrs(false)
            .context("error reading init expr")?;
        // Advance the buffer to what the reader has read
        self.buf
            .set_position(self.offset() + init_expr_reader.offset());
        ensure!(
            init_expr.len() == 2,
            "init expr can must have 2 instructions"
        );
        // This is just a sanity check to make sure we didn't read until the end of the buffer (the
        // only other case when `read_instrs` would stop).
        let last = init_expr.last().unwrap();
        ensure!(
            last.opcode() == Opcode::End,
            "init expr must end with an end instruction"
        );
        let instr = init_expr.first().unwrap();
        let expr = match instr {
            Instruction::I32Const(val) => InitExpr::I32Const(val),
            Instruction::I64Const(val) => InitExpr::I64Const(val),
            Instruction::F32Const(val) => InitExpr::F32Const(val),
            Instruction::F64Const(val) => InitExpr::F64Const(val),
            Instruction::GlobalGet { idx } => InitExpr::ConstGlobalGet(idx),
            Instruction::RefNull { ty } => InitExpr::RefNull(ty),
            Instruction::RefFunc { func_idx } => InitExpr::RefFunc(func_idx),
            instr => bail!("init expr instruction `{instr}` is not const-valid"),
        };
        Ok(expr)
    }

    fn read_external_kind(&mut self) -> Result<ExternalKind> {
        let external_kind =
            ExternalKind::try_from_primitive(self.read_u8()?).context("invalid external kind")?;
        debug!("read external kind: {external_kind:?}");
        Ok(external_kind)
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
