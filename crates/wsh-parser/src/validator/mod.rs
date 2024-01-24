mod func_validator;

use std::collections::HashSet;

use anyhow::{bail, ensure, Context, Result};
use rayon::prelude::*;
use tracing::{debug, info, instrument, trace};

use crate::{
    validator::func_validator::FuncValidator, Code, Data, Element, ElementKind, Export,
    ExternalKind, FuncType, Function, Global, GlobalType, Import, ImportKind, InitExpr, Limit,
    MemoryType, Module, RefType, TableType, ValType,
};

/// Validate a WebAssembly module.
pub fn validate(m: &Module) -> Result<()> {
    Validator::new().validate_module(m)
}

#[derive(Debug)]
struct ValidatorGlobal<'a> {
    ty: &'a GlobalType,
    // Some globals can only be referenced if they've been imported, so this field keeps track of
    // that
    is_imported: bool,
}

/// A validator for a WebAssembly module.
///
/// Contains the current state of validation for the module.
#[derive(Debug)]
struct Validator<'a> {
    /// Types decalred in the module.
    module_tys: &'a [FuncType],
    elems: &'a [Element],

    /// Functions, either imported or declared in the module.
    funcs: Vec<&'a FuncType>,
    tables: Vec<&'a TableType>,
    mems: Vec<&'a MemoryType>,
    globals: Vec<ValidatorGlobal<'a>>,
    // Used to determine if a `RefFunc` instruction is valid. For now, a function is "declared" if
    // it is exported or if it is present in an InitExpr.
    declared_funcs: HashSet<u32>,
    datas_found: usize,
}

impl<'a> Validator<'a> {
    pub fn new() -> Self {
        Self {
            module_tys: &[],
            declared_funcs: HashSet::new(),
            funcs: vec![],
            tables: vec![],
            mems: vec![],
            globals: vec![],
            datas_found: 0,
            elems: &[],
        }
    }

    #[instrument(level = "debug", skip_all)]
    fn validate_module(mut self, m: &'a Module) -> Result<()> {
        info!("began module validation");

        self.module_tys = &m.types;
        self.elems = &m.elements;

        self.validate_imports(&m.imports)
            .context("error validating module imports")?;

        for func in &m.functions {
            let ty = m
                .types
                .get(func.index as usize)
                .context("function type not found in functions section")?;
            self.funcs.push(ty);
        }
        self.tables.extend(&m.tables);
        self.mems.extend(&m.memories);
        self.datas_found = m.datas.len();

        // Validate globals before adding them to validator to prevent self-referencing
        self.valdate_globals(&m.globals)
            .context("error validating module globals")?;
        self.globals
            .extend(m.globals.iter().map(|g| ValidatorGlobal {
                ty: &g.kind,
                is_imported: false,
            }));

        trace!("begin current module state");
        trace!("module_tys: {:?}", self.module_tys);
        trace!("elems: {:?}", self.elems);
        trace!("funcs: {:?}", self.funcs);
        trace!("datas_found: {}", self.datas_found);
        trace!("globals: {:?}", self.globals);
        trace!("end current module state");

        self.validate_elems(&m.elements)
            .context("error validating module elements")?;
        self.validate_tables(&m.tables)
            .context("error validating module tables")?;
        self.validate_memories(&m.memories)
            .context("error validating module memories")?;
        self.validate_datas(&m.datas)
            .context("error validating module datas")?;

        if let Some(start) = m.start {
            let ty = self
                .funcs
                .get(start as usize)
                .context("start function not found")?;
            ensure!(ty.0.is_empty(), "start function params not zero");
            ensure!(ty.1.is_empty(), "start function results not zero");
            debug!("validated start function: {start}");
        }

        self.validate_exports(&m.exports)
            .context("error validating module exports")?;

        self.validate_codes(&m.codes, &m.functions)
            .context("error vaildating module codes")?;

        // Multi-memories not supported
        ensure!(
            self.mems.len() <= 1,
            "too many memores, only one is supported"
        );

        info!("finisehd module validation");
        Ok(())
    }

    #[inline]
    fn get_ty(&self, idx: u32) -> Result<&'a FuncType> {
        // Type must exist
        self.funcs
            .get(idx as usize)
            .copied()
            .context("function type not found")
    }

    #[instrument(level = "debug", skip(self))]
    fn validate_imports(&mut self, imports: &'a [Import]) -> Result<()> {
        for import in imports {
            match &import.kind {
                ImportKind::Function(idx) => {
                    let ty = self
                        .module_tys
                        .get(*idx as usize)
                        .context("function type not found")?;
                    self.funcs.push(ty);
                }
                ImportKind::Table(table) => {
                    self.validate_limit(&table.limit)
                        .context("invalid imported table")?;
                    self.tables.push(table);
                }
                ImportKind::Memory(mem) => {
                    self.validate_mem(mem).context("invalid imported memory")?;
                    self.mems.push(mem);
                }
                ImportKind::Global(global) => self.globals.push(ValidatorGlobal {
                    ty: global,
                    is_imported: true,
                }),
            }
            debug!("validated import: {import}");
        }

        Ok(())
    }

    #[allow(clippy::unused_self)]
    fn validate_limit(&self, limit: &Limit) -> Result<()> {
        let Some(max) = limit.max else {
            return Ok(());
        };
        ensure!(limit.initial <= max, "invalid limit: {limit}");
        Ok(())
    }

    #[instrument(level = "debug", skip(self))]
    fn validate_tables(&self, tables: &[TableType]) -> Result<()> {
        for tbl in tables {
            self.validate_limit(&tbl.limit)
                .context("invalid table limit")?;
            debug!("validated table: {tbl}");
        }

        Ok(())
    }

    #[instrument(level = "debug", skip(self))]
    fn validate_memories(&self, mems: &[MemoryType]) -> Result<()> {
        for mem in mems {
            self.validate_mem(mem)?;
        }

        Ok(())
    }

    #[instrument(level = "debug", skip(self))]
    fn validate_mem(&self, mem: &MemoryType) -> Result<()> {
        // Memory limit must be less than 4GB
        const MAX: u64 = (1u64 << 32) / 65536;

        if let Some(max) = mem.limit.max {
            let max = max as u64;
            ensure!(max <= MAX, "memory too large: {max}");
        }
        let initial = mem.limit.initial as u64;
        ensure!(initial <= MAX, "memory too large: {initial}");

        self.validate_limit(&mem.limit)
            .context("invalid limit for memory")?;

        debug!("validated memory: {mem}");
        Ok(())
    }

    #[instrument(level = "debug", skip(self))]
    fn validate_datas(&mut self, datas: &[Data]) -> Result<()> {
        for init in datas.iter().filter_map(|data| data.offset) {
            ensure!(!self.mems.is_empty(), "no memory to copy data section to");
            // Data offset into memory must be an integer
            self.validate_init_expr(init, ValType::I32)
                .context("invalid offset expr for data")?;
            debug!("validated data offset: {init}");
        }

        Ok(())
    }

    fn validate_init_expr(&mut self, init: InitExpr, expected: ValType) -> Result<()> {
        let ty = match init {
            InitExpr::I32Const(_) => ValType::I32,
            InitExpr::I64Const(_) => ValType::I64,
            InitExpr::F32Const(_) => ValType::F32,
            InitExpr::F64Const(_) => ValType::F64,
            InitExpr::ConstGlobalGet(idx) => {
                // Global must exist
                let global = self
                    .globals
                    .get(idx as usize)
                    .context("init expr global not found")?;
                ensure!(
                    global.is_imported && !global.ty.mutable,
                    "cannot initialize with-non imported mutable global const expr"
                );
                global.ty.content_type
            }
            InitExpr::RefNull(t) => match t {
                RefType::Func => ValType::Func,
                RefType::Extern => ValType::Extern,
            },
            InitExpr::RefFunc(idx) => {
                ensure!(
                    self.funcs.get(idx as usize).is_some(),
                    "init expr function reference not found"
                );
                // Function is "declared"
                self.declared_funcs.insert(idx);
                ValType::Func
            }
        };

        ensure!(
            expected == ty,
            "unexepected init expr type: {expected} != {ty}"
        );

        debug!("validated init expr {init} => {ty}");
        Ok(())
    }

    #[instrument(level = "debug", skip(self))]
    fn valdate_globals(&mut self, globals: &[Global]) -> Result<()> {
        for global in globals {
            // Global init expr must match global type
            self.validate_init_expr(global.init, global.kind.content_type)
                .context("invalid global init expr")?;
            debug!("validated global: {global}");
        }

        Ok(())
    }

    #[instrument(level = "debug", skip(self))]
    fn validate_elems(&mut self, elements: &[Element]) -> Result<()> {
        for elem in elements {
            debug!("started validating element");

            if let ElementKind::Active { tbl_idx, offset } = elem.kind {
                let Some(tbl) = self.tables.get(tbl_idx as usize) else {
                    bail!("table not found: {tbl_idx}");
                };
                // Elem type must match table type
                ensure!(
                    tbl.elem_type == elem.types,
                    "elem type mismatch: {} != {}",
                    tbl.elem_type,
                    elem.types
                );
                // Offset must be an integer
                self.validate_init_expr(offset, ValType::I32)
                    .context("invalid element init expr")?;
            }

            for init in &elem.elems {
                // Init expr must match elem type (either funcref or externref)
                self.validate_init_expr(*init, elem.types.into())
                    .context("invalid value for element")?;
            }
            debug!("validated element: {elem}");
        }

        Ok(())
    }

    #[instrument(level = "debug", skip(self))]
    fn validate_exports(&mut self, exports: &[Export]) -> Result<()> {
        let mut seen_exports = Vec::with_capacity(exports.len());
        for export in exports {
            // Exports must be unique
            ensure!(
                !seen_exports.contains(&export.field),
                "duplicate export of {}",
                export.field
            );
            let idx = export.external_idx as usize;
            match export.kind {
                ExternalKind::Function => {
                    ensure!(
                        self.funcs.get(idx).is_some(),
                        "export function type not found"
                    );
                    self.declared_funcs.insert(export.external_idx);
                }
                ExternalKind::Table => {
                    ensure!(self.tables.get(idx).is_some(), "export table not found");
                }
                ExternalKind::Memory => {
                    ensure!(self.mems.get(idx).is_some(), "export memory not found");
                }
                ExternalKind::Global => {
                    ensure!(self.globals.get(idx).is_some(), "export global not found");
                }
            }
            seen_exports.push(export.field);
            debug!("validated export: {export}");
            trace!("seen exports: {seen_exports:?}");
        }

        Ok(())
    }

    #[instrument(level = "debug", skip_all)]
    fn validate_codes(&mut self, codes: &[Code], funcs: &[Function]) -> Result<()> {
        // Paralellizing here speeds up validation by around 80%
        codes
            .par_iter()
            .zip(funcs)
            .try_for_each(|(code, func)| -> Result<()> {
                let mut validator = FuncValidator::new(self);
                validator.validate(code, func)?;
                Ok(())
            })
    }
}
