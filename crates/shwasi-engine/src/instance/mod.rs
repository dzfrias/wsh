#![allow(dead_code)]

use std::rc::Rc;

use shwasi_parser::{
    validate, Code, ElementKind, ExternalKind, FuncType, ImportKind, InstrBuffer, Module,
};

use crate::{
    error::{Error, Result},
    store::{
        Addr, DataInst, ElemInst, ExportInst, Extern, ExternVal, FuncInst, GlobalInst, MemInst,
        ModuleFunc, Store, TableInst,
    },
    vm,
};

/// Memory page size in WebAssembly.
pub const PAGE_SIZE: usize = 65536;

/// An instance of a WebAssembly [`Module`].
///
/// This can be interacted with by calling functions, getting and setting globals, and
/// reading/writing memory.
#[derive(Debug, Default)]
pub struct Instance {
    pub types: Vec<FuncType>,
    pub exports: Vec<ExportInst>,

    pub func_addrs: Vec<Addr>,
    pub table_addrs: Vec<Addr>,
    pub mem_addrs: Vec<Addr>,
    pub global_addrs: Vec<Addr>,
    pub elem_addrs: Vec<Addr>,
    pub data_addrs: Vec<Addr>,
}

impl Instance {
    /// Instantiate a module with the given [`ExternVal`]s and [`Store`].
    ///
    /// This will allocate the module's imports into the store, and allocate the module's different
    /// sections, and run the module's start function, if present.
    pub fn instantiate(
        mut module: Module,
        store: &mut Store,
        externs: &[ExternVal],
    ) -> Result<Rc<Self>> {
        validate(&module).map_err(Error::Validation)?;

        if module.imports.len() != externs.len() {
            return Err(Error::InvalidExternLength {
                want: module.imports.len(),
                got: externs.len(),
            });
        }

        let mut inst = Self::default();

        for (extern_val, import) in externs.iter().zip(module.imports.iter()) {
            let ty = store
                .data
                .types
                .get(extern_val)
                .ok_or(Error::ExternNotFound(*extern_val))?;
            let import_ty = match &import.kind {
                ImportKind::Function(idx) => Extern::Func(module.types[*idx as usize].clone()),
                ImportKind::Table(table) => Extern::Table(table.clone()),
                ImportKind::Memory(mem) => Extern::Mem(mem.clone()),
                ImportKind::Global(global) => Extern::Global(global.clone()),
            };
            if ty != &import_ty {
                return Err(Error::BadExternType {
                    want: import_ty,
                    got: ty.clone(),
                });
            }

            match extern_val {
                ExternVal::Func(addr) => inst.func_addrs.push(*addr),
                ExternVal::Table(addr) => inst.table_addrs.push(*addr),
                ExternVal::Mem(addr) => inst.mem_addrs.push(*addr),
                ExternVal::Global(addr) => inst.global_addrs.push(*addr),
            }
        }
        let imported_globals = inst.global_addrs.clone();

        // Put types in the instance
        inst.types = module.types;

        // Allocate indices, as FuncInst's can only be put in store after module is put in an Rc
        let fsi_min = store.data.functions.len();
        let fsi_max = fsi_min + module.functions.len();
        for addr in fsi_min..fsi_max {
            inst.func_addrs.push(addr);
        }

        inst.mem_addrs
            .extend(module.memories.into_iter().map(|mem| {
                let init = mem.limit.initial as usize * PAGE_SIZE;
                // Create the memory based on the initial size
                store.mut_.memories.push(MemInst {
                    ty: mem.clone(),
                    data: vec![0; init],
                });
                let addr = store.mut_.memories.len() - 1;
                store
                    .data
                    .types
                    .insert(ExternVal::Mem(addr), Extern::Mem(mem));
                addr
            }));
        inst.global_addrs
            .extend(module.globals.into_iter().map(|global| {
                let value =
                    vm::eval_const_expr(&store.mut_.globals, &imported_globals, &global.init);
                store.mut_.globals.push(GlobalInst {
                    ty: global.kind.clone(),
                    value,
                });
                let addr = store.mut_.globals.len() - 1;
                store
                    .data
                    .types
                    .insert(ExternVal::Global(addr), Extern::Global(global.kind));
                addr
            }));
        inst.table_addrs
            .extend(module.tables.into_iter().map(|table| {
                let init = table.limit.initial;
                let inst = TableInst {
                    ty: table.clone(),
                    elements: vec![None; init as usize],
                };
                store.mut_.tables.push(inst);
                let addr = store.mut_.tables.len() - 1;
                store
                    .data
                    .types
                    .insert(ExternVal::Table(addr), Extern::Table(table));
                addr
            }));

        inst.exports
            .extend(module.exports.into_iter().map(|export| {
                let idx = export.external_idx;
                ExportInst {
                    name: export.field.to_owned(),
                    reference: match export.kind {
                        ExternalKind::Function => ExternVal::Func(inst.func_addrs[idx as usize]),
                        ExternalKind::Table => ExternVal::Table(inst.table_addrs[idx as usize]),
                        ExternalKind::Memory => ExternVal::Mem(inst.mem_addrs[idx as usize]),
                        ExternalKind::Global => ExternVal::Global(inst.global_addrs[idx as usize]),
                    },
                }
            }));
        inst.elem_addrs.extend(module.elements.iter().map(|elem| {
            let inst = ElemInst {
                ty: elem.types,
                elems: elem
                    .elems
                    .iter()
                    .map(|init| {
                        vm::eval_const_expr(&store.mut_.globals, &imported_globals, init).as_ref()
                    })
                    .collect(),
            };
            store.mut_.elems.push(inst);
            store.mut_.elems.len() - 1
        }));
        inst.data_addrs.extend(module.datas.iter().map(|data| {
            let inst = DataInst(data.data.to_vec());
            store.mut_.datas.push(inst);
            store.mut_.datas.len() - 1
        }));

        let inst = Rc::new(inst);

        // Allocate functions into store
        for func in module.functions {
            let ty = &inst.types[func.index as usize];
            let code = std::mem::replace(
                &mut module.codes[func.index as usize],
                Code {
                    locals: vec![],
                    body: InstrBuffer::new(),
                },
            );
            let func_inst = FuncInst::Module(ModuleFunc {
                ty: ty.clone(),
                code,
                inst: Rc::clone(&inst),
            });
            store.data.functions.push(func_inst);
            store.data.types.insert(
                ExternVal::Func(store.data.functions.len() - 1),
                Extern::Func(ty.clone()),
            );
        }

        // Initialize tables with element segments
        for (elem_addr, elem) in inst.elem_addrs.iter().zip(&module.elements) {
            let elem_inst = &mut store.mut_.elems[*elem_addr];
            match elem.kind {
                ElementKind::Passive => {}
                ElementKind::Declarative => elem_inst.elem_drop(),
                ElementKind::Active { tbl_idx, offset } => {
                    let offset =
                        vm::eval_const_expr(&store.mut_.globals, &imported_globals, &offset)
                            .as_u32();
                    for (i, func_idx) in elem_inst.elems.iter().enumerate() {
                        let funcaddr = inst.func_addrs[*func_idx];
                        store.mut_.tables[inst.table_addrs[tbl_idx as usize]].elements
                            [offset as usize + i] = Some(funcaddr);
                    }
                    elem_inst.elem_drop();
                }
            }
        }
        // Initialize memory using data segmengts
        for data in &module.datas {
            let Some(offset_expr) = &data.offset else {
                continue;
            };
            let offset = vm::eval_const_expr(&store.mut_.globals, &imported_globals, offset_expr)
                .as_u32() as usize;
            let mem = &mut store.mut_.memories[inst.mem_addrs[0]];
            mem.data[offset..offset + data.data.len()].copy_from_slice(data.data);
        }

        // TODO: run start

        Ok(inst)
    }
}
