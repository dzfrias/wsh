mod wasm_func;

use std::rc::Rc;

use shwasi_parser::{
    validate, Code, ElementKind, ExternalKind, FuncType, ImportKind, InstrBuffer, Module,
};

pub use self::wasm_func::*;
use crate::{
    error::{Error, Result},
    store::{
        Addr, DataInst, ElemInst, ExportInst, Extern, ExternVal, FuncInst, GlobalInst, MemInst,
        ModuleFunc, Store, TableInst,
    },
    vm::{self, Vm},
};

/// Memory page size in WebAssembly.
pub const PAGE_SIZE: usize = 65536;

/// An instance of a WebAssembly [`Module`].
///
/// These are cheap to clone, and should be passed around freely.
#[derive(Debug, Default, Clone)]
pub struct Instance {
    inner: Rc<InstanceInner>,
}

/// Inner representation of an [`Instance`].
#[derive(Debug, Default)]
struct InstanceInner {
    types: Vec<FuncType>,
    exports: Vec<ExportInst>,

    func_addrs: Vec<Addr>,
    table_addrs: Vec<Addr>,
    mem_addrs: Vec<Addr>,
    global_addrs: Vec<Addr>,
    elem_addrs: Vec<Addr>,
    data_addrs: Vec<Addr>,
}

impl Instance {
    pub fn types(&self) -> &[FuncType] {
        &self.inner.types
    }

    pub fn exports(&self) -> &[ExportInst] {
        &self.inner.exports
    }

    pub fn func_addrs(&self) -> &[Addr] {
        &self.inner.func_addrs
    }

    pub fn table_addrs(&self) -> &[Addr] {
        &self.inner.table_addrs
    }

    pub fn mem_addrs(&self) -> &[Addr] {
        &self.inner.mem_addrs
    }

    pub fn global_addrs(&self) -> &[Addr] {
        &self.inner.global_addrs
    }

    pub fn elem_addrs(&self) -> &[Addr] {
        &self.inner.elem_addrs
    }

    pub fn data_addrs(&self) -> &[Addr] {
        &self.inner.data_addrs
    }
}

impl Instance {
    /// Instantiate a module with the given [`ExternVal`]s and [`Store`].
    ///
    /// This will allocate the module's imports into the store, and allocate the module's different
    /// sections, and run the module's start function, if present.
    pub fn instantiate(store: &mut Store, mut module: Module) -> Result<Self> {
        validate(&module).map_err(Error::Validation)?;

        let mut inst = InstanceInner::default();

        for import in module.imports.iter() {
            let extern_val = store.resolve(import.module, import.field).ok_or_else(|| {
                Error::ExternNotFound {
                    module: import.module.to_owned(),
                    field: import.field.to_owned(),
                }
            })?;
            let ty = store.data.types.get(&extern_val).unwrap();
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
                ExternVal::Func(addr) => inst.func_addrs.push(addr),
                ExternVal::Table(addr) => inst.table_addrs.push(addr),
                ExternVal::Mem(addr) => inst.mem_addrs.push(addr),
                ExternVal::Global(addr) => inst.global_addrs.push(addr),
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
            let inst = DataInst(data.data.to_owned());
            store.data.datas.push(inst);
            store.data.datas.len() - 1
        }));

        let inst = Instance {
            inner: Rc::new(inst),
        };

        // Allocate functions into store
        for (i, func) in module.functions.into_iter().enumerate() {
            let ty = &inst.types()[func.index as usize];
            let code = std::mem::replace(
                &mut module.codes[i],
                Code {
                    locals: vec![],
                    body: InstrBuffer::new(),
                },
            );
            let func_inst = FuncInst::Module(ModuleFunc {
                ty: ty.clone(),
                code,
                inst: inst.clone(),
            });
            store.data.functions.push(func_inst);
            store.data.types.insert(
                ExternVal::Func(store.data.functions.len() - 1),
                Extern::Func(ty.clone()),
            );
        }

        // Initialize tables with element segments
        for (elem_addr, elem) in inst.elem_addrs().iter().zip(&module.elements) {
            let elem_inst = &mut store.mut_.elems[*elem_addr];
            match elem.kind {
                ElementKind::Passive => {}
                ElementKind::Declarative => elem_inst.elem_drop(),
                ElementKind::Active { tbl_idx, offset } => {
                    let offset =
                        vm::eval_const_expr(&store.mut_.globals, &imported_globals, &offset)
                            .as_u32();
                    for (i, func_idx) in elem_inst.elems.iter().enumerate() {
                        let func_addr = func_idx.map(|i| inst.func_addrs()[i]);
                        store.mut_.tables[inst.table_addrs()[tbl_idx as usize]].elements
                            [offset as usize + i] = func_addr;
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
            let mem = &mut store.mut_.memories[inst.mem_addrs()[0]];
            mem.data[offset..offset + data.data.len()].copy_from_slice(data.data);
        }

        if let Some(start) = module.start {
            let func_addr = inst.func_addrs()[start as usize];
            let mut vm = Vm::new(&store.data, &mut store.mut_, inst.clone());
            vm.call(func_addr, &[]).map_err(Error::Trap)?;
        }

        Ok(inst)
    }

    pub fn export_as(&self, store: &mut Store, name: &str) {
        store.data.instances.insert(name.to_owned(), self.clone());
    }

    /// Get a function by name and type.
    ///
    /// # Panics
    /// Panics if the function's type doesn't match the given type params and results. Note that
    /// this behavior is different from [`Self::get_func_untyped`], which returns an error at call
    /// time instead.
    ///
    /// This discrepancy exists because this function is intended to be used with functions who's
    /// types are known at compile time.
    pub fn get_func<Params, Results>(
        &self,
        store: &mut Store,
        name: &str,
    ) -> Result<WasmFunc<Params, Results>>
    where
        Params: WasmParams,
        Results: WasmResults,
    {
        let func = self
            .exports()
            .iter()
            .find(|export| export.name == name)
            .ok_or_else(|| Error::FunctionNotFound(name.to_owned()))?;
        let ExternVal::Func(func_addr) = func.reference else {
            return Err(Error::AttemptingToCallNonFunction(func.reference));
        };
        let f = &store.data.functions[func_addr];
        if !Params::matches(f.ty().0.iter().copied()) {
            panic!("typed func \"{name}\" params don't match, got {}", f.ty());
        }
        if !Results::matches(f.ty().0.iter().copied()) {
            panic!("typed func \"{name}\" results don't match, got {}", f.ty());
        }
        Ok(WasmFunc::new(func_addr, self.clone()))
    }

    /// Get a function by name. Types are not validated until the function is called.
    ///
    /// For the the version of this function that provides a type-safe API for calling WebAssembly
    /// functions, see [`Self::get_func`].
    pub fn get_func_untyped(&self, _store: &mut Store, name: &str) -> Result<WasmFuncUntyped> {
        let func = self
            .exports()
            .iter()
            .find(|export| export.name == name)
            .ok_or_else(|| Error::FunctionNotFound(name.to_owned()))?;
        let ExternVal::Func(func_addr) = func.reference else {
            return Err(Error::AttemptingToCallNonFunction(func.reference));
        };
        Ok(WasmFuncUntyped::new(func_addr, self.clone()))
    }
}
