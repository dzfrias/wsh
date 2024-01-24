mod into_host_func;
mod wasm_func;

use std::rc::Rc;

use wsh_parser::{
    validate, Code, ElementKind, ExternalKind, FuncType, GlobalType, ImportKind, InstrBuffer,
    Module,
};

pub use self::into_host_func::IntoHostFunc;
pub use self::wasm_func::*;
use crate::{
    error::{ErrorKind, Result},
    store::{
        Addr, Data, Element, Export, Extern, ExternVal, Func, Global, Memory, ModuleFunc, Store,
        Table,
    },
    vm::{self, Vm},
    Trap, Value,
};

/// Memory page size in WebAssembly.
pub const PAGE_SIZE: usize = 65536;

/// An instance of a WebAssembly [`Module`].
///
/// These are cheap to clone, and should be passed around freely.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct Instance {
    inner: Rc<InstanceInner>,
}

/// Inner representation of an [`Instance`].
#[derive(Debug, Default, PartialEq)]
struct InstanceInner {
    types: Vec<FuncType>,
    exports: Vec<Export>,

    imports: Vec<ExternVal>,
    func_addrs: Vec<Addr<Func>>,
    table_addrs: Vec<Addr<Table>>,
    mem_addrs: Vec<Addr<Memory>>,
    global_addrs: Vec<Addr<Global>>,
    elem_addrs: Vec<Addr<Element>>,
    data_addrs: Vec<Addr<Data>>,
}

// Public impl
impl Instance {
    /// Instantiate a module with the given [`Store`].
    ///
    /// This will allocate the module's imports into the store, and allocate the module's different
    /// sections, and run the module's start function, if present.
    pub fn instantiate(store: &mut Store, mut module: Module) -> Result<Self> {
        validate(&module).map_err(ErrorKind::ValidationError)?;

        let mut inst = InstanceInner::default();

        for import in module.imports.iter() {
            // This will give us an an `ExternVal`, which holds data about the address of the
            // extern we're workign with.
            let extern_val = store.resolve(import.module, import.field).ok_or_else(|| {
                ErrorKind::ExternNotFound {
                    module: import.module.to_owned(),
                    field: import.field.to_owned(),
                }
            })?;
            // `ty` is the type of the extern **from the store**, using the address we just got.
            let ty = match extern_val {
                ExternVal::Mem(addr) => Extern::Mem(store.memories[addr].ty.clone()),
                ExternVal::Func(addr) => Extern::Func(store.functions[addr].ty().clone()),
                ExternVal::Table(addr) => Extern::Table(store.tables[addr].ty().clone()),
                ExternVal::Global(addr) => {
                    let global = &store.globals[addr];
                    Extern::Global(GlobalType {
                        mutable: global.mutable,
                        content_type: global.value.ty(),
                    })
                }
            };
            // This is the type of the extern from the module imports.
            let import_ty = match &import.kind {
                ImportKind::Function(idx) => Extern::Func(module.types[*idx as usize].clone()),
                ImportKind::Table(table) => Extern::Table(table.clone()),
                ImportKind::Memory(mem) => Extern::Mem(mem.clone()),
                ImportKind::Global(global) => Extern::Global(*global),
            };
            if !ty.matches(&import_ty) {
                return Err(ErrorKind::BadExtern {
                    want: import_ty,
                    got: ty.clone(),
                }
                .into());
            }

            inst.imports.push(extern_val);
            match extern_val {
                ExternVal::Func(addr) => inst.func_addrs.push(addr),
                ExternVal::Table(addr) => inst.table_addrs.push(addr),
                ExternVal::Mem(addr) => inst.mem_addrs.push(addr),
                ExternVal::Global(addr) => inst.global_addrs.push(addr),
            }
        }
        let imported_globals = inst.global_addrs.clone();

        inst.types = module.types;

        let fsi_min = store.functions.len();
        let mut fsi_max = fsi_min + module.functions.len();
        inst.func_addrs.reserve_exact(fsi_max - fsi_min);
        // This is pretty horrific. We need to allocate the addresses **before** the instance is
        // put in an Arc, because functions require an Instance. To get around this, we
        // pre-add the addresses to the instance to where they _should_ be. For now, this works,
        // but this could EASILY break if the allocation strategy got slightly more complex, and
        // relies on a lot of implementation details.
        for i in store
            .functions
            .all_free()
            .into_iter()
            .take(module.functions.len())
        {
            inst.func_addrs.push(Addr::new(i));
            fsi_max -= 1;
        }
        for addr in fsi_min..fsi_max {
            inst.func_addrs.push(Addr::new(addr));
        }

        inst.mem_addrs
            .extend(module.memories.into_iter().map(|mem| {
                let init = mem.limit.initial as usize * PAGE_SIZE;
                // Create the memory based on the initial size
                store.memories.alloc(Memory {
                    ty: mem.clone(),
                    data: vec![0; init],
                })
            }));
        store.globals.reserve(module.globals.len());
        inst.global_addrs
            .extend(module.globals.into_iter().map(|global| {
                let value = vm::eval_const_expr(
                    &store.globals,
                    &imported_globals,
                    &inst.func_addrs,
                    &global.init,
                );
                store.globals.alloc(Global {
                    mutable: global.kind.mutable,
                    value: value.into_typed(global.kind.content_type),
                })
            }));
        store.tables.reserve(module.tables.len());
        inst.table_addrs.extend(
            module
                .tables
                .into_iter()
                .map(|table| store.tables.alloc(Table::new(table.clone()))),
        );

        inst.exports
            .extend(module.exports.into_iter().map(|export| {
                let idx = export.external_idx;
                Export {
                    name: export.field.to_owned(),
                    reference: match export.kind {
                        ExternalKind::Function => ExternVal::Func(inst.func_addrs[idx as usize]),
                        ExternalKind::Table => ExternVal::Table(inst.table_addrs[idx as usize]),
                        ExternalKind::Memory => ExternVal::Mem(inst.mem_addrs[idx as usize]),
                        ExternalKind::Global => ExternVal::Global(inst.global_addrs[idx as usize]),
                    },
                }
            }));
        store.elems.reserve(module.elements.len());
        inst.elem_addrs.extend(module.elements.iter().map(|elem| {
            let inst = Element {
                elems: elem
                    .elems
                    .iter()
                    .map(|init| {
                        vm::eval_const_expr(
                            &store.globals,
                            &imported_globals,
                            &inst.func_addrs,
                            init,
                        )
                        .as_ref()
                    })
                    .collect(),
            };
            store.elems.alloc(inst)
        }));
        store.datas.reserve(module.datas.len());
        inst.data_addrs.extend(module.datas.iter().map(|data| {
            let inst = Data(data.data.to_owned());
            store.datas.alloc(inst)
        }));

        let inst = Instance {
            inner: Rc::new(inst),
        };

        // Allocate functions into store
        store.functions.reserve(module.functions.len());
        for (i, func) in module.functions.into_iter().enumerate() {
            let ty = &inst.types()[func.index as usize];
            let code = std::mem::replace(
                &mut module.codes[i],
                Code {
                    locals: vec![],
                    body: InstrBuffer::new(),
                },
            );
            let func_inst = Func::Module(ModuleFunc {
                ty: ty.clone(),
                code: Box::new(code),
                inst: inst.clone(),
            });
            store.functions.alloc(func_inst);
        }

        // Initialize tables with element segments
        for (elem_addr, elem) in inst.elem_addrs().iter().zip(&module.elements) {
            let elem_inst = &mut store.elems[*elem_addr];
            match elem.kind {
                ElementKind::Passive => {}
                ElementKind::Declarative => elem_inst.elem_drop(),
                ElementKind::Active { tbl_idx, offset } => {
                    let offset = vm::eval_const_expr(
                        &store.globals,
                        &imported_globals,
                        inst.func_addrs(),
                        &offset,
                    )
                    .as_u32();
                    let len = store.tables[inst.table_addrs()[tbl_idx as usize]].size();
                    let new_offset = offset.saturating_add(elem_inst.elems.len() as u32);
                    if new_offset > len {
                        return Err(ErrorKind::Trap(Trap::TableGetOutOfBounds).into());
                    }
                    for (i, func_idx) in elem_inst.elems.iter().enumerate() {
                        let tbl = &mut store.tables[inst.table_addrs()[tbl_idx as usize]];
                        tbl.set(offset + i as u32, *func_idx).unwrap();
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
            let offset = vm::eval_const_expr(
                &store.globals,
                &imported_globals,
                inst.func_addrs(),
                offset_expr,
            )
            .as_u32() as usize;
            let mem = &mut store.memories[inst.mem_addrs()[0]];
            if offset + data.data.len() > mem.data.len() {
                return Err(ErrorKind::Trap(Trap::MemoryAccessOutOfBounds).into());
            }
            mem.data[offset..offset + data.data.len()].copy_from_slice(data.data);
        }

        if let Some(start) = module.start {
            let func_addr = inst.func_addrs()[start as usize];
            let mut vm = Vm::new(store, inst.clone());
            vm.call(func_addr, [])?;
        }

        Ok(inst)
    }

    pub fn export_as(&self, store: &mut Store, name: &str) {
        store.instances.insert(name.to_owned(), self.clone());
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
        store: &Store,
        name: &str,
    ) -> Result<WasmFunc<Params, Results>>
    where
        Params: WasmParams,
        Results: WasmResults,
    {
        let func = self
            .find_export(name)
            .ok_or_else(|| ErrorKind::FunctionNotFound(name.to_owned()))?;
        let ExternVal::Func(func_addr) = func.reference else {
            return Err(ErrorKind::AttemptingToCallNonFunction(func.reference.ty()).into());
        };
        let f = &store.functions[func_addr];
        if !Params::matches(f.ty().0.iter().copied()) {
            panic!("typed func \"{name}\" params don't match, got {}", f.ty());
        }
        if !Results::matches(f.ty().1.iter().copied()) {
            panic!("typed func \"{name}\" results don't match, got {}", f.ty());
        }
        Ok(WasmFunc::new(func_addr, self.clone()))
    }

    /// Get a function by name. Types are not validated until the function is called.
    ///
    /// For the the version of this function that provides a type-safe API for calling WebAssembly
    /// functions, see [`Self::get_func`].
    pub fn get_func_untyped(&self, _store: &Store, name: &str) -> Result<WasmFuncUntyped> {
        let func = self
            .find_export(name)
            .ok_or_else(|| ErrorKind::FunctionNotFound(name.to_owned()))?;
        let ExternVal::Func(func_addr) = func.reference else {
            return Err(ErrorKind::AttemptingToCallNonFunction(func.reference.ty()).into());
        };
        Ok(WasmFuncUntyped::new(func_addr, self.clone()))
    }

    /// Get an exported global by name.
    pub fn get_global(&self, store: &Store, name: &str) -> Option<Value> {
        let global = self.find_export(name)?;
        let ExternVal::Global(addr) = global.reference else {
            return None;
        };
        let global = &store.globals[addr];
        Some(global.value)
    }

    pub fn get_mem<'a>(&self, store: &'a mut Store) -> Option<&'a mut Memory> {
        let mem_addr = self.mem_addrs().first()?;
        Some(&mut store.memories[*mem_addr])
    }

    /// Drop everything in the store related to this Instance.
    pub fn free(self, store: &mut Store) {
        for addr in self
            .func_addrs()
            .iter()
            .filter(|addr| !self.inner.imports.contains(&ExternVal::Func(**addr)))
        {
            store.functions.free(*addr);
        }
        for data in self.data_addrs() {
            store.datas.free(*data);
        }
        for mem in self
            .mem_addrs()
            .iter()
            .filter(|addr| !self.inner.imports.contains(&ExternVal::Mem(**addr)))
        {
            store.memories.free(*mem);
        }
        for global in self
            .global_addrs()
            .iter()
            .filter(|addr| !self.inner.imports.contains(&ExternVal::Global(**addr)))
        {
            store.globals.free(*global);
        }
        for elem in self.elem_addrs() {
            store.elems.free(*elem);
        }
        for tbl in self
            .table_addrs()
            .iter()
            .filter(|addr| !self.inner.imports.contains(&ExternVal::Table(**addr)))
        {
            store.tables.free(*tbl);
        }

        store.instances.retain(|_, inst| inst != &self);
    }
}

// Private impl
impl Instance {
    pub(crate) fn types(&self) -> &[FuncType] {
        &self.inner.types
    }

    pub(crate) fn exports(&self) -> &[Export] {
        &self.inner.exports
    }

    pub(crate) fn func_addrs(&self) -> &[Addr<Func>] {
        &self.inner.func_addrs
    }

    pub(crate) fn table_addrs(&self) -> &[Addr<Table>] {
        &self.inner.table_addrs
    }

    pub(crate) fn mem_addrs(&self) -> &[Addr<Memory>] {
        &self.inner.mem_addrs
    }

    pub(crate) fn global_addrs(&self) -> &[Addr<Global>] {
        &self.inner.global_addrs
    }

    pub(crate) fn elem_addrs(&self) -> &[Addr<Element>] {
        &self.inner.elem_addrs
    }

    pub(crate) fn data_addrs(&self) -> &[Addr<Data>] {
        &self.inner.data_addrs
    }

    pub(crate) fn find_export(&self, name: &str) -> Option<&Export> {
        self.exports().iter().find(|export| export.name == name)
    }
}
