use std::marker::PhantomData;

use shwasi_parser::ValType;

use crate::{
    error::{Error, Result},
    store::Addr,
    value::Value,
    vm::Vm,
    Instance, Store,
};

/// An untyped WebAssembly function.
///
/// Using [`Self::call`], you can call the function with arguments whose types are not known at
/// compile time. For a well-typed version of this function, see [`WasmFunc`].
#[derive(Debug)]
pub struct WasmFuncUntyped {
    func_addr: Addr,
    inst: Instance,
}

impl WasmFuncUntyped {
    pub(crate) fn new(func_addr: Addr, inst: Instance) -> Self {
        Self { func_addr, inst }
    }

    /// Call the function with the given arguments, returning the results.
    ///
    /// Note that this function will perform type validation, and will return
    /// [`Error::FunctionArgsMismatch`] given a mismatch.
    pub fn call(&self, store: &mut Store, args: &[Value]) -> Result<Vec<Value>> {
        let f = self.inst.func_addrs()[self.func_addr];
        let func = &store.data.functions[f];
        if func.ty().0.len() != args.len()
            || !func
                .ty()
                .0
                .iter()
                .zip(args.iter().map(|v| v.ty()))
                .all(|(a, b)| a == &b)
        {
            return Err(Error::FunctionArgsMismatch {
                want: func.ty().0.clone(),
                got: args.iter().map(|v| v.ty()).collect(),
            });
        }
        let mut vm = Vm::new(&store.data, &mut store.mut_, self.inst.clone());
        let res = vm.call(self.func_addr, args).map_err(Error::Trap)?;
        Ok(res)
    }
}

/// A typed WebAssembly function.
///
/// For the untyped counterpart of this type, see [`WasmFuncUntyped`].
#[derive(Debug)]
pub struct WasmFunc<Params, Results> {
    func_addr: Addr,
    inst: Instance,
    _phantom: PhantomData<(Params, Results)>,
}

impl<Params, Results> WasmFunc<Params, Results>
where
    Params: WasmParams,
    Results: WasmResults,
{
    pub(crate) fn new(func_addr: Addr, inst: Instance) -> Self {
        Self {
            func_addr,
            inst,
            _phantom: PhantomData,
        }
    }

    /// Call the function with the given arguments, returning the results.
    pub fn call(&self, store: &mut Store, args: Params) -> Result<Results> {
        let mut vm = Vm::new(&store.data, &mut store.mut_, self.inst.clone());
        let res = vm
            .call(self.func_addr, &args.as_values())
            .map_err(Error::Trap)?;
        let values = Results::from_values(&res);
        Ok(values)
    }
}

pub trait WasmType {
    fn as_value(&self) -> Value;
    fn from_value(value: Value) -> Self;
    fn ty() -> ValType;
}

impl WasmType for i32 {
    fn as_value(&self) -> Value {
        Value::I32(*self as u32)
    }

    fn from_value(value: Value) -> Self {
        value.as_u32() as Self
    }

    fn ty() -> ValType {
        ValType::I32
    }
}

impl WasmType for i64 {
    fn as_value(&self) -> Value {
        Value::I64(*self as u64)
    }

    fn from_value(value: Value) -> Self {
        value.as_u64() as Self
    }

    fn ty() -> ValType {
        ValType::I64
    }
}

impl WasmType for f32 {
    fn as_value(&self) -> Value {
        Value::F32(*self)
    }

    fn from_value(value: Value) -> Self {
        value.as_f32() as Self
    }

    fn ty() -> ValType {
        ValType::F32
    }
}

impl WasmType for f64 {
    fn as_value(&self) -> Value {
        Value::F64(*self)
    }

    fn from_value(value: Value) -> Self {
        value.as_f64() as Self
    }

    fn ty() -> ValType {
        ValType::F64
    }
}

impl WasmType for u32 {
    fn as_value(&self) -> Value {
        Value::I32(*self)
    }

    fn from_value(value: Value) -> Self {
        value.as_u32() as Self
    }

    fn ty() -> ValType {
        ValType::I32
    }
}

impl WasmType for u64 {
    fn as_value(&self) -> Value {
        Value::I64(*self)
    }

    fn from_value(value: Value) -> Self {
        value.as_u64() as Self
    }

    fn ty() -> ValType {
        ValType::I64
    }
}

pub trait WasmParams {
    fn as_values(&self) -> Vec<Value>;
    fn matches<I>(values: I) -> bool
    where
        I: ExactSizeIterator<Item = ValType>;
}

pub trait WasmResults {
    fn from_values(values: &[Value]) -> Self;
    fn matches<I>(values: I) -> bool
    where
        I: ExactSizeIterator<Item = ValType>;
}

impl WasmResults for () {
    fn from_values(_values: &[Value]) -> Self {}

    fn matches<I>(values: I) -> bool
    where
        I: ExactSizeIterator<Item = ValType>,
    {
        values.len() == 0
    }
}

impl<T: WasmType> WasmResults for T {
    fn from_values(values: &[Value]) -> Self {
        T::from_value(values[0])
    }

    fn matches<I>(mut values: I) -> bool
    where
        I: ExactSizeIterator<Item = ValType>,
    {
        values.len() == 1 && values.next().unwrap() == T::ty()
    }
}

impl WasmParams for () {
    fn as_values(&self) -> Vec<Value> {
        vec![]
    }

    fn matches<I>(values: I) -> bool
    where
        I: ExactSizeIterator<Item = ValType>,
    {
        values.len() == 0
    }
}

impl<T: WasmType> WasmParams for T {
    fn as_values(&self) -> Vec<Value> {
        vec![self.as_value()]
    }

    fn matches<I>(mut values: I) -> bool
    where
        I: ExactSizeIterator<Item = ValType>,
    {
        values.len() == 1 && values.next().unwrap() == T::ty()
    }
}

/// Macro to help implement generic trait implementations for tuple types.
macro_rules! for_each_tuple {
    ($mac:ident) => {
        $mac!(1 0);
        $mac!(2 0 1);
        $mac!(3 0 1 2);
        $mac!(4 0 1 2 3);
        $mac!(5 0 1 2 3 4);
        $mac!(6 0 1 2 3 4 5);
        $mac!(7 0 1 2 3 4 5 6);
        $mac!(8 0 1 2 3 4 5 6 7);
        $mac!(9 0 1 2 3 4 5 6 7 8);
        $mac!(10 0 1 2 3 4 5 6 7 8 9);
        $mac!(11 0 1 2 3 4 5 6 7 8 9 10);
        $mac!(12 0 1 2 3 4 5 6 7 8 9 10 11);
        $mac!(13 0 1 2 3 4 5 6 7 8 9 10 11 12);
        $mac!(14 0 1 2 3 4 5 6 7 8 9 10 11 12 13);
        $mac!(15 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14);
        $mac!(16 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15);
    }
}

macro_rules! impl_wasm_params {
    ($n:tt $($T:tt)*) => {paste::paste! {
        impl<$([<T $T>]: WasmType),*> WasmParams for ($([<T $T>],)*) {
            fn as_values(&self) -> Vec<Value> {
                vec![$(self.$T.as_value()),*]
            }

            fn matches<I>(values: I) -> bool
            where
                I: ExactSizeIterator<Item = ValType>,
            {
                values.len() == $n && {
                    values.into_iter().zip([$([<T $T>]::ty()),*].iter()).all(|(a, b)| a == *b)
                }
            }
        }
    }}
}

macro_rules! impl_wasm_types {
    ($n:tt $($T:tt)*) => {paste::paste! {
        impl<$([<T $T>]: WasmType),*> WasmResults for ($([<T $T>],)*) {
            fn from_values(values: &[Value]) -> Self {
                ($(<[<T $T>]>::from_value(values[$T])),*,)
            }

            fn matches<I>(values: I) -> bool
            where
                I: ExactSizeIterator<Item = ValType>,
            {
                values.len() == $n && {
                    values.into_iter().zip([$([<T $T>]::ty()),*].iter()).all(|(a, b)| a == *b)
                }
            }
        }
    }}
}

for_each_tuple!(impl_wasm_params);
for_each_tuple!(impl_wasm_types);
