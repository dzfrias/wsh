use std::marker::PhantomData;

use wsh_parser::ValType;

use crate::{
    error::{ErrorKind, Result},
    store::Addr,
    value::{Value, ValueUntyped},
    vm::Vm,
    Func, Instance, Store,
};

/// An untyped WebAssembly function.
///
/// Using [`Self::call`], you can call the function with arguments whose types are not known at
/// compile time. For a well-typed version of this function, see [`WasmFunc`].
#[derive(Debug)]
pub struct WasmFuncUntyped {
    func_addr: Addr<Func>,
    inst: Instance,
}

impl WasmFuncUntyped {
    pub(crate) fn new(func_addr: Addr<Func>, inst: Instance) -> Self {
        Self { func_addr, inst }
    }

    /// Call the function with the given arguments, returning the results.
    ///
    /// Note that this function will perform type validation, and will return
    /// [`Error::FunctionArgsMismatch`] given a mismatch.
    pub fn call(&self, store: &mut Store, args: &[Value]) -> Result<Vec<Value>> {
        let func = &store.functions[self.func_addr];
        if func.ty().0.len() != args.len()
            || !func
                .ty()
                .0
                .iter()
                .zip(args.iter().map(|v| v.ty()))
                .all(|(a, b)| a == &b)
        {
            return Err(ErrorKind::FunctionArgsMismatch {
                want: func.ty().0.to_vec(),
                got: args.iter().map(|v| v.ty()).collect(),
            }
            .into());
        }
        let mut vm = Vm::new(store, self.inst.clone());
        let res = vm.call(self.func_addr, args.iter().map(|v| (*v).into()))?;
        Ok(res)
    }

    pub fn arg_types<'a>(&self, store: &'a Store) -> &'a [ValType] {
        &store.functions[self.func_addr].ty().0
    }
}

/// A typed WebAssembly function.
///
/// For the untyped counterpart of this type, see [`WasmFuncUntyped`].
#[derive(Debug)]
pub struct WasmFunc<Params, Results> {
    func_addr: Addr<Func>,
    inst: Instance,
    _phantom: PhantomData<(Params, Results)>,
}

impl<Params, Results> WasmFunc<Params, Results>
where
    Params: WasmParams,
    Results: WasmResults,
{
    pub(crate) fn new(func_addr: Addr<Func>, inst: Instance) -> Self {
        Self {
            func_addr,
            inst,
            _phantom: PhantomData,
        }
    }

    /// Call the function with the given arguments, returning the results.
    pub fn call(&self, store: &mut Store, args: Params) -> Result<Results> {
        let mut vm = Vm::new(store, self.inst.clone());
        let res = vm.call(self.func_addr, args.as_values())?;
        let values = Results::from_values(res.into_iter().map(Into::into));
        Ok(values)
    }
}

/// An opaque trait that represents values that can be passed to a WebAssembly function.
pub trait WasmType {
    #[doc(hidden)]
    fn as_value(&self) -> ValueUntyped;
    #[doc(hidden)]
    fn from_value(value: ValueUntyped) -> Self;
    #[doc(hidden)]
    fn ty() -> ValType;
}

/// An opaque trait that represents values that can be passed into a WebAssembly function.
pub trait WasmParams {
    #[doc(hidden)]
    fn as_values(&self) -> impl DoubleEndedIterator<Item = ValueUntyped>;
    #[doc(hidden)]
    fn matches<I>(values: I) -> bool
    where
        I: IntoIterator<Item = ValType>;
    #[doc(hidden)]
    fn valtypes() -> impl Iterator<Item = ValType>;
}

/// An opaque trait that represents values that can be returned from a WebAssembly function.
pub trait WasmResults {
    #[doc(hidden)]
    fn from_values<I>(values: I) -> Self
    where
        I: IntoIterator<Item = ValueUntyped>;
    #[doc(hidden)]
    fn matches<I>(values: I) -> bool
    where
        I: IntoIterator<Item = ValType>;
    #[doc(hidden)]
    fn into_values(self) -> Result<Vec<ValueUntyped>>;
    #[doc(hidden)]
    fn valtypes() -> impl Iterator<Item = ValType>;
}

impl WasmType for i32 {
    fn as_value(&self) -> ValueUntyped {
        (*self).into()
    }

    fn from_value(value: ValueUntyped) -> Self {
        value.as_u32() as Self
    }

    fn ty() -> ValType {
        ValType::I32
    }
}

impl WasmType for i64 {
    fn as_value(&self) -> ValueUntyped {
        (*self).into()
    }

    fn from_value(value: ValueUntyped) -> Self {
        value.as_u64() as Self
    }

    fn ty() -> ValType {
        ValType::I64
    }
}

impl WasmType for f32 {
    fn as_value(&self) -> ValueUntyped {
        (*self).into()
    }

    fn from_value(value: ValueUntyped) -> Self {
        value.as_f32() as Self
    }

    fn ty() -> ValType {
        ValType::F32
    }
}

impl WasmType for f64 {
    fn as_value(&self) -> ValueUntyped {
        (*self).into()
    }

    fn from_value(value: ValueUntyped) -> Self {
        value.as_f64() as Self
    }

    fn ty() -> ValType {
        ValType::F64
    }
}

impl WasmType for u32 {
    fn as_value(&self) -> ValueUntyped {
        (*self).into()
    }

    fn from_value(value: ValueUntyped) -> Self {
        value.as_u32() as Self
    }

    fn ty() -> ValType {
        ValType::I32
    }
}

impl WasmType for u64 {
    fn as_value(&self) -> ValueUntyped {
        (*self).into()
    }

    fn from_value(value: ValueUntyped) -> Self {
        value.as_u64() as Self
    }

    fn ty() -> ValType {
        ValType::I64
    }
}

impl WasmResults for () {
    fn from_values<I>(_values: I) -> Self
    where
        I: IntoIterator<Item = ValueUntyped>,
    {
    }

    fn matches<I>(values: I) -> bool
    where
        I: IntoIterator<Item = ValType>,
    {
        values.into_iter().next().is_none()
    }

    fn into_values(self) -> Result<Vec<ValueUntyped>> {
        Ok(vec![])
    }

    fn valtypes() -> impl Iterator<Item = ValType> {
        None.into_iter()
    }
}

impl<T: WasmType> WasmResults for T {
    fn from_values<I>(values: I) -> Self
    where
        I: IntoIterator<Item = ValueUntyped>,
    {
        T::from_value(values.into_iter().next().unwrap())
    }

    fn matches<I>(values: I) -> bool
    where
        I: IntoIterator<Item = ValType>,
    {
        let mut iter = values.into_iter();
        iter.next().is_some_and(|v| v == T::ty()) && iter.next().is_none()
    }

    fn into_values(self) -> Result<Vec<ValueUntyped>> {
        Ok(vec![self.as_value()])
    }

    fn valtypes() -> impl Iterator<Item = ValType> {
        std::iter::once(T::ty())
    }
}

impl WasmParams for () {
    fn as_values(&self) -> impl DoubleEndedIterator<Item = ValueUntyped> {
        None.into_iter()
    }

    fn matches<I>(values: I) -> bool
    where
        I: IntoIterator<Item = ValType>,
    {
        values.into_iter().next().is_none()
    }

    fn valtypes() -> impl Iterator<Item = ValType> {
        None.into_iter()
    }
}

impl<T: WasmType> WasmParams for T {
    fn as_values(&self) -> impl DoubleEndedIterator<Item = ValueUntyped> {
        std::iter::once(self.as_value())
    }

    fn matches<I>(values: I) -> bool
    where
        I: IntoIterator<Item = ValType>,
    {
        let mut iter = values.into_iter();
        iter.next().is_some_and(|v| v == T::ty()) && iter.next().is_none()
    }

    fn valtypes() -> impl Iterator<Item = ValType> {
        std::iter::once(T::ty())
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
            fn as_values(&self) -> impl DoubleEndedIterator<Item = ValueUntyped> {
                [$(self.$T.as_value()),*].into_iter()
            }

            fn matches<I>(values: I) -> bool
            where
                I: IntoIterator<Item = ValType>,
            {
                let mut iter = values.into_iter();
                $(
                    if !iter.next().is_some_and(|v| v == [<T $T>]::ty()) {
                        return false;
                    }
                 )*
                iter.next().is_none()
            }

            fn valtypes() -> impl Iterator<Item = ValType> {
                [$([<T $T>]::ty()),*].into_iter()
            }
        }
    }}
}

macro_rules! impl_wasm_results {
    ($n:tt $($T:tt)*) => {paste::paste! {
        impl<$([<T $T>]: WasmType),*> WasmResults for ($([<T $T>],)*) {
            fn from_values<I>(values: I) -> Self
            where
                I: IntoIterator<Item = ValueUntyped>,
            {
                let mut iter = values.into_iter();
                ($(<[<T $T>]>::from_value(iter.next().unwrap())),*,)
            }

            fn matches<I>(values: I) -> bool
            where
                I: IntoIterator<Item = ValType>,
            {
                let mut iter = values.into_iter();
                $(
                    if !iter.next().is_some_and(|v| v == [<T $T>]::ty()) {
                        return false;
                    }
                 )*
                iter.next().is_none()
            }

            fn into_values(self) -> Result<Vec<ValueUntyped>> {
                Ok(vec![$(self.$T.as_value()),*])
            }

            fn valtypes() -> impl Iterator<Item = ValType> {
                [$([<T $T>]::ty()),*].into_iter()
            }
        }
    }}
}

for_each_tuple!(impl_wasm_params);
for_each_tuple!(impl_wasm_results);

impl<T> WasmResults for Result<T>
where
    T: WasmResults,
{
    fn from_values<I>(values: I) -> Self
    where
        I: IntoIterator<Item = ValueUntyped>,
    {
        Ok(T::from_values(values))
    }

    fn matches<I>(values: I) -> bool
    where
        I: IntoIterator<Item = ValType>,
    {
        T::matches(values)
    }

    fn into_values(self) -> Result<Vec<ValueUntyped>> {
        self.map_or_else(Err, T::into_values)
    }

    fn valtypes() -> impl Iterator<Item = ValType> {
        T::valtypes()
    }
}
