use std::sync::Arc;

use shwasi_parser::FuncType;

use crate::{store::HostFunc, Instance, Store, WasmParams, WasmResults, WasmType};

mod private {
    pub trait Sealed {}
}

pub trait IntoHostFunc<Params, Results> {
    #[doc(hidden)]
    type Params: WasmParams;
    #[doc(hidden)]
    type Results: WasmResults;

    #[doc(hidden)]
    fn into_host_func(self) -> HostFunc;
}

impl<F, R> IntoHostFunc<(), R> for F
where
    F: FnMut(Instance, &mut Store) -> R + 'static,
    R: WasmResults,
{
    type Params = ();
    type Results = R;

    fn into_host_func(mut self) -> HostFunc {
        HostFunc {
            ty: FuncType(Arc::new([]), Arc::new([])),
            code: Box::new(move |vm| {
                let res = self(vm.get_module(), vm.get_store());
                res.into_values()
            }),
        }
    }
}

// Macro to help implement generic trait implementations for tuple types.
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

macro_rules! impl_host_func {
    ($n:tt $($T:tt)*) => {
        paste::paste! {
            impl<F, $([<T $T>]),*, R> IntoHostFunc<($([<T $T>]),* ,), R> for F
            where
                F: FnMut(Instance, &mut Store, $([<T $T>]),*) -> R + 'static,
                R: WasmResults,
                ($([<T $T>],)*): WasmParams,
                $(
                    [<T $T>]: WasmType,
                 )*
            {
                type Params = ($([<T $T>]),* ,);
                type Results = R;

                fn into_host_func(mut self) -> HostFunc {
                    HostFunc {
                        ty: FuncType(<Self::Params as WasmParams>::valtypes().collect(), R::valtypes().collect()),
                        code: Box::new(move |vm| {
                            let mut args = [$crate::Value::I32(0).untyped(); $n];
                            for i in 0..$n {
                                args[i] = vm.stack.pop().unwrap();
                            }
                            $(
                                let [<t $T>] = [<T $T>]::from_value(args[$n - $T - 1]);
                             )*
                            let res = self(vm.get_module(), vm.get_store(), $([<t $T>]),*);
                            res.into_values()
                        }),
                    }
                }
            }
        }
    };
}

for_each_tuple!(impl_host_func);
