use shwasi_parser::FuncType;

use crate::{store::HostFunc, WasmParams, WasmResults, WasmType};

mod private {
    pub trait Sealed {}
}

pub trait IntoHostFunc<Params, Results>: Send + Sync + 'static {
    #[doc(hidden)]
    type Params: WasmParams;
    #[doc(hidden)]
    type Results: WasmResults;

    #[doc(hidden)]
    fn into_host_func(self) -> HostFunc;
}

impl<F, R> IntoHostFunc<(), R> for F
where
    F: Fn() -> R + Send + Sync + 'static,
    R: WasmResults,
{
    type Params = ();
    type Results = R;

    fn into_host_func(self) -> HostFunc {
        HostFunc {
            ty: FuncType(vec![], vec![]),
            code: Box::new(move |_| {
                let res = self();
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
                F: Fn($([<T $T>]),*) -> R + Send + Sync + 'static,
                R: WasmResults,
                ($([<T $T>],)*): WasmParams,
                $(
                    [<T $T>]: WasmType,
                 )*
            {
                type Params = ($([<T $T>]),* ,);
                type Results = R;

                fn into_host_func(self) -> HostFunc {
                    HostFunc {
                        ty: FuncType(<Self::Params as WasmParams>::valtypes(), R::valtypes()),
                        code: Box::new(move |vm| {
                            $(
                                let [<t $T>] = [<T $T>]::from_value(vm.stack.pop().unwrap());
                             )*
                            let res = self($([<t $T>]),*);
                            res.into_values()
                        }),
                    }
                }
            }
        }
    };
}

for_each_tuple!(impl_host_func);
