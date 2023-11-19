use criterion::{criterion_group, criterion_main, Criterion};
use shwasi_engine::{Instance, Store};
use shwasi_parser::Parser;

macro_rules! bench {
    ($c:expr, $($name:ident<$inputs:ty, $results:ty>($($arg:expr),*)),* $(,)?) => {
        $(
            $c.bench_function(stringify!($name), |b| {
                let parser = Parser::new(include_bytes!(concat!(
                    "inputs/",
                    stringify!($name),
                    ".wasm"
                )));
                let m = parser.read_module().expect("should be able to read module");
                let mut store = Store::default();
                let instance =
                    Instance::instantiate(&mut store, m).expect("should be able to instantiate module");
                let $name = instance
                    .get_func::<$inputs, $results>(&store, stringify!($name))
                    .expect("should be able to get fib function from instance");
                b.iter(|| {
                    $name.call(&mut store, ($($arg),*)).expect("unexpected trap");
                });
            });
         )*
    };
}

fn run_benchmarks(c: &mut Criterion) {
    #[rustfmt::skip]
    bench!(c, 
       fib<u32, u32>(25),
    );
}

criterion_group!(benchmark, run_benchmarks);
criterion_main!(benchmark);
