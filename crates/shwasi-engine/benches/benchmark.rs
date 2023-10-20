use std::{
    fs,
    path::{Path, PathBuf},
};

use criterion::{criterion_group, criterion_main, Criterion};
use shwasi_engine::module::Parser;

#[derive(Debug)]
struct BenchTarget {
    path: PathBuf,
    wasm: Vec<u8>,
}

fn get_bench_inputs(path: impl AsRef<Path>, targets: &mut Vec<BenchTarget>) {
    for entry in path.as_ref().read_dir().expect("path should be valid dir") {
        let path = entry.expect("entry should be valid").path();
        if path.is_dir() {
            get_bench_inputs(&path, targets);
        }

        if path.extension().is_some_and(|ext| ext == "wasm") {
            let wasm = fs::read(&path).expect("should be able to read wasm");
            targets.push(BenchTarget { path, wasm });
        }
    }
}

fn run_benchmarks(c: &mut Criterion) {
    let mut targets = vec![];
    get_bench_inputs("./benches/inputs", &mut targets);

    for BenchTarget { path, wasm } in targets {
        let name = path.file_stem().unwrap().to_str().unwrap();
        c.bench_function(name, |b| {
            b.iter(|| {
                let parser = Parser::new(&wasm);
                criterion::black_box(parser.read_module().expect("should be able to read module"));
            })
        });
    }
}

criterion_group!(benchmark, run_benchmarks);
criterion_main!(benchmark);
