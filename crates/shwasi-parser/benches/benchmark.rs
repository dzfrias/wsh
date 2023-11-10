use std::{
    fs,
    path::{Path, PathBuf},
};

use criterion::{criterion_group, criterion_main, Criterion};
use shwasi_parser::{validate, Parser};

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

        if path
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .starts_with("simd")
        {
            continue;
        }

        match path.extension().and_then(|ext| ext.to_str()) {
            Some("wasm") => {
                let wasm = fs::read(&path).expect("should be able to read wasm");
                targets.push(BenchTarget { path, wasm });
            }
            _ => continue,
        }
    }
}

fn run_benchmarks(c: &mut Criterion) {
    let mut targets = vec![];
    get_bench_inputs("./benches/inputs", &mut targets);
    get_bench_inputs("./tests/spectests/wasm", &mut targets);

    for BenchTarget { path, wasm } in &targets {
        let name = path.file_stem().unwrap().to_str().unwrap();
        c.bench_function(&format!("parser:{name}"), |b| {
            b.iter(|| {
                let parser = Parser::new(wasm);
                parser.read_module().expect("should be able to read module");
            });
        });
    }
    println!("finished benchmarks for parser");
    for BenchTarget { path, wasm } in targets {
        let name = path.file_stem().unwrap().to_str().unwrap();
        c.bench_function(&format!("validator:{name}"), |b| {
            let module = Parser::new(&wasm)
                .read_module()
                .expect("should be able to read moudle");
            b.iter(|| {
                validate(&module).expect("module should be valid");
            });
        });
    }
}

criterion_group!(benchmark, run_benchmarks);
criterion_main!(benchmark);
