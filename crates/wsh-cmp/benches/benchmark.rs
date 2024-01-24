use std::{fs, path::Path};

use criterion::{criterion_group, criterion_main, Criterion};
use wsh_cmp::trie::Trie;

fn run_benchmarks(c: &mut Criterion) {
    let targets = [
        Path::new("./benches/inputs/sowpods.txt"),
        Path::new("./benches/inputs/1984.txt"),
    ];
    for target in targets {
        let name = &target.file_stem().unwrap().to_string_lossy();
        c.bench_function(name, |b| {
            let mut trie = Trie::new();
            let contents = fs::read_to_string(target).unwrap();
            // We allocate this here for fast looping during the benchmark
            let split = contents.split_whitespace().collect::<Vec<&str>>();
            for word in &split {
                trie.insert(word);
            }
            b.iter(|| {
                for word in &split {
                    std::hint::black_box(trie.prefix_list(word));
                }
            })
        });
    }
}

criterion_group!(benches, run_benchmarks);
criterion_main!(benches);
