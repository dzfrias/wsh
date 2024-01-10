use criterion::{criterion_group, criterion_main, Criterion};
use shwasi_cmp::trie::Trie;

fn run_benchmarks(c: &mut Criterion) {
    c.bench_function("sowpods", |b| {
        let mut trie = Trie::new();
        for line in include_str!("./inputs/sowpods.txt").lines() {
            if line.ends_with("\n") {
                panic!()
            }
            trie.insert(line);
        }
        b.iter(|| {
            std::hint::black_box(trie.prefix_list("e"));
        })
    });
}

criterion_group!(benches, run_benchmarks);
criterion_main!(benches);
