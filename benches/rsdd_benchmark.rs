extern crate criterion;
extern crate rsdd;

use rsdd::{builder::bdd_builder::BddManager, repr::cnf::Cnf};

use criterion::*;

fn compile_bdd(str: String) {
    let cnf = Cnf::from_file(str);
    let mut man = BddManager::new_default_order(cnf.num_vars());
    man.from_cnf(&cnf);
}

fn bench01_cnf_bdd() {
    let cnf_str = String::from(include_str!("../cnf/bench-01.cnf"));
    compile_bdd(black_box(cnf_str));
}

fn bench02_cnf_bdd() {
    let cnf_str = String::from(include_str!("../cnf/bench-02.cnf"));
    compile_bdd(black_box(cnf_str));
}

fn bench03_cnf_bdd() {
    let cnf_str = String::from(include_str!("../cnf/bench-03.cnf"));
    compile_bdd(black_box(cnf_str));
}

fn bench_c8_easier_cnf_bdd() {
    let cnf_str = String::from(include_str!("../cnf/c8-very-easy.cnf"));
    compile_bdd(black_box(cnf_str));
}

fn bench_bdd_full(c: &mut Criterion) {
    let mut group = c.benchmark_group("sample-bdd-full");
    group.sampling_mode(SamplingMode::Flat);
    group.bench_function("bench-01", |b| b.iter(bench01_cnf_bdd));
    group.bench_function("bench-02", |b| b.iter(bench02_cnf_bdd));
    group.bench_function("bench-03", |b| b.iter(bench03_cnf_bdd));
    group.bench_function("bench-c8-very-easy", |b| b.iter(bench_c8_easier_cnf_bdd));
    group.finish();
}

criterion_group!(benches, bench_bdd_full);
criterion_main!(benches);
