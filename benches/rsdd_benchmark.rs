extern crate criterion;
extern crate rsdd;

use rsdd::{manager::rsbdd_manager::BddManager, repr::cnf::Cnf};
use rsdd::manager::sdd_manager::SddManager;

use criterion::*;

fn compile_bdd(str: String) -> () {
    let cnf = Cnf::from_file(str);
    let mut man = BddManager::new_default_order(cnf.num_vars());
    man.from_cnf(&cnf);
}

fn bench01_cnf_bdd() -> () {
    let cnf_str = String::from(include_str!("../cnf/bench-01.cnf"));
    compile_bdd(black_box(cnf_str));
}

fn bench02_cnf_bdd() -> () {
    let cnf_str = String::from(include_str!("../cnf/bench-02.cnf"));
    compile_bdd(black_box(cnf_str));
}

fn bench03_cnf_bdd() -> () {
    let cnf_str = String::from(include_str!("../cnf/bench-03.cnf"));
    compile_bdd(black_box(cnf_str));
}

// fn bench_c8_easier_cnf_bdd() -> () {
//     let cnf_str = String::from(include_str!("../cnf/c8-very-easy.cnf"));
//     compile_bdd(black_box(cnf_str));
// }

fn bench_bdd_full(c: &mut Criterion) {
    let mut group = c.benchmark_group("sample-bdd-full");
    group.sampling_mode(SamplingMode::Flat);
    group.bench_function("bench-01", |b| b.iter(|| bench01_cnf_bdd()));
    group.bench_function("bench-02", |b| b.iter(|| bench02_cnf_bdd()));
    group.bench_function("bench-03", |b| b.iter(|| bench03_cnf_bdd()));
    // group.bench_function("bench-c8-very-easy", |b| b.iter(|| bench_c8_easier_cnf_bdd()));
    group.finish();
}

fn bench01_cnf_from_file() -> () {
    let cnf_str = String::from(include_str!("../cnf/bench-01.cnf"));
    Cnf::from_file(black_box(cnf_str));
}

fn bench02_cnf_from_file() -> () {
    let cnf_str = String::from(include_str!("../cnf/bench-02.cnf"));
    Cnf::from_file(black_box(cnf_str));
}

fn bench03_cnf_from_file() -> () {
    let cnf_str = String::from(include_str!("../cnf/bench-03.cnf"));
    Cnf::from_file(black_box(cnf_str));
}

fn c8easier_cnf_from_file() -> () {
    let cnf_str = String::from(include_str!("../cnf/c8-easier.cnf"));
    Cnf::from_file(black_box(cnf_str));
}

fn c8veryeasy_cnf_from_file() -> () {
    let cnf_str = String::from(include_str!("../cnf/c8-very-easy.cnf"));
    Cnf::from_file(black_box(cnf_str));
}

fn c8_cnf_from_file() -> () {
    let cnf_str = String::from(include_str!("../cnf/c8.cnf"));
    Cnf::from_file(black_box(cnf_str));
}

fn count_cnf_from_file() -> () {
    let cnf_str = String::from(include_str!("../cnf/count.cnf"));
    Cnf::from_file(black_box(cnf_str));
}

fn s298_cnf_from_file() -> () {
    let cnf_str = String::from(include_str!("../cnf/s298.cnf"));
    Cnf::from_file(black_box(cnf_str));
}

fn s344_cnf_from_file() -> () {
    let cnf_str = String::from(include_str!("../cnf/s344.cnf"));
    Cnf::from_file(black_box(cnf_str));
}

fn s444_cnf_from_file() -> () {
    let cnf_str = String::from(include_str!("../cnf/s444.cnf"));
    Cnf::from_file(black_box(cnf_str));
}

fn s510_cnf_from_file() -> () {
    let cnf_str = String::from(include_str!("../cnf/s510.cnf"));
    Cnf::from_file(black_box(cnf_str));
}

fn s641_cnf_from_file() -> () {
    let cnf_str = String::from(include_str!("../cnf/s641.cnf"));
    Cnf::from_file(black_box(cnf_str));
}

fn unsat1_cnf_from_file() -> () {
    let cnf_str = String::from(include_str!("../cnf/unsat-1.cnf"));
    Cnf::from_file(black_box(cnf_str));
}

fn bench_cnf_from_file(c: &mut Criterion) {
    let mut group = c.benchmark_group("sample-cnf-from-file");
    group.sampling_mode(SamplingMode::Flat);
    group.bench_function("bench-01", |b| b.iter(|| bench01_cnf_from_file()));
    group.bench_function("bench-02", |b| b.iter(|| bench02_cnf_from_file()));
    group.bench_function("bench-03", |b| b.iter(|| bench03_cnf_from_file()));
    group.bench_function("c8-easier", |b| b.iter(|| c8easier_cnf_from_file()));
    group.bench_function("c8-very-easy", |b| b.iter(|| c8veryeasy_cnf_from_file()));
    group.bench_function("c8", |b| b.iter(|| c8_cnf_from_file()));
    group.bench_function("count", |b| b.iter(|| count_cnf_from_file()));
    group.bench_function("s298", |b| b.iter(|| s298_cnf_from_file()));
    group.bench_function("s344", |b| b.iter(|| s344_cnf_from_file()));
    group.bench_function("s444", |b| b.iter(|| s444_cnf_from_file()));
    group.bench_function("s510", |b| b.iter(|| s510_cnf_from_file()));
    group.bench_function("s641", |b| b.iter(|| s641_cnf_from_file()));
    group.bench_function("unsat-1", |b| b.iter(|| unsat1_cnf_from_file()));
    group.finish();
}


criterion_group!(benches, bench_bdd_full, bench_cnf_from_file);
criterion_main!(benches);
