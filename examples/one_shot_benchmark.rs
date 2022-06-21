extern crate criterion;
extern crate rayon;
extern crate rsdd;

use clap::Parser;
use criterion::black_box;
use rayon::prelude::*;
use rsdd::{builder::bdd_builder::BddManager, repr::cnf::Cnf};
use std::time::{Duration, Instant};

/// Test driver for one-shot benchmark
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Number of threads to subdivide benchmarks into;
    /// if this is larger than the number of logical threads,
    /// the benchmark will likely be degraded
    #[clap(short, long, value_parser, default_value_t = 1)]
    threads: usize,
}

fn compile_bdd(str: String) -> () {
    let cnf = Cnf::from_file(str);
    let mut man = BddManager::new_default_order(cnf.num_vars());
    man.from_cnf(&cnf);
    println!("# recursive calls: {}", man.num_recursive_calls());
}

fn bench_cnf_bdd(cnf_str: String) -> Duration {
    let start = Instant::now();
    compile_bdd(black_box(cnf_str));
    start.elapsed()
}

fn main() {
    let args = Args::parse();

    rayon::ThreadPoolBuilder::new().num_threads(args.threads).build_global().unwrap();

    let cnf_strs = vec![
        (
            "bench-01",
            String::from(include_str!("../cnf/bench-01.cnf")),
        ),
        (
            "bench-02",
            String::from(include_str!("../cnf/bench-02.cnf")),
        ),
        (
            "bench-03",
            String::from(include_str!("../cnf/bench-03.cnf")),
        ),
        // ("c8-easier", String::from(include_str!("../cnf/c8-easier.cnf"))),
        ("php-4-6", String::from(include_str!("../cnf/php-4-6.cnf"))),
        ("php-5-4", String::from(include_str!("../cnf/php-5-4.cnf"))),
        // ("php-12-14", String::from(include_str!("../cnf/php-12-14.cnf"))),
        (
            "rand-3-25-75-1",
            String::from(include_str!("../cnf/rand-3-25-75-1.cnf")),
        ),
        (
            "rand-3-25-100-1",
            String::from(include_str!("../cnf/rand-3-25-100-1.cnf")),
        ),
        (
            "rand-3-25-100-2",
            String::from(include_str!("../cnf/rand-3-25-100-2.cnf")),
        ),
        (
            "rand-3-25-100-3",
            String::from(include_str!("../cnf/rand-3-25-100-3.cnf")),
        ),
        (
            "rand-3-37-75-1",
            String::from(include_str!("../cnf/rand-3-37-75-1.cnf")),
        ),
        (
            "rand-3-37-75-2",
            String::from(include_str!("../cnf/rand-3-37-75-2.cnf")),
        ),
        (
            "rand-3-37-75-3",
            String::from(include_str!("../cnf/rand-3-37-75-3.cnf")),
        ),
        (
            "rand-3-37-75-4",
            String::from(include_str!("../cnf/rand-3-37-75-4.cnf")),
        ),
        (
            "rand-3-37-75-5",
            String::from(include_str!("../cnf/rand-3-37-75-5.cnf")),
        ),
        (
            "rand-3-37-75-6",
            String::from(include_str!("../cnf/rand-3-37-75-6.cnf")),
        ),
        (
            "rand-3-37-75-7",
            String::from(include_str!("../cnf/rand-3-37-75-7.cnf")),
        ),
        (
            "rand-3-37-75-8",
            String::from(include_str!("../cnf/rand-3-37-75-8.cnf")),
        ),
        (
            "rand-3-50-200-1",
            String::from(include_str!("../cnf/rand-3-50-200-1.cnf")),
        ),
        (
            "rand-3-50-200-2",
            String::from(include_str!("../cnf/rand-3-50-200-2.cnf")),
        ),
        (
            "rand-3-50-200-3",
            String::from(include_str!("../cnf/rand-3-50-200-3.cnf")),
        ),
        (
            "rand-3-100-400-1",
            String::from(include_str!("../cnf/rand-3-100-400-1.cnf")),
        ),
        (
            "c8-very-easy",
            String::from(include_str!("../cnf/c8-very-easy.cnf")),
        ),
        // ("c8", String::from(include_str!("../cnf/c8.cnf"))),
        // ("count", String::from(include_str!("../cnf/count.cnf"))),
        ("s298", String::from(include_str!("../cnf/s298.cnf"))),
        // ("s344", String::from(include_str!("../cnf/s344.cnf"))),
        // ("s444", String::from(include_str!("../cnf/s444.cnf"))),
        // ("s510", String::from(include_str!("../cnf/s510.cnf"))),
        // ("s641", String::from(include_str!("../cnf/s641.cnf"))),
        // ("unsat-1", String::from(include_str!("../cnf/unsat-1.cnf"))),
    ];

    println!("Benchmarking {} CNFs with {} thread{}", cnf_strs.len(), args.threads, if args.threads > 1 {"s"} else {""});

    let cnf_results: Vec<(&str, f64)> = cnf_strs.into_par_iter().map(|(cnf_name, cnf_str)| {
        let d = bench_cnf_bdd(cnf_str);
        (cnf_name, d.as_secs_f64())
    }).collect();

    for (cnf_name, cnf_time) in cnf_results {
        println!("one-shot compile_bdd time for {}: {}s", cnf_name, cnf_time);
    }
}
