extern crate criterion;
extern crate rsdd;

use criterion::black_box;
use rsdd::{builder::{bdd_builder::BddManager, var_order::VarOrder}, repr::cnf::Cnf};
use std::time::{Duration, Instant};

fn compile_bdd(str: String) -> () {
    let cnf = Cnf::from_file(str);
    let mut man = rsdd::builder::decision_nnf_builder::DecisionNNFBuilder::new(cnf.num_vars());
    let ddnnf = man.from_cnf_topdown(&VarOrder::linear_order(cnf.num_vars()), &cnf);
    println!("# nodes: {}", man.count_nodes(ddnnf));

    // let mut man = BddManager::new_default_order(cnf.num_vars());
    // let b2 = man.from_cnf(&cnf);
    // let b1 = man.from_cnf_topdown(&cnf);
    // assert_eq!(b1, b2);
    // println!("# recursive calls: {}, size: {}", man.num_recursive_calls(), man.count_nodes(b1));
}

fn bench_cnf_bdd(cnf_str: String) -> Duration {
    let start = Instant::now();
    compile_bdd(black_box(cnf_str));
    start.elapsed()
}

fn main() {
    let cnf_strs = vec![
        ("grid-50-10-1-q", String::from(include_str!("../cnf/50-10-1-q.cnf"))),
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
        ("s298", String::from(include_str!("../cnf/s298.cnf"))),
        ("s344", String::from(include_str!("../cnf/s344.cnf"))),
        ("grid-75-16-2-q", String::from(include_str!("../cnf/75-16-2-q.cnf"))),
        ("c8-easier", String::from(include_str!("../cnf/c8-easier.cnf"))),
        ("s444", String::from(include_str!("../cnf/s444.cnf"))),
        ("s510", String::from(include_str!("../cnf/s510.cnf"))),
        ("s641", String::from(include_str!("../cnf/s641.cnf"))),
        ("count", String::from(include_str!("../cnf/count.cnf"))),
        (
            "c8-very-easy",
            String::from(include_str!("../cnf/c8-very-easy.cnf")),
        ),
        ("log1", String::from(include_str!("../cnf/log1.cnf"))),
        // ("c8", String::from(include_str!("../cnf/c8.cnf"))),
    ];

    for (cnf_name, cnf_str) in cnf_strs {
        let d = bench_cnf_bdd(cnf_str);
        let c8_time = d.as_secs_f64();
        println!("one-shot compile_bdd time for {}: {}s", cnf_name, c8_time);
    }
}
