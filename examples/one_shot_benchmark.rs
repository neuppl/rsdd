extern crate criterion;
extern crate rsdd;

use criterion::black_box;
use rsdd::{manager::rsbdd_manager::BddManager, repr::cnf::Cnf};
use std::time::{Instant, Duration};

fn compile_bdd(str: String) -> () {
  let cnf = Cnf::from_file(str);
  let mut man = BddManager::new_default_order(cnf.num_vars());
  man.from_cnf(&cnf);
}

fn bench_cnf_bdd(cnf_str: String) -> Duration {
  let start = Instant::now();
  compile_bdd(black_box(cnf_str));
  start.elapsed()
}

fn main() {
  let cnf_strs = vec![
    ("bench-01", String::from(include_str!("../cnf/bench-01.cnf"))),
    ("bench-02", String::from(include_str!("../cnf/bench-02.cnf"))),
    ("bench-03", String::from(include_str!("../cnf/bench-03.cnf"))),
    // ("c8-easier", String::from(include_str!("../cnf/c8-easier.cnf"))),
    ("c8-very-easy", String::from(include_str!("../cnf/c8-very-easy.cnf"))),
    // ("c8", String::from(include_str!("../cnf/c8.cnf"))),
    // ("count", String::from(include_str!("../cnf/count.cnf"))),
    ("s298", String::from(include_str!("../cnf/s298.cnf"))),
    // ("s344", String::from(include_str!("../cnf/s344.cnf"))),
    // ("s444", String::from(include_str!("../cnf/s444.cnf"))),
    // ("s510", String::from(include_str!("../cnf/s510.cnf"))),
    // ("s641", String::from(include_str!("../cnf/s641.cnf"))),
    // ("unsat-1", String::from(include_str!("../cnf/unsat-1.cnf"))),
  ];

  for (cnf_name, cnf_str) in cnf_strs {
    let d = bench_cnf_bdd(cnf_str);
    let c8_time = d.as_secs_f64();
    println!("one-shot compile_bdd time for {}: {}s", cnf_name, c8_time);
  }
}
