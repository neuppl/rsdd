extern crate criterion;
extern crate rayon;
extern crate rsdd;
extern crate serde_json;

use clap::Parser;
use criterion::black_box;
use rayon::prelude::*;
use rsdd::{builder::{bdd_builder::{BddManager, BddWmc}, var_order::VarOrder, repr::builder_sdd::VTree}, repr::{cnf::Cnf, var_label::VarLabel}};
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::{collections::HashMap, iter::FromIterator};
use std::fs;
use std::{time::{Duration, Instant}};

/// Test driver for one-shot benchmark
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Print debug messages to console
    #[clap(short, long, value_parser, default_value_t = false)]
    debug: bool,

    /// File to output JSON to, if any
    #[clap(short, long, value_parser, default_value = "")]
    output: String,

    /// Number of threads to subdivide benchmarks into;
    /// if this is larger than the number of logical threads,
    /// the benchmark will likely be degraded
    #[clap(short, long, value_parser, default_value_t = 1)]
    threads: usize,
}

#[derive(Serialize, Deserialize)]
struct BenchmarkLog {
    git_hash: String,
    threads: usize,
    benchmarks: HashMap<String, BenchmarkEntry>,
}

#[derive(Serialize, Deserialize)]
struct BenchmarkEntry {
    time_in_secs: f64,
}

fn compile_topdown_nnf(str: String, debug: bool) -> () {
    let cnf = Cnf::from_file(str);
    let mut man = rsdd::builder::decision_nnf_builder::DecisionNNFBuilder::new(cnf.num_vars());
    let ddnnf = man.from_cnf_topdown(&VarOrder::linear_order(cnf.num_vars()), &cnf);
}

fn compile_topdown_nnf_sample(str: String, debug: bool) -> () {
    let cnf = Cnf::from_file(str);
    let mut man = rsdd::builder::decision_nnf_builder::DecisionNNFBuilder::new(cnf.num_vars());
    let weight_map : HashMap<VarLabel, (f64, f64)> = HashMap::from_iter(
        (0..cnf.num_vars()).map(|x| (VarLabel::new(x as u64), (0.5, 0.5))));
    let wmc : BddWmc<f64> = BddWmc::new_with_default(0.0, 1.0, weight_map);
    let order = &VarOrder::linear_order(cnf.num_vars());
    let query_var = VarLabel::new_usize(0);

    // let ddnnf = man.from_cnf_topdown_sample(&VarOrder::linear_order(cnf.num_vars()), &cnf, &wmc, 10);
    let r = man.estimate_marginal(1000, order, query_var, &wmc, &cnf);

    let exact_ddnnf = man.from_cnf_topdown(order, &cnf);
    let z = man.unsmsoothed_wmc(exact_ddnnf, &wmc);
    let cond = man.condition(exact_ddnnf, query_var, true);
    let p = man.unsmsoothed_wmc(cond, &wmc) * 0.5;

    println!("result: {:?}, exact: {}, p: {p}, z: {z}", r, p / z);
}

fn compile_sdd(str: String, debug: bool) -> () {
    use rsdd::builder::sdd_builder::*;
    let cnf = Cnf::from_file(str);
    let order : Vec<VarLabel> = (0..cnf.num_vars()).map(|x| VarLabel::new(x as u64)).collect();
    let mut man = SddManager::new(even_split(&order, 3));
    let ddnnf = man.from_cnf(&cnf);
}

fn compile_bdd(str: String, debug: bool) -> () {
    use rsdd::builder::bdd_builder::*;
    let cnf = Cnf::from_file(str);
    let order : Vec<VarLabel> = (0..cnf.num_vars()).map(|x| VarLabel::new(x as u64)).collect();
    let mut man = BddManager::new(VarOrder::linear_order(cnf.num_vars()));
    let bdd = man.from_cnf(&cnf);
}

fn bench_cnf_bdd(cnf_str: String, debug: bool) -> Duration {
    let start = Instant::now();
    compile_topdown_nnf_sample(black_box(cnf_str), debug);
    start.elapsed()
}

fn main() {
    let args = Args::parse();

    rayon::ThreadPoolBuilder::new().num_threads(args.threads).build_global().unwrap();

    let cnf_strs = vec![
        // ("grid-50-10-1-q", String::from(include_str!("../cnf/50-10-1-q.cnf"))),
        // ("tiny1", String::from(include_str!("../cnf/tiny1.cnf"))),
        // ("tiny2", String::from(include_str!("../cnf/tiny2.cnf"))),
        // (
        //     "bench-01",
        //     String::from(include_str!("../cnf/bench-01.cnf")),
        // ),
        // (
        //     "bench-02",
        //     String::from(include_str!("../cnf/bench-02.cnf")),
        // ),
        (
            "bench-03",
            String::from(include_str!("../cnf/bench-03.cnf")),
        ),
        // ("php-4-6", String::from(include_str!("../cnf/php-4-6.cnf"))),
        // ("php-5-4", String::from(include_str!("../cnf/php-5-4.cnf"))),
        // ("php-12-14", String::from(include_str!("../cnf/php-12-14.cnf"))),
        // (
        //     "rand-3-25-75-1",
        //     String::from(include_str!("../cnf/rand-3-25-75-1.cnf")),
        // ),
        // (
        //     "rand-3-25-100-1",
        //     String::from(include_str!("../cnf/rand-3-25-100-1.cnf")),
        // ),
        // (
        //     "rand-3-25-100-2",
        //     String::from(include_str!("../cnf/rand-3-25-100-2.cnf")),
        // ),
        // (
        //     "rand-3-25-100-3",
        //     String::from(include_str!("../cnf/rand-3-25-100-3.cnf")),
        // ),
        // (
        //     "rand-3-37-75-1",
        //     String::from(include_str!("../cnf/rand-3-37-75-1.cnf")),
        // ),
        // (
        //     "rand-3-37-75-2",
        //     String::from(include_str!("../cnf/rand-3-37-75-2.cnf")),
        // ),
        // (
        //     "rand-3-37-75-3",
        //     String::from(include_str!("../cnf/rand-3-37-75-3.cnf")),
        // ),
        // (
        //     "rand-3-37-75-4",
        //     String::from(include_str!("../cnf/rand-3-37-75-4.cnf")),
        // ),
        // (
        //     "rand-3-37-75-5",
        //     String::from(include_str!("../cnf/rand-3-37-75-5.cnf")),
        // ),
        // (
        //     "rand-3-37-75-6",
        //     String::from(include_str!("../cnf/rand-3-37-75-6.cnf")),
        // ),
        // (
        //     "rand-3-37-75-7",
        //     String::from(include_str!("../cnf/rand-3-37-75-7.cnf")),
        // ),
        // (
        //     "rand-3-37-75-8",
        //     String::from(include_str!("../cnf/rand-3-37-75-8.cnf")),
        // ),
        // (
        //     "rand-3-50-200-1",
        //     String::from(include_str!("../cnf/rand-3-50-200-1.cnf")),
        // ),
        // (
        //     "rand-3-50-200-2",
        //     String::from(include_str!("../cnf/rand-3-50-200-2.cnf")),
        // ),

        // ("grid-75-16-2-q", String::from(include_str!("../cnf/75-16-2-q.cnf"))),
        // (
        //     "rand-3-50-200-3",
        //     String::from(include_str!("../cnf/rand-3-50-200-3.cnf")),
        // ),
        // (
        //     "rand-3-100-400-1",
        //     String::from(include_str!("../cnf/rand-3-100-400-1.cnf")),
        // ),
        // ("s298", String::from(include_str!("../cnf/s298.cnf"))),
        // ("grid-75-18-6-q", String::from(include_str!("../cnf/75-18-6-q.cnf"))),
        // ("grid-90-42-1-q", String::from(include_str!("../cnf/90-42-1-q.cnf"))),
        // ("grid-90-16-2-q", String::from(include_str!("../cnf/90-16-2-q.cnf"))),
        // ("s344", String::from(include_str!("../cnf/s344.cnf"))),
        // ("c8-easier", String::from(include_str!("../cnf/c8-easier.cnf"))),
        // ("s444", String::from(include_str!("../cnf/s444.cnf"))),
        // ("s510", String::from(include_str!("../cnf/s510.cnf"))),
        // ("s641", String::from(include_str!("../cnf/s641.cnf"))),
        // ("count", String::from(include_str!("../cnf/count.cnf"))),
        // (
        //     "c8-very-easy",
        //     String::from(include_str!("../cnf/c8-very-easy.cnf")),
        // ),
        // ("log1", String::from(include_str!("../cnf/log1.cnf"))),
        // ("c8", String::from(include_str!("../cnf/c8.cnf"))),
    ];

    println!("Benchmarking {} CNFs with {} thread{}", cnf_strs.len(), args.threads, if args.threads > 1 {"s"} else {""});

    let cnf_results: Vec<(&str, f64)> = cnf_strs.into_par_iter().map(|(cnf_name, cnf_str)| {
        let d = bench_cnf_bdd(cnf_str, args.debug);
        (cnf_name, d.as_secs_f64())
    }).collect();

    let benchmarks: HashMap<String, BenchmarkEntry> = cnf_results.into_iter().map(|(cnf_name, cnf_time)| {
        (cnf_name.to_string(), BenchmarkEntry {
            time_in_secs: cnf_time
        })
    }).into_iter().collect();

    // borrowed from: https://stackoverflow.com/questions/43753491/include-git-commit-hash-as-string-into-rust-program
    // and strips whitespace
    let git_output = std::process::Command::new("git").args(&["rev-parse", "HEAD"]).output().unwrap();
    let git_hash = String::from_utf8(git_output.stdout).unwrap().split_whitespace().collect();

    let benchmark_log = BenchmarkLog {
        git_hash,
        threads: args.threads,
        benchmarks,
    };

    let obj = json!(benchmark_log);
    let pretty_str = serde_json::to_string_pretty(&obj).unwrap();
    println!("{}", pretty_str);

    if !args.output.is_empty() {
        fs::write(&args.output, pretty_str).expect("Error writing to file");
        if args.debug {
            println!("Outputted to {}", args.output);
        }
    }
}
