extern crate criterion;
extern crate rayon;
extern crate rsdd;
extern crate serde_json;

use clap::Parser;
use rsdd::repr::dtree::DTree;
use std::env;
use std::fs;
use rayon::prelude::*;
use rsdd::builder::cache::lru_app::BddApplyTable;
use rsdd::repr::bdd::BddPtr;
use rsdd::repr::cnf::Cnf;
use rsdd::repr::ddnnf::DDNNFPtr;
use rsdd::repr::var_label::VarLabel;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::vtree::VTree;
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::collections::HashMap;
use std::time::{Duration, Instant};

/// Test driver for one-shot benchmark
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Print debug messages to console
    #[clap(short, long, value_parser, default_value_t = false)]
    debug: bool,

    /// File to benchmark
    #[clap(short, long, value_parser)]
    file: String,


    /// Mode to compile in
    /// Options: 
    ///    bdd_topological
    ///    sdd_right_linear
    ///    sdd_topological_elim: compile in a topological elimination order
    ///    sdd_with_vtree: compile with a supplied vtree file
    #[clap(short, long, value_parser)]
    mode: String,

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
    name: String,
    num_recursive: usize,
    time_in_sec: f64,
    circuit_size: usize,
    mode: String,
}

struct BenchResult {
    num_recursive: usize,
    size: usize
}

// TODO: resolve unused
#[allow(unused)]
fn compile_topdown_nnf(str: String, args: &Args) -> BenchResult {
    let cnf = Cnf::from_file(str);
    let mut man = rsdd::builder::decision_nnf_builder::DecisionNNFBuilder::new(cnf.num_vars());
    let order = VarOrder::linear_order(cnf.num_vars());
    // let order = cnf.force_order();
    let ddnnf = man.from_cnf_topdown(&order, &cnf);
    BenchResult { num_recursive: 0, size: ddnnf.count_nodes() }

}

fn compile_sdd_dtree(str: String, args: &Args) -> BenchResult {
    use rsdd::builder::sdd_builder::*;
    let cnf = Cnf::from_file(str);
    let dtree = DTree::from_cnf(&cnf, &VarOrder::linear_order(cnf.num_vars()));
    let mut man = SddManager::new(dtree.to_vtree().unwrap());
    let _sdd = man.from_cnf(&cnf);
    BenchResult { num_recursive: man.get_stats().num_rec, size: _sdd.count_nodes() }
}


fn compile_sdd_rightlinear(str: String, args: &Args) -> BenchResult {
    use rsdd::builder::sdd_builder::*;
    let cnf = Cnf::from_file(str);
    let o : Vec<VarLabel> = (0..cnf.num_vars()).map(|x| VarLabel::new(x as u64)).collect();
    let mut man = SddManager::new(VTree::right_linear(&o));
    let _sdd = man.from_cnf(&cnf);
    BenchResult { num_recursive: man.get_stats().num_rec, size: _sdd.count_nodes() }
}

// TODO: resolve unused
#[allow(unused)]
fn compile_bdd(str: String, args: &Args) -> BenchResult {
    use rsdd::builder::bdd_builder::*;
    let cnf = Cnf::from_file(str);
    let mut man = BddManager::<BddApplyTable<BddPtr>>::new_default_order_lru(cnf.num_vars());
    let _bdd = man.from_cnf(&cnf);
    BenchResult { num_recursive: man.num_recursive_calls(), size: _bdd.count_nodes() }
}


fn main() {
    let args = Args::parse();

    let file = fs::read_to_string(args.file.clone()).unwrap();

    let start = Instant::now();
    let res = match args.mode.as_str() {
        "bdd_topological" => compile_bdd(file, &args),
        "sdd_right_linear" => compile_sdd_rightlinear(file, &args),
        "sdd_dtree_topological" => compile_sdd_dtree(file, &args),
        x => panic!("Unknown mode option: {}", x)
    };
    let duration = start.elapsed();

    let benchmark_log = BenchmarkLog {
        name: args.file,
        time_in_sec: duration.as_secs_f64(),
        num_recursive: res.num_recursive,
        mode: args.mode,
        circuit_size: res.size,
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
