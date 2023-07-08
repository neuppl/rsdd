extern crate rsdd;

use clap::Parser;
use rsdd::builder::bdd::builder::BddBuilder;
use rsdd::builder::cache::lru_app::BddApplyTable;
use rsdd::plan::bdd_plan::BddPlan;
use rsdd::repr::cnf::Cnf;
use rsdd::repr::ddnnf::DDNNFPtr;
use rsdd::repr::dtree::DTree;
use rsdd::repr::var_label::VarLabel;
use rsdd::repr::var_order::VarOrder;
use rsdd::repr::vtree::VTree;
use rsdd::serialize::{ser_bdd, ser_sdd, ser_vtree};
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::fs::{self, File};
use std::io::Write;
use std::time::Instant;

/// Test driver for one-shot benchmark
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Print debug messages to console
    #[clap(long, value_parser, default_value_t = false)]
    debug: bool,

    /// Dumps the vtree to the specified file in JSON format
    #[clap(long, value_parser)]
    dump_vtree: Option<String>,

    /// Dumps the bdd to the specified file in JSON format
    #[clap(long, value_parser)]
    dump_bdd: Option<String>,

    /// Dumps the sdd to the specified file in JSON format
    #[clap(long, value_parser)]
    dump_sdd: Option<String>,

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
    size: usize,
}

fn compile_topdown_nnf(str: String, _args: &Args) -> BenchResult {
    let cnf = Cnf::from_file(str);
    let order = VarOrder::linear_order(cnf.num_vars());
    let builder = rsdd::builder::decision_nnf_builder::DecisionNNFBuilder::new(order);
    // let order = cnf.force_order();
    let ddnnf = builder.compile_cnf_topdown(&cnf);
    println!("num redundant: {}", builder.num_logically_redundant());
    BenchResult {
        num_recursive: 0,
        size: ddnnf.count_nodes(),
    }
}

fn compile_sdd_dtree(str: String, _args: &Args) -> BenchResult {
    use rsdd::builder::sdd::{builder::SddBuilder, compression::CompressionSddBuilder};
    let cnf = Cnf::from_file(str);
    let dtree = DTree::from_cnf(&cnf, &cnf.min_fill_order());
    let vtree = VTree::from_dtree(&dtree).unwrap();
    let builder = CompressionSddBuilder::new(vtree.clone());
    let sdd = builder.compile_cnf(&cnf);

    if let Some(path) = &_args.dump_sdd {
        let json = ser_sdd::SDDSerializer::from_sdd(sdd);
        let mut file = File::create(path).unwrap();
        let r = file.write_all(serde_json::to_string(&json).unwrap().as_bytes());
        assert!(r.is_ok(), "Error writing file");
    }

    if let Some(path) = &_args.dump_vtree {
        let json = ser_vtree::VTreeSerializer::from_vtree(&vtree);
        let mut file = File::create(path).unwrap();
        let r = file.write_all(serde_json::to_string(&json).unwrap().as_bytes());
        assert!(r.is_ok(), "Error writing file");
    }

    BenchResult {
        num_recursive: builder.stats().num_recursive_calls,
        size: sdd.count_nodes(),
    }
}

fn compile_sdd_rightlinear(str: String, _args: &Args) -> BenchResult {
    use rsdd::builder::sdd::{builder::SddBuilder, compression::CompressionSddBuilder};
    let cnf = Cnf::from_file(str);
    let o: Vec<VarLabel> = (0..cnf.num_vars())
        .map(|x| VarLabel::new(x as u64))
        .collect();
    let vtree = VTree::right_linear(&o);
    let builder = CompressionSddBuilder::new(vtree.clone());
    let sdd = builder.compile_cnf(&cnf);

    if let Some(path) = &_args.dump_sdd {
        let json = ser_sdd::SDDSerializer::from_sdd(sdd);
        let mut file = File::create(path).unwrap();
        let r = file.write_all(serde_json::to_string(&json).unwrap().as_bytes());
        assert!(r.is_ok(), "Error writing file");
    }

    if let Some(path) = &_args.dump_vtree {
        let json = ser_vtree::VTreeSerializer::from_vtree(&vtree);
        let mut file = File::create(path).unwrap();
        let r = file.write_all(serde_json::to_string(&json).unwrap().as_bytes());
        assert!(r.is_ok(), "Error writing file");
    }

    BenchResult {
        num_recursive: builder.stats().num_recursive_calls,
        size: sdd.count_nodes(),
    }
}

fn compile_bdd(str: String, _args: &Args) -> BenchResult {
    use rsdd::builder::bdd::robdd::RobddBuilder;
    use rsdd::repr::bdd::BddPtr;

    let cnf = Cnf::from_file(str);
    let builder = RobddBuilder::<BddApplyTable<BddPtr>>::new_default_order_lru(cnf.num_vars());
    let bdd = builder.compile_cnf(&cnf);

    if let Some(path) = &_args.dump_bdd {
        let json = ser_bdd::BDDSerializer::from_bdd(bdd);
        let mut file = File::create(path).unwrap();
        let r = file.write_all(serde_json::to_string(&json).unwrap().as_bytes());
        assert!(r.is_ok(), "Error writing file");
    }

    BenchResult {
        num_recursive: builder.num_recursive_calls(),
        size: bdd.count_nodes(),
    }
}

fn compile_bdd_dtree(str: String, _args: &Args) -> BenchResult {
    use rsdd::builder::bdd::robdd::RobddBuilder;
    use rsdd::repr::bdd::BddPtr;

    let cnf = Cnf::from_file(str);
    let order = cnf.min_fill_order();
    let dtree = DTree::from_cnf(&cnf, &order);
    let builder =
        RobddBuilder::<BddApplyTable<BddPtr>>::new(order, BddApplyTable::new(cnf.num_vars()));
    let plan = BddPlan::from_dtree(&dtree);
    let bdd = builder.compile_plan(&plan);

    if let Some(path) = &_args.dump_bdd {
        let json = ser_bdd::BDDSerializer::from_bdd(bdd);
        let mut file = File::create(path).unwrap();
        let r = file.write_all(serde_json::to_string(&json).unwrap().as_bytes());
        assert!(r.is_ok(), "Error writing file");
    }

    BenchResult {
        num_recursive: builder.num_recursive_calls(),
        size: bdd.count_nodes(),
    }
}

fn main() {
    let args = Args::parse();

    let file = fs::read_to_string(args.file.clone()).unwrap();

    let start = Instant::now();
    let res = match args.mode.as_str() {
        "bdd_topological" => compile_bdd(file, &args),
        "bdd_dtree_minfill" => compile_bdd_dtree(file, &args),
        "dnnf_topdown" => compile_topdown_nnf(file, &args),
        "sdd_right_linear" => compile_sdd_rightlinear(file, &args),
        "sdd_dtree_minfill" => compile_sdd_dtree(file, &args),
        x => panic!("Unknown mode option: {}", x),
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
