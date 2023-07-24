use std::{fs, time::Instant};

use clap::Parser;
use rsdd::{
    builder::{
        bdd::{BddBuilder, RobddBuilder},
        cache::LruIteTable,
    },
    plan::BddPlan,
    repr::{bdd::BddPtr, cnf::Cnf, dtree::DTree},
    serialize::BDDSerializer,
};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// input CNF in DIMACS form
    #[clap(short, long, value_parser)]
    file: String,

    /// variable order for BDD.
    /// defaults to `auto_minfill`, which uses a min-fill heuristic.
    /// allowed: `auto_minfill`, `auto_force`
    #[clap(long, value_parser, default_value_t = String::from("auto_minfill"))]
    order: String,

    /// compilation order (a tree describing sequence of clause-conjunctions).
    /// defaults to `dtree`, a variable order-based decision tree decomposition
    #[clap(long, value_parser, default_value_t = String::from("dtree"))]
    strategy: String,

    /// show verbose output (including timing information, cache profiling, etc.)
    #[clap(short, long, value_parser)]
    verbose: bool,
}

fn main() {
    let args = Args::parse();

    let file = fs::read_to_string(args.file).unwrap();

    let cnf = Cnf::from_dimacs(&file);

    let start = Instant::now();

    let order = match args.order.as_str() {
        "auto_minfill" => cnf.min_fill_order(),
        "auto_force" => cnf.force_order(),
        _ => panic!(
            "Unknown order {} provided, expected one of: `auto_minfill`, `auto_force`",
            args.order
        ),
    };

    let plan = match args.strategy.as_str() {
        "dtree" => {
            let dtree = DTree::from_cnf(&cnf, &order);
            BddPlan::from_dtree(&dtree)
        }
        _ => panic!(
            "Unknown strategy {} provided, expected one of: `dtree`",
            args.order
        ),
    };

    let builder = RobddBuilder::<LruIteTable<BddPtr>>::new(order);
    let bdd = builder.compile_plan(&plan);

    let elapsed = start.elapsed();

    if args.verbose {
        eprintln!("=== STATS ===");

        let stats = builder.stats();
        eprintln!("compilation time: {:.4}s", elapsed.as_secs_f64());
        eprintln!("recursive calls: {}", stats.num_recursive_calls);
    }

    let serialized = BDDSerializer::from_bdd(bdd);

    println!("{}", serde_json::to_string(&serialized).unwrap());
}
