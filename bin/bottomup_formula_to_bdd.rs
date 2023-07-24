use std::{fs, time::Instant};

use clap::Parser;
use rsdd::{
    builder::{bdd::RobddBuilder, cache::LruIteTable, BottomUpBuilder},
    repr::{bdd::BddPtr, logical_expr::LogicalExpr, var_order::VarOrder},
    serialize::{BDDSerializer, LogicalSExpr},
};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// input logical expression in s expression form
    #[clap(short, long, value_parser)]
    file: String,

    /// variable order for BDD.
    #[clap(long, value_parser, default_value_t = String::from("linear"))]
    ordering: String,

    /// show verbose output (including timing information, cache profiling, etc.)
    #[clap(short, long, value_parser)]
    verbose: bool,
}

fn main() {
    let args = Args::parse();

    let file = fs::read_to_string(args.file).unwrap();

    let sexpr = serde_sexpr::from_str::<LogicalSExpr>(&file).unwrap();
    let expr = LogicalExpr::from_sexpr(&sexpr);

    let start = Instant::now();

    let order = match args.ordering.as_str() {
        "linear" => VarOrder::linear_order(sexpr.unique_variables().len()),
        _ => todo!(),
    };

    let builder = RobddBuilder::<LruIteTable<BddPtr>>::new(order);
    let bdd = builder.compile_logical_expr(&expr);

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
