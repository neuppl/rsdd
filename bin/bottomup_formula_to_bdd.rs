use std::{fs, time::Instant};

use clap::Parser;
use rsdd::{
    builder::{bdd::RobddBuilder, cache::LruIteTable, BottomUpBuilder},
    repr::{bdd::BddPtr, logical_expr::LogicalExpr, var_label::VarLabel, var_order::VarOrder},
    serialize::{BDDSerializer, LogicalSExpr},
};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct Config {
    order: Vec<String>,
}

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// input logical expression in s expression form
    #[clap(short, long, value_parser)]
    file: String,

    /// variable order for BDD.
    /// options: `linear`, `manual` (requires config in `-c`)
    #[clap(long, value_parser, default_value_t = String::from("linear"))]
    ordering: String,

    /// (optional) config file for variable ordering
    #[clap(short, long, value_parser)]
    config: Option<String>,

    /// show verbose output (including timing information, cache profiling, etc.)
    #[clap(short, long, value_parser)]
    verbose: bool,
}

fn main() {
    let args = Args::parse();

    let file = fs::read_to_string(args.file).unwrap();

    let config: Option<Config> = if let Some(path_to_config) = args.config {
        let config = fs::read_to_string(path_to_config).unwrap();
        Some(serde_json::from_str::<Config>(&config).unwrap())
    } else {
        None
    };

    let sexpr = serde_sexpr::from_str::<LogicalSExpr>(&file).unwrap();
    let expr = LogicalExpr::from_sexpr(&sexpr);

    let start = Instant::now();

    let order = match args.ordering.as_str() {
        "linear" => VarOrder::linear_order(sexpr.unique_variables().len()),
        "manual" => {
            let mapping = sexpr.variable_mapping();
            match config {
                Some(c) => VarOrder::new(
                    c.order
                        .iter()
                        .map(|var| VarLabel::new(*mapping.get(var).unwrap() as u64))
                        .collect(),
                ),
                None => panic!("error; `manual` ordering requires config, passed in with -c"),
            }
        }
        _ => todo!(),
    };

    let builder = RobddBuilder::<LruIteTable<BddPtr>>::new(order.clone());
    let bdd = builder.compile_logical_expr(&expr);

    let elapsed = start.elapsed();

    if args.verbose {
        eprintln!("=== METADATA ===");
        eprintln!("variable mapping: {:?}", sexpr.variable_mapping());
        eprintln!("variable ordering: {}", order);
        eprintln!("=== STATS ===");

        let stats = builder.stats();
        eprintln!("compilation time: {:.4}s", elapsed.as_secs_f64());
        eprintln!("recursive calls: {}", stats.num_recursive_calls);
    }

    let serialized = BDDSerializer::from_bdd(bdd);

    println!("{}", serde_json::to_string(&serialized).unwrap());
}
