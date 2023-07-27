use std::{collections::HashMap, fs, time::Instant};

use clap::Parser;
use rsdd::{
    builder::{bdd::RobddBuilder, cache::LruIteTable, BottomUpBuilder},
    constants::primes,
    repr::{
        bdd::BddPtr, ddnnf::DDNNFPtr, logical_expr::LogicalExpr, var_label::VarLabel,
        var_order::VarOrder, wmc::WmcParams,
    },
    serialize::LogicalSExpr,
    util::semirings::{FiniteField, RealSemiring, Semiring},
};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct VariableWeight<T> {
    low: T,
    high: T,
}

#[derive(Serialize, Deserialize)]
struct Config {
    order: Option<Vec<String>>,
}

impl Config {
    fn to_var_order(&self, mapping: &HashMap<&String, usize>) -> Option<VarOrder> {
        self.order.as_ref().map(|o| {
            VarOrder::new(
                o.iter()
                    .map(|var| {
                        VarLabel::new(*mapping.get(var).unwrap_or_else(|| {
                            panic!("Found unknown variable {} in order configuration", var)
                        }) as u64)
                    })
                    .collect(),
            )
        })
    }
}

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// input logical expression in s expression form
    #[clap(short, long, value_parser)]
    file: String,

    /// variable order for BDD.
    /// options: `linear`, `manual` (requires config in `-c`)
    #[clap(long, value_parser, default_value_t = String::from("manual"))]
    ordering: String,

    /// (optional) config file for variable ordering
    #[clap(short, long, value_parser)]
    config: Option<String>,

    /// path to weights JSON file
    #[clap(short, long, value_parser)]
    weights: Option<String>,

    /// whether or not to perform a smooth WMC
    #[clap(long, value_parser)]
    smooth: bool,

    /// show verbose output (including timing information, cache profiling, etc.)
    #[clap(short, long, value_parser)]
    verbose: bool,
}

fn main() {
    let args = Args::parse();

    let file = fs::read_to_string(&args.file)
        .unwrap_or_else(|e| panic!("Error reading file {}; error: {}", args.file, e));

    let config: Option<Config> = if let Some(path_to_config) = args.config {
        let config = fs::read_to_string(&path_to_config)
            .unwrap_or_else(|e| panic!("Error reading file {}; error: {}", path_to_config, e));
        Some(serde_json::from_str::<Config>(&config).unwrap_or_else(|e| {
            panic!(
                "Error parsing {} as JSON config option; error: {}",
                config, e
            )
        }))
    } else {
        None
    };

    let weights = if let Some(path_to_weights) = args.weights {
        let config = fs::read_to_string(&path_to_weights)
            .unwrap_or_else(|e| panic!("Error reading file {}; error: {}", path_to_weights, e));
        serde_json::from_str::<HashMap<String, VariableWeight<f64>>>(&config)
            .unwrap_or_else(|e| panic!("Error parsing {} as JSON weights; error: {}", config, e))
    } else {
        panic!("no weights file provided");
    };

    let sexpr = serde_sexpr::from_str::<LogicalSExpr>(&file).unwrap_or_else(|e| {
        panic!(
            "Error parsing {} as logical s-expression; error: {}",
            file, e
        )
    });
    let expr = LogicalExpr::from_sexpr(&sexpr);
    let mut num_vars = sexpr.unique_variables().len();
    let mut mapping = sexpr.variable_mapping();

    let start = Instant::now();

    let params: WmcParams<RealSemiring> =
        WmcParams::new(HashMap::from_iter(weights.iter().map(|(k, v)| {
            let label = mapping.get(k);

            match label {
                None => {
                    let n = (
                        VarLabel::new(num_vars as u64),
                        (RealSemiring(v.low), RealSemiring(v.high)),
                    );
                    mapping.insert(k, num_vars);
                    num_vars += 1;
                    n
                }
                Some(index) => (
                    VarLabel::new(*index as u64),
                    (RealSemiring(v.low), RealSemiring(v.high)),
                ),
            }
        })));

    let unweighted_params: WmcParams<FiniteField<{ primes::U64_LARGEST }>> =
        WmcParams::new(HashMap::from_iter(
            (0..num_vars as u64)
                .map(|v| (VarLabel::new(v), (FiniteField::one(), FiniteField::one()))),
        ));

    let order = match args.ordering.as_str() {
        "linear" => VarOrder::linear_order(num_vars),
        "manual" => config
            .unwrap_or_else(|| {
                panic!(
                    "Selected manual option, but could not find valid config with variable order"
                )
            })
            .to_var_order(&mapping)
            .unwrap_or_else(|| {
                panic!(
                    "Selected manual option, but could not find valid config with variable order"
                )
            }),
        _ => todo!(),
    };

    let builder = RobddBuilder::<LruIteTable<BddPtr>>::new(order.clone());
    let bdd = builder.compile_logical_expr(&expr);

    let bdd = if args.smooth {
        builder.smooth(bdd, num_vars)
    } else {
        bdd
    };

    let res = bdd.wmc(&order, &params);

    let elapsed = start.elapsed();

    println!(
        "unweighted model count: {}\nweighted model count: {}",
        builder
            .smooth(bdd, num_vars)
            .wmc(&order, &unweighted_params),
        res
    );

    if args.verbose {
        eprintln!("=== METADATA ===");
        eprintln!("variable mapping: {:?}", sexpr.variable_mapping());
        eprintln!("variable ordering: {}", order);
        eprintln!("=== STATS ===");

        let stats = builder.stats();
        eprintln!("compilation time: {:.4}s", elapsed.as_secs_f64());
        eprintln!("recursive calls: {}", stats.num_recursive_calls);
    }
}
