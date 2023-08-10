use std::{
    collections::HashMap,
    fmt::Debug,
    fs::{self, File},
    io::Write,
    time::Instant,
};

use clap::Parser;
use rsdd::{
    builder::{bdd::RobddBuilder, cache::LruIteTable, BottomUpBuilder},
    constants::primes,
    repr::{BddPtr, DDNNFPtr, LogicalExpr, PartialModel, VarLabel, VarOrder, WmcParams},
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
    partials: Option<Vec<HashMap<String, bool>>>,
}

#[derive(Serialize, Debug)]
struct PartialWmcResult<T: Semiring + Serialize + Debug> {
    partial_model: HashMap<String, bool>,
    wmc: T,
}

#[derive(Serialize)]
struct PartialWmcOutput<T: Semiring + Serialize + Debug> {
    bdd_size: usize,
    results: Vec<PartialWmcResult<T>>,
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

    /// output file to write results to
    #[clap(short, long, value_parser)]
    output: Option<String>,

    /// variable order for BDD.
    /// options: `linear`, `manual` (requires config in `-c`)
    #[clap(long, value_parser, default_value_t = String::from("linear"))]
    ordering: String,

    /// (optional) config file for variable ordering
    #[clap(short, long, value_parser)]
    config: Option<String>,

    /// path to weights JSON file
    #[clap(short, long, value_parser)]
    weights: Option<String>,

    /// show verbose output (including timing information, cache profiling, etc.)
    #[clap(short, long, value_parser)]
    verbose: bool,
}

fn generate_partial_assignments(
    partials: &[HashMap<String, bool>],
    mapping: &HashMap<&String, usize>,
    num_vars: usize,
) -> Vec<PartialModel> {
    let inverse_mapping: HashMap<usize, &String> =
        HashMap::from_iter(mapping.iter().map(|(k, v)| (*v, *k)));
    partials
        .iter()
        .map(|assignments| {
            PartialModel::from_vec(
                (0..num_vars)
                    .map(|index| {
                        if let Some(str) = inverse_mapping.get(&index) {
                            if let Some(polarity) = assignments.get(*str) {
                                return Some(*polarity);
                            }
                        }
                        None
                    })
                    .collect::<Vec<_>>(),
            )
        })
        .collect()
}

fn serialize_partial_model(
    model: &PartialModel,
    mapping: &HashMap<&String, usize>,
) -> HashMap<String, bool> {
    let inverse_mapping: HashMap<usize, &String> =
        HashMap::from_iter(mapping.iter().map(|(k, v)| (*v, *k)));
    let mut h = HashMap::new();

    model.true_assignments.iter().for_each(|v| {
        h.insert(
            (*inverse_mapping.get(&(v.value() as usize)).unwrap()).clone(),
            true,
        );
    });

    model.false_assignments.iter().for_each(|v| {
        h.insert(
            (*inverse_mapping.get(&(v.value() as usize)).unwrap()).clone(),
            false,
        );
    });

    h
}

fn single_wmc(
    expr: LogicalExpr,
    num_vars: usize,
    order: VarOrder,
    params: WmcParams<RealSemiring>,
    verbose: bool,
) {
    let builder = RobddBuilder::<LruIteTable<BddPtr>>::new(order.clone());

    let unweighted_params: WmcParams<FiniteField<{ primes::U64_LARGEST }>> =
        WmcParams::new(HashMap::from_iter(
            (0..num_vars as u64)
                .map(|v| (VarLabel::new(v), (FiniteField::one(), FiniteField::one()))),
        ));

    let start = Instant::now();

    let bdd = builder.compile_logical_expr(&expr);

    let bdd = builder.smooth(bdd, num_vars);

    let res = bdd.wmc(&order, &params);

    let elapsed = start.elapsed();

    println!(
        "unweighted model count: {}\nweighted model count: {}",
        builder
            .smooth(bdd, num_vars)
            .wmc(&order, &unweighted_params),
        res
    );

    if verbose {
        eprintln!("=== STATS ===");

        let stats = builder.stats();
        eprintln!("compilation time: {:.4}s", elapsed.as_secs_f64());
        eprintln!("recursive calls: {}", stats.num_recursive_calls);
    }
}

fn partial_wmcs(
    expr: LogicalExpr,
    num_vars: usize,
    order: &VarOrder,
    params: &WmcParams<RealSemiring>,
    partials: &[PartialModel],
    mapping: &HashMap<&String, usize>,
    verbose: bool,
) -> PartialWmcOutput<RealSemiring> {
    let builder = RobddBuilder::<LruIteTable<BddPtr>>::new(order.clone());
    let mut results = Vec::new();

    let start = Instant::now();

    let bdd = builder.compile_logical_expr(&expr);

    let init_compilation = start.elapsed();

    for model in partials {
        let conditioned = builder.condition_model(bdd, model);
        let wmc = builder.smooth(conditioned, num_vars).wmc(order, params);

        let res = PartialWmcResult {
            partial_model: serialize_partial_model(model, mapping),
            wmc,
        };

        println!("{:?}", res);

        results.push(res);
    }

    let elapsed = start.elapsed();

    if verbose {
        eprintln!("=== STATS ===");

        let stats = builder.stats();
        eprintln!(
            "initial compilation time: {:.4}s",
            init_compilation.as_secs_f64()
        );
        eprintln!("total compilation time: {:.4}s", elapsed.as_secs_f64());
        eprintln!(
            "amortized partial model time: {:.4}s",
            elapsed.as_secs_f64() / partials.len() as f64
        );
        eprintln!("recursive calls: {}", stats.num_recursive_calls);
    }

    PartialWmcOutput {
        bdd_size: bdd.count_nodes(),
        results,
    }
}

fn main() {
    let args = Args::parse();

    let file = fs::read_to_string(&args.file)
        .unwrap_or_else(|e| panic!("Error reading file {}; error: {}", args.file, e));

    let config = if let Some(path_to_config) = args.config {
        let config = fs::read_to_string(&path_to_config)
            .unwrap_or_else(|e| panic!("Error reading file {}; error: {}", path_to_config, e));
        serde_json::from_str::<Config>(&config).unwrap_or_else(|e| {
            panic!(
                "Error parsing {} as JSON config option; error: {}",
                config, e
            )
        })
    } else {
        Config {
            order: None,
            partials: None,
        }
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

    let order = match args.ordering.as_str() {
        "linear" => VarOrder::linear_order(num_vars),
        "manual" => config.to_var_order(&mapping).unwrap_or_else(|| {
            panic!("Selected manual option, but could not find valid config with variable order")
        }),
        _ => todo!(),
    };

    if let Some(partials) = config.partials {
        let partials = generate_partial_assignments(&partials, &mapping, num_vars);
        let output = partial_wmcs(
            expr,
            num_vars,
            &order,
            &params,
            &partials,
            &mapping,
            args.verbose,
        );

        if let Some(path) = args.output {
            let mut file = File::create(path).unwrap();
            let r = file.write_all(serde_json::to_string_pretty(&output).unwrap().as_bytes());
            assert!(r.is_ok(), "Error writing file");
        }
    } else {
        single_wmc(expr, num_vars, order.clone(), params, args.verbose);
    }

    if args.verbose {
        eprintln!("=== METADATA ===");
        eprintln!("variable mapping: {:?}", sexpr.variable_mapping());
        eprintln!("variable ordering: {}", order);
    }
}
