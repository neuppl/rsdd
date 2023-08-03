use std::{collections::HashMap, fs};

use clap::Parser;
use rand::Rng;
use rsdd::{
    builder::{bdd::RobddBuilder, cache::AllIteTable, BottomUpBuilder},
    repr::{BddPtr, Cnf, VarLabel, WmcParams},
    util::semirings::RealSemiring,
};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// input CNF in DIMACS form
    #[arg(short, long, value_parser)]
    file: String,

    /// vars to include in marginal map
    #[arg(short, long, value_parser, action=clap::ArgAction::Append)]
    vars: Vec<usize>,

    /// weights to include in marginal map, in order;
    /// the rest of the weights will be randomly generated
    #[arg(short, long, value_parser, action=clap::ArgAction::Append)]
    weights: Vec<f64>,
}

fn gen_all_weights(
    prev_weights: &[f64],
    num_vars: usize,
) -> HashMap<VarLabel, (RealSemiring, RealSemiring)> {
    let mut var_to_val = HashMap::default();
    for (index, weight) in prev_weights.iter().enumerate() {
        var_to_val.insert(
            VarLabel::new(index as u64),
            (RealSemiring(*weight), RealSemiring(1.0 - weight)),
        );
    }

    let mut rng = rand::thread_rng();

    for index in prev_weights.len()..num_vars {
        let weight = rng.gen_range(0.0..1.0);
        var_to_val.insert(
            VarLabel::new(index as u64),
            (RealSemiring(weight), RealSemiring(1.0 - weight)),
        );
    }
    var_to_val
}

fn main() {
    let args = Args::parse();

    let cnf_input = fs::read_to_string(args.file).expect("Should have been able to read the file");

    let cnf = Cnf::from_dimacs(&cnf_input);
    println!("num vars: {}", cnf.num_vars());

    // TODO: allow user to pick varorder
    let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(cnf.num_vars());

    let bdd = builder.compile_cnf(&cnf);

    if args.vars.is_empty() {
        panic!("No vars provided. Please provide at least one with -v")
    }

    let vars = args
        .vars
        .iter()
        .map(|v| VarLabel::new(*v as u64))
        .collect::<Vec<VarLabel>>();

    let var_to_val = gen_all_weights(&args.weights, builder.num_vars());

    let wmc = WmcParams::new(var_to_val);

    let (probability, partial) = bdd.marginal_map(&vars, builder.num_vars(), &wmc);

    println!("MAP: {:.2}%", probability * 100.0);
    println!("{}", partial);
}
