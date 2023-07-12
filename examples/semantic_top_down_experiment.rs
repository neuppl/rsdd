extern crate rsdd;

use std::{collections::HashMap, fs, time::Instant};

use clap::Parser;
use rsdd::{
    builder::decision_nnf::{
        builder::DecisionNNFBuilder, semantic::SemanticDecisionNNFBuilder,
        standard::StandardDecisionNNFBuilder,
    },
    constants::primes,
    repr::{
        bdd::BddPtr, cnf::Cnf, ddnnf::DDNNFPtr, var_label::VarLabel, var_order::VarOrder,
        wmc::WmcParams,
    },
    util::semirings::{realsemiring::RealSemiring, semiring_traits::Semiring},
};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// input CNF in DIMACS form
    #[clap(short, long, value_parser)]
    file: String,
}

fn diff_by_wmc(num_vars: usize, order: &VarOrder, std_dnnf: BddPtr, sem_dnnf: BddPtr) -> f64 {
    let weight_map: HashMap<VarLabel, (RealSemiring, RealSemiring)> =
        HashMap::from_iter((0..num_vars).map(|x| {
            (
                VarLabel::new(x as u64),
                (RealSemiring(0.3), RealSemiring(0.7)),
            )
        }));
    let params = WmcParams::new_with_default(RealSemiring::zero(), RealSemiring::one(), weight_map);

    let std_wmc = std_dnnf.wmc(order, &params);
    let sem_wmc = sem_dnnf.wmc(order, &params);

    f64::abs(std_wmc.0 - sem_wmc.0)
}

fn main() {
    let args = Args::parse();

    let cnf_input = fs::read_to_string(args.file).expect("Should have been able to read the file");

    let cnf = Cnf::from_file(cnf_input);

    let linear_order = VarOrder::linear_order(cnf.num_vars());

    let std_builder = StandardDecisionNNFBuilder::new(linear_order.clone());

    let start = Instant::now();
    let std_dnnf = std_builder.compile_cnf_topdown(&cnf);
    let std_time = start.elapsed().as_secs_f64();

    let sem_builder =
        SemanticDecisionNNFBuilder::<{ primes::U64_LARGEST }>::new(linear_order.clone());

    let start = Instant::now();
    let sem_dnnf = sem_builder.compile_cnf_topdown(&cnf);
    let sem_time = start.elapsed().as_secs_f64();

    if diff_by_wmc(cnf.num_vars(), &linear_order, std_dnnf, sem_dnnf) > 0.0001 {
        println!(
            "error on input {}\n std bdd: {}\nsem bdd: {}",
            cnf,
            std_dnnf.to_string_debug(),
            sem_dnnf.to_string_debug()
        );
    }

    println!(
        "sem/std size ratio: {:.4}x ({} / {})",
        (sem_dnnf.count_nodes() as f64 / std_dnnf.count_nodes() as f64),
        sem_dnnf.count_nodes(),
        std_dnnf.count_nodes()
    );
    println!(
        "sem/std alloc ratio: {:.4}x ({} / {})",
        (sem_builder.stats().num_nodes_alloc as f64 / std_builder.stats().num_nodes_alloc as f64),
        sem_builder.stats().num_nodes_alloc,
        std_builder.stats().num_nodes_alloc
    );
    println!(
        "sem/std time ratio: {:.4}x ({:.4}s / {:.4}s)",
        (sem_time / std_time),
        sem_time,
        std_time
    );
}
