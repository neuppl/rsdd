use std::{collections::HashMap, fs, time::Instant};

use clap::Parser;
use rsdd::{
    builder::decision_nnf::{
        DecisionNNFBuilder, SemanticDecisionNNFBuilder, StandardDecisionNNFBuilder,
    },
    constants::primes,
    repr::{
        bdd::BddPtr, cnf::Cnf, ddnnf::DDNNFPtr, var_label::VarLabel, var_order::VarOrder,
        wmc::WmcParams,
    },
    util::semirings::RealSemiring,
};

use rand::seq::SliceRandom;
use rand::thread_rng;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// input CNF in DIMACS form
    #[clap(short, long, value_parser)]
    file: String,

    /// number of variable orders to try. 0 means just do linear
    #[clap(short, long, value_parser, default_value_t = 0)]
    orders: usize,
}

fn diff_by_wmc(num_vars: usize, order: &VarOrder, std_dnnf: BddPtr, sem_dnnf: BddPtr) -> f64 {
    let weight_map: HashMap<VarLabel, (RealSemiring, RealSemiring)> =
        HashMap::from_iter((0..num_vars).map(|x| {
            (
                VarLabel::new(x as u64),
                (RealSemiring(0.3), RealSemiring(0.7)),
            )
        }));
    let params = WmcParams::new(weight_map);

    let std_wmc = std_dnnf.wmc(order, &params);
    let sem_wmc = sem_dnnf.wmc(order, &params);

    f64::abs(std_wmc.0 - sem_wmc.0)
}

fn compare_sem_and_std(cnf: &Cnf, order: &VarOrder) -> (usize, usize) {
    let std_builder = StandardDecisionNNFBuilder::new(order.clone());

    let start = Instant::now();
    let std_dnnf = std_builder.compile_cnf_topdown(cnf);
    let std_time = start.elapsed().as_secs_f64();

    let sem_builder = SemanticDecisionNNFBuilder::<{ primes::U64_LARGEST }>::new(order.clone());

    let start = Instant::now();
    let sem_dnnf = sem_builder.compile_cnf_topdown(cnf);
    let sem_time = start.elapsed().as_secs_f64();

    if diff_by_wmc(cnf.num_vars(), order, std_dnnf, sem_dnnf) > 0.0001 {
        println!(
            "error on input {}\n std bdd: {}\nsem bdd: {}",
            cnf,
            std_dnnf.to_string_debug(),
            sem_dnnf.to_string_debug()
        );
    }

    let sem_nodes = sem_dnnf.count_nodes();
    let std_nodes = std_dnnf.count_nodes();

    println!(
        "sem/std size ratio: {:.4}x ({} / {})",
        (sem_nodes as f64 / std_nodes as f64),
        sem_nodes,
        std_nodes
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

    (sem_nodes, std_nodes)
}

struct RandomVarOrders {
    vars: u64,
    total: usize,
    current: usize,
}

impl RandomVarOrders {
    fn new(cnf: &Cnf, total: usize) -> RandomVarOrders {
        RandomVarOrders {
            vars: cnf.num_vars() as u64,
            total,
            current: 0,
        }
    }
}

impl Iterator for RandomVarOrders {
    type Item = VarOrder;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current >= self.total {
            return None;
        }

        self.current += 1;

        let mut order: Vec<VarLabel> = (0..self.vars).map(VarLabel::new).collect();

        order.shuffle(&mut thread_rng());

        Some(VarOrder::new(order))
    }
}

fn main() {
    let args = Args::parse();

    let cnf_input = fs::read_to_string(args.file).expect("Should have been able to read the file");

    let cnf = Cnf::from_file(cnf_input);

    if args.orders == 0 {
        let linear_order = VarOrder::linear_order(cnf.num_vars());

        compare_sem_and_std(&cnf, &linear_order);
        return;
    }

    let orders = RandomVarOrders::new(&cnf, args.orders);

    let mut avg_sem_nodes = 0;
    let mut avg_std_nodes = 0;

    for order in orders {
        println!("\norder: {}\n", order);
        let (sem, std) = compare_sem_and_std(&cnf, &order);
        avg_sem_nodes += sem;
        avg_std_nodes += std;
    }

    println!("=== AVG, n = {} ===", args.orders);

    println!(
        "sem/std size ratio: {:.2}x ({} / {})",
        (avg_sem_nodes as f64 / avg_std_nodes as f64),
        avg_sem_nodes / args.orders,
        avg_std_nodes / args.orders
    );
}
