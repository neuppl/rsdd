use std::{
    fs,
    time::{Duration, Instant},
};

use clap::Parser;
use rsdd::{
    builder::{parallel::SemanticBddBuilder, BottomUpBuilder},
    constants::primes,
    repr::{
        cnf::Cnf,
        ddnnf::{create_semantic_hash_map, DDNNFPtr},
        var_order::VarOrder,
    },
};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// input CNF in DIMACS form
    #[clap(short, long, value_parser)]
    file: String,

    /// number of splits to perform on the data
    #[clap(short, long, value_parser, default_value_t = 4)]
    num_splits: usize,

    /// number of threads to test on;
    /// a value of 0 will only run single-threaded
    #[clap(short, long, value_parser, default_value_t = 0)]
    threads: usize,
}

fn split_cnf(cnf: &Cnf, num_splits: usize) -> Vec<Cnf> {
    cnf.clauses()
        .chunks(cnf.clauses().len() / num_splits + 1)
        .map(Cnf::new)
        .collect()
}

fn single_threaded(cnf: &Cnf, num_splits: usize) {
    let num_vars = cnf.num_vars();
    let map = create_semantic_hash_map(num_vars);
    let order = VarOrder::linear_order(num_vars);
    // println!("order: {}", order);
    // println!("map: {:?}", map);

    let builders: Vec<_> = (0..num_splits)
        .map(|_| {
            SemanticBddBuilder::<{ primes::U64_LARGEST }>::new_with_map(order.clone(), map.clone())
        })
        .collect();

    let mut ptrs = Vec::new();

    let mut timings = Vec::new();

    for (i, subcnf) in split_cnf(cnf, num_splits).iter().enumerate() {
        let start = Instant::now();
        ptrs.push(builders[i].compile_cnf(subcnf));
        let end = start.elapsed();
        timings.push(end);
    }

    let compile_duration: Duration = timings.iter().sum();
    let compile_max = timings.iter().max().unwrap();

    println!("DONE COMPILING: {:.4}s", compile_duration.as_secs_f64());
    println!("MAX COMPILATION: {:.4}s", compile_max.as_secs_f64());

    let start = Instant::now();

    let builder = &builders[0];
    let mut ptr = ptrs[0];

    for i in 1..num_splits {
        let new_ptr = builder.merge_from(&builders[i], &[ptrs[i]])[0];
        ptr = builder.and(ptr, new_ptr);
    }

    let merge_duration = start.elapsed();

    let st_builder =
        SemanticBddBuilder::<{ primes::U64_LARGEST }>::new_with_map(order.clone(), map.clone());

    let start = Instant::now();
    let st_ptr = st_builder.compile_cnf(cnf);
    let single_duration = start.elapsed();

    let wmc = ptr.wmc(&order, &map);
    let st_wmc = st_ptr.wmc(&order, &map);

    println!("=== TIMING ===");
    println!(
        "Compile: {:4}s (Total {:4}s), Merge: {:4}s",
        compile_max.as_secs_f64(),
        compile_duration.as_secs_f64(),
        merge_duration.as_secs_f64()
    );
    println!("Single-threaded: {:4}s", single_duration.as_secs_f64());
    println!(
        "Speedup ratio: {:4}x",
        single_duration.as_secs_f64() / (compile_max.as_secs_f64() + merge_duration.as_secs_f64())
    );
    if wmc != st_wmc {
        println!(
            "BROKEN. Not equal WMC; single: {}, merge: {}",
            st_wmc.value(),
            wmc.value()
        );
    }
}

fn main() {
    let args = Args::parse();

    let cnf_input = fs::read_to_string(args.file).expect("Should have been able to read the file");

    let cnf = Cnf::from_dimacs(&cnf_input);

    single_threaded(&cnf, args.num_splits)
}
