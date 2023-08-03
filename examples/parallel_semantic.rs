use std::{
    fs,
    time::{Duration, Instant},
};

use clap::Parser;
use rayon::prelude::*;
use rsdd::{
    builder::{parallel::SemanticBddBuilder, BottomUpBuilder},
    constants::primes,
    repr::{create_semantic_hash_map, Cnf, DDNNFPtr},
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

    /// use multithreading!
    #[clap(short, long, value_parser)]
    thread: bool,
}

fn split_cnf(cnf: &Cnf, num_splits: usize) -> Vec<Cnf> {
    let chunk_size = cnf.clauses().len() / num_splits
        + (if cnf.clauses().len() % num_splits == 0 {
            0
        } else {
            1
        });

    cnf.clauses().chunks(chunk_size).map(Cnf::new).collect()
}

fn single_threaded(cnf: &Cnf, num_splits: usize) {
    let num_splits = std::cmp::min(num_splits, cnf.clauses().len());

    let num_vars = cnf.num_vars();
    let map = create_semantic_hash_map(num_vars);
    let order = cnf.min_fill_order();

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

    println!("DONE COMPILING: {:.2}s", compile_duration.as_secs_f64());
    println!("MAX COMPILATION: {:.2}s", compile_max.as_secs_f64());

    let start = Instant::now();

    let builder = &builders[0];
    let mut ptr = ptrs[0];

    for i in 1..ptrs.len() {
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
        "Compile: {:.2}s (Total {:.2}s), Merge: {:.2}s",
        compile_max.as_secs_f64(),
        compile_duration.as_secs_f64(),
        merge_duration.as_secs_f64()
    );
    println!("Single-threaded: {:.2}s", single_duration.as_secs_f64());
    println!(
        "Speedup ratio: {:.2}x",
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

fn multi_threaded(cnf: &Cnf, num_splits: usize) {
    let num_splits = std::cmp::min(num_splits, cnf.clauses().len());

    let num_vars = cnf.num_vars();
    let map = create_semantic_hash_map(num_vars);
    let order = cnf.min_fill_order();

    let builders: Vec<_> = split_cnf(cnf, num_splits)
        .into_par_iter()
        .map(|subcnf| {
            (
                SemanticBddBuilder::<{ primes::U64_LARGEST }>::new_with_map(
                    order.clone(),
                    map.clone(),
                ),
                subcnf,
            )
        })
        .collect();

    let start = Instant::now();

    let ptrs: Vec<_> = builders
        .par_iter()
        .map(|(builder, cnf)| builder.compile_cnf(cnf))
        .collect();

    let compile_duration: Duration = start.elapsed();

    println!("DONE COMPILING: {:.2}s", compile_duration.as_secs_f64());

    let mut merge_ds = 0.0;
    let mut merge_and = 0.0;

    let builder = &builders[0].0;
    let mut ptr = ptrs[0];

    for i in 1..ptrs.len() {
        let start = Instant::now();
        let new_ptr = builder.merge_from(&builders[i].0, &[ptrs[i]])[0];
        merge_ds += start.elapsed().as_secs_f64();

        let start = Instant::now();
        ptr = builder.and(ptr, new_ptr);
        merge_and += start.elapsed().as_secs_f64();
        println!("completed one AND; total time spent: {:.2}s", merge_and);
    }

    let merge_duration = merge_ds + merge_and;

    let st_builder =
        SemanticBddBuilder::<{ primes::U64_LARGEST }>::new_with_map(order.clone(), map.clone());

    let start = Instant::now();
    let st_ptr = st_builder.compile_cnf(cnf);
    let single_duration = start.elapsed();

    let wmc = ptr.wmc(&order, &map);
    let st_wmc = st_ptr.wmc(&order, &map);

    println!("=== TIMING ===");
    println!(
        "Compile: {:.2}s, Merge: {:.2}s (ds: {:.2}s, ands: {:.2}s)",
        compile_duration.as_secs_f64(),
        merge_duration,
        merge_ds,
        merge_and
    );
    println!("Single-threaded: {:.2}s", single_duration.as_secs_f64());
    println!(
        "Speedup ratio: {:.2}x",
        single_duration.as_secs_f64() / (compile_duration.as_secs_f64() + merge_duration)
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

    match args.thread {
        false => single_threaded(&cnf, args.num_splits),
        true => multi_threaded(&cnf, args.num_splits),
    }
}
