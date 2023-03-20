extern crate rsdd;

use clap::Parser;
use rsdd::{
    builder::{
        canonicalize::{
            CompressionCanonicalizer, SddCanonicalizationScheme, SemanticCanonicalizer,
        },
        sdd_builder::SddManager,
    },
    repr::{cnf::Cnf, var_label::VarLabel, vtree::VTree},
};
use std::fs;
use std::time::Instant;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// An input Bayesian network file in JSON format
    #[clap(short, long, value_parser)]
    file: String,
}

fn run_canonicalizer_experiment(c: Cnf, vtree: VTree) {
    let start = Instant::now();
    println!("creating uncompressed...");

    let mut uncompr_mgr = SddManager::<CompressionCanonicalizer>::new(vtree.clone());
    uncompr_mgr.set_compression(false);
    let uncompr_cnf = uncompr_mgr.from_cnf(&c);

    let duration = start.elapsed();
    println!("time: {:?}", duration);

    let start = Instant::now();
    println!("creating compressed...");

    let mut compr_mgr = SddManager::<CompressionCanonicalizer>::new(vtree.clone());
    let compr_cnf = compr_mgr.from_cnf(&c);

    let duration = start.elapsed();
    println!("time: {:?}", duration);

    let start = Instant::now();
    println!("creating semantic...");

    // 18,446,744,073,709,551,616 - 25
    // TODO: make the prime a CLI arg
    let mut sem_mgr = SddManager::<SemanticCanonicalizer<18_446_744_073_709_551_591>>::new(vtree);
    let sem_cnf = sem_mgr.from_cnf(&c);
    // saving this before we run sdd_eq, which could add nodes to the table
    let sem_uniq = sem_mgr.canonicalizer().bdd_num_uniq() + sem_mgr.canonicalizer().sdd_num_uniq();

    let duration = start.elapsed();
    println!("time: {:?}", duration);

    if !sem_mgr.sdd_eq(uncompr_cnf, sem_cnf) {
        println!("not equal! not continuing with test...");
        println!("uncompr sdd: {}", sem_mgr.print_sdd(uncompr_cnf));
        println!("sem sdd: {}", sem_mgr.print_sdd(sem_cnf));
        return;
    }

    if !sem_mgr.sdd_eq(compr_cnf, sem_cnf) {
        println!("not equal! not continuing with test...");
        println!("compr sdd: {}", sem_mgr.print_sdd(compr_cnf));
        println!("sem sdd: {}", sem_mgr.print_sdd(sem_cnf));
        return;
    }

    println!(
        "uncompr: {} nodes, {} uniq",
        uncompr_cnf.num_nodes(),
        uncompr_mgr.canonicalizer().bdd_num_uniq() + uncompr_mgr.canonicalizer().sdd_num_uniq()
    );

    println!(
        "compr: {} nodes, {} uniq",
        compr_cnf.num_nodes(),
        compr_mgr.canonicalizer().bdd_num_uniq() + compr_mgr.canonicalizer().sdd_num_uniq()
    );

    println!("sem: {} nodes, {} uniq", sem_cnf.num_nodes(), sem_uniq);
}

fn main() {
    let args = Args::parse();

    let cnf_input = fs::read_to_string(args.file).expect("Should have been able to read the file");

    let cnf = Cnf::from_file(cnf_input);
    println!("{}", cnf.num_vars());

    let range: Vec<usize> = (0..cnf.num_vars() + 1).collect();
    let binding = range
        .iter()
        .map(|i| VarLabel::new(*i as u64))
        .collect::<Vec<VarLabel>>();
    let vars = binding.as_slice();

    let vtree = VTree::right_linear(vars);

    run_canonicalizer_experiment(cnf, vtree);
}
