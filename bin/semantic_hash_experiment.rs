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
    /// An input CNF
    #[clap(short, long, value_parser)]
    file: String,

    /// use left linear vtree
    #[clap(short, long, value_parser, default_value_t = false)]
    left_linear: bool,

    /// use even split
    #[clap(short, long, value_parser, default_value_t = 0)]
    even_split: u8,
}

fn run_canonicalizer_experiment(c: Cnf, vtree: VTree) {
    let start = Instant::now();

    let mut compr_mgr = SddManager::<CompressionCanonicalizer>::new(vtree.clone());
    let compr_cnf = compr_mgr.from_cnf(&c);

    let cduration = start.elapsed();
    println!(" ");

    let stats = compr_mgr.get_stats();
    println!(
        "c: {:05} nodes | {:06} uniq | {:07} rec | {} g/i | {}/{} compr/and",
        compr_cnf.num_nodes(),
        compr_mgr.canonicalizer().bdd_num_uniq() + compr_mgr.canonicalizer().sdd_num_uniq(),
        stats.num_rec,
        stats.num_get_or_insert,
        stats.num_compr,
        stats.num_compr_and,
    );

    let start = Instant::now();

    // TODO: make the prime a CLI arg / iterable?
    // 18,446,744,073,709,551,616 - 25
    // let mut sem_mgr = SddManager::<SemanticCanonicalizer<18_446_744_073_709_551_591>>::new(vtree);
    // let mut sem_mgr = SddManager::<SemanticCanonicalizer<100000049>>::new(vtree);
    let mut sem_mgr = SddManager::<SemanticCanonicalizer<479001599>>::new(vtree);
    let sem_cnf = sem_mgr.from_cnf(&c);

    let duration = start.elapsed();

    let stats = sem_mgr.get_stats();
    println!(
        "s: {:05} nodes | {:06} uniq | {:07} rec | {} g/i",
        sem_cnf.num_nodes(),
        sem_mgr.canonicalizer().bdd_num_uniq() + sem_mgr.canonicalizer().sdd_num_uniq(),
        stats.num_rec,
        stats.num_get_or_insert,
    );

    println!(" ");
    println!("c time: {:?}", cduration);
    println!("s time: {:?}", duration);
    println!(" ");

    sem_mgr.dump_sdd_state(sem_cnf);
    if !sem_mgr.sdd_eq(compr_cnf, sem_cnf) {
        println!(" ");
        println!("not equal! test is broken...");
        eprintln!("=== COMPRESSED CNF ===");
        eprintln!("{}", sem_mgr.print_sdd(compr_cnf));
        eprintln!("=== SEMANTIC CNF ===");
        eprintln!("{}", sem_mgr.print_sdd(sem_cnf));
    }
}

fn main() {
    let args = Args::parse();

    let cnf_input = fs::read_to_string(args.file).expect("Should have been able to read the file");

    let cnf = Cnf::from_file(cnf_input);
    println!("num vars: {}", cnf.num_vars());
    println!(
        "vtree: {}",
        if args.left_linear {
            "left linear".to_string()
        } else if args.even_split > 0 {
            format!("even_split({0})", args.even_split)
        } else {
            "right linear".to_string()
        }
    );

    let range: Vec<usize> = (0..cnf.num_vars() + 1).collect();
    let binding = range
        .iter()
        .map(|i| VarLabel::new(*i as u64))
        .collect::<Vec<VarLabel>>();
    let vars = binding.as_slice();

    let vtree = if args.left_linear {
        VTree::left_linear(vars)
    } else if args.even_split > 0 {
        VTree::even_split(vars, args.even_split.into())
    } else {
        VTree::right_linear(vars)
    };

    run_canonicalizer_experiment(cnf, vtree);
}
