extern crate rsdd;

use clap::Parser;
use rsdd::{
    builder::{
        sdd::{
            builder::SddBuilder, compression::CompressionSddBuilder, semantic::SemanticSddBuilder,
        },
        BottomUpBuilder,
    },
    repr::{
        cnf::Cnf, dtree::DTree, sdd::SddPtr, var_label::VarLabel, var_order::VarOrder, vtree::VTree,
    },
};
use std::{
    fmt::Display,
    fs,
    time::{Duration, Instant},
};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// input CNF in DIMACS form
    #[clap(short, long, value_parser)]
    file: String,

    /// compare random vtrees, generating a random number
    #[clap(short, long, value_parser, default_value_t = 0)]
    run_random_vtrees: usize,

    /// how right-biased should random vtrees be? 1 is right-linear, 0 is even split
    #[clap(long, value_parser, default_value_t = 0.75)]
    right_bias: f64,

    /// use dtree heuristic (min fill)
    #[clap(short, long, value_parser, default_value_t = false)]
    dtree: bool,

    /// use dtree heuristic (linear order)
    #[clap(long, value_parser, default_value_t = false)]
    dtree_linear: bool,

    /// use left linear vtree
    #[clap(long, value_parser, default_value_t = false)]
    left_linear: bool,

    /// use even split; specify number of splits until right-linear
    #[clap(short, long, value_parser, default_value_t = 0)]
    even_split: u8,

    /// print SDD information to stderr (it's very large!)
    #[clap(short, long, value_parser, default_value_t = false)]
    verbose: bool,
}

struct BenchStats {
    label: String,
    time: Duration,
    num_nodes_cnf: usize,
    num_nodes_alloc: usize,
    // num_nodes_bdd: usize,
    // num_nodes_sdd: usize,
    // num_rec: usize,
    // num_get_or_insert: usize,
    // brt_cache_rate: f32,
    // app_cache_rate: f32,
    // num_compr: usize,
    // num_compr_and: usize,
}

impl BenchStats {
    pub fn from_run<'a>(
        label: String,
        time: Duration,
        sdd: &SddPtr,
        mgr: &impl SddBuilder<'a>,
    ) -> BenchStats {
        // let stats = mgr.stats();
        BenchStats {
            label,
            time,
            num_nodes_cnf: sdd.num_child_nodes(),
            num_nodes_alloc: mgr.node_iter().len(),
            // num_rec: stats.num_rec,
            // num_get_or_insert: stats.num_get_or_insert,
            // brt_cache_rate: (mgr.num_app_cache_hits()) as f32 / (stats.num_get_or_insert as f32)
            //     * 100.0,
            // app_cache_rate: stats.num_app_cache_hits as f32 / stats.num_rec as f32 * 100.0,
            // num_compr: stats.num_compr,
            // num_compr_and: stats.num_compr_and,
        }
    }
}

impl Display for BenchStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // write!(f, "{}: {:05} nodes | {:06} bdd / {:06} sdd uniq | {:07} rec | {:06} g/i, {:.1}% brt cache | {:.1}% app cache | {:05}/{:05} compr/and | t: {:?}",
        // write!(f, "{}: {:05} nodes | {:06} nodes alloc | {:07} rec | {:06} g/i, {:.1}% brt cache | {:.1}% app cache | {:05}/{:05} compr/and | t: {:?}",
        write!(
            f,
            "{}: {:05} nodes | {:06} nodes alloc | t: {:?}",
            self.label,
            self.num_nodes_cnf,
            // self.num_nodes_bdd,
            // self.num_nodes_sdd,
            self.num_nodes_alloc,
            // self.num_rec,
            // self.num_get_or_insert,
            // self.brt_cache_rate,
            // self.app_cache_rate,
            // self.num_compr,
            // self.num_compr_and,
            self.time,
        )
    }
}

fn run_compr_sem(cnf: &Cnf, vtree: &VTree) -> (BenchStats, BenchStats) {
    let start = Instant::now();
    let compr_mgr = CompressionSddBuilder::new(vtree.clone());
    let compr_cnf = compr_mgr.from_cnf(cnf);
    let compr = BenchStats::from_run("c".to_owned(), start.elapsed(), &compr_cnf, &compr_mgr);

    let start = Instant::now();
    let sem_mgr = SemanticSddBuilder::<479001599>::new(vtree.clone());
    let sem_cnf = sem_mgr.from_cnf(cnf);
    let sem = BenchStats::from_run("s".to_owned(), start.elapsed(), &sem_cnf, &sem_mgr);

    (compr, sem)
}

fn vtree_rightness(vtree: &VTree) -> f32 {
    fn helper(vtree: &VTree) -> usize {
        if vtree.is_leaf() {
            return 0;
        }
        let count = if vtree.left().is_leaf() { 1 } else { 0 };
        return count + helper(vtree.left()) + helper(vtree.right());
    }
    (helper(vtree) - 1) as f32 / (vtree.num_vars() - 2) as f32
}

fn run_random_comparisons(cnf: Cnf, order: &[VarLabel], num: usize, bias: f64) {
    println!("---");

    let mut avg_nodes_cnf_sem = 0;
    let mut avg_nodes_cnf_compr = 0;

    // let mut avg_rec_sem = 0;
    // let mut avg_rec_compr = 0;

    for _ in 0..num {
        let vtree = VTree::rand_split(order, bias);

        let (compr, sem) = run_compr_sem(&cnf, &vtree);
        println!(
            // "c/s: {:.2}x nodes ({:.2}x b+sdd) | {:.2}x rec | {:.2}x g/i | {:.2}x %brt | {:.2}x %app | r% {:.2}",
            "c/s: {:.2}x nodes ({:.2}x b+sdd) | r% {:.2}",
            sem.num_nodes_cnf as f32 / compr.num_nodes_cnf as f32,
            // (sem.num_nodes_bdd + sem.num_nodes_sdd) as f32
            //     / (compr.num_nodes_bdd + compr.num_nodes_sdd) as f32,
            (sem.num_nodes_alloc) as f32 / (compr.num_nodes_alloc) as f32,
            // sem.num_rec as f32 / compr.num_rec as f32,
            // sem.num_get_or_insert as f32 / compr.num_get_or_insert as f32,
            // sem.brt_cache_rate / compr.brt_cache_rate,
            // sem.app_cache_rate / compr.app_cache_rate,
            vtree_rightness(&vtree)
        );

        avg_nodes_cnf_sem += sem.num_nodes_cnf;
        avg_nodes_cnf_compr += compr.num_nodes_cnf;

        // avg_rec_sem += sem.num_rec;
        // avg_rec_compr += compr.num_rec;
    }
    println!("---");
    println!(
        // "total c/s (n={}): {:.2}x nodes | {:.2}x rec",
        "total c/s (n={}): {:.2}x nodes",
        num,
        (avg_nodes_cnf_sem as f32 / avg_nodes_cnf_compr as f32),
        // (avg_rec_sem as f32 / avg_rec_compr as f32)
    );
}

fn run_canonicalizer_experiment(c: Cnf, vtree: VTree, verbose: bool) {
    let start = Instant::now();

    let compr_mgr = CompressionSddBuilder::new(vtree.clone());
    let compr_cnf = compr_mgr.from_cnf(&c);

    println!(" ");

    println!(
        "{}",
        BenchStats::from_run("c".to_owned(), start.elapsed(), &compr_cnf, &compr_mgr)
    );

    let start = Instant::now();

    // other primes: 100000049, 18_446_744_073_709_551_591
    let sem_mgr = SemanticSddBuilder::<18_446_744_073_709_551_591>::new(vtree);
    let sem_cnf = sem_mgr.from_cnf(&c);

    println!(
        "{}\n",
        BenchStats::from_run("s".to_owned(), start.elapsed(), &sem_cnf, &sem_mgr)
    );

    if !sem_mgr.eq(compr_cnf, sem_cnf) {
        println!(" ");
        println!("not equal! test is broken...");
        if verbose {
            eprintln!("=== COMPRESSED CNF ===");
            eprintln!("{}", sem_mgr.print_sdd(compr_cnf));
            eprintln!("=== SEMANTIC CNF ===");
            eprintln!("{}", sem_mgr.print_sdd(sem_cnf));
        }
    }
}

fn main() {
    let args = Args::parse();

    let cnf_input = fs::read_to_string(args.file).expect("Should have been able to read the file");

    let cnf = Cnf::from_file(cnf_input);
    let range: Vec<usize> = (0..cnf.num_vars()).collect();
    let binding = range
        .iter()
        .map(|i| VarLabel::new(*i as u64))
        .collect::<Vec<VarLabel>>();
    let vars = binding.as_slice();

    println!("num vars: {}", cnf.num_vars());

    if args.run_random_vtrees > 0 {
        println!(
            "random search (right-bias {}), n={}",
            args.right_bias, args.run_random_vtrees
        );
        run_random_comparisons(cnf, vars, args.run_random_vtrees, args.right_bias);
        return;
    }

    let vtree = if args.left_linear {
        println!("left linear");
        VTree::left_linear(vars)
    } else if args.even_split > 0 {
        println!("even_split({0})", args.even_split);
        VTree::even_split(vars, args.even_split.into())
    } else if args.dtree {
        println!("from dtree (min-fill)");
        let dtree = DTree::from_cnf(&cnf, &cnf.min_fill_order());
        VTree::from_dtree(&dtree).unwrap()
    } else if args.dtree_linear {
        println!("from dtree (linear)");
        let dtree = DTree::from_cnf(&cnf, &VarOrder::linear_order(cnf.num_vars()));
        VTree::from_dtree(&dtree).unwrap()
    } else {
        VTree::right_linear(vars)
    };

    run_canonicalizer_experiment(cnf, vtree, args.verbose);
}
