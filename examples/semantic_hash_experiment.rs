extern crate rsdd;

use clap::Parser;
use rsdd::{
    builder::{
        sdd::{
            builder::SddBuilder, compression::CompressionSddBuilder, semantic::SemanticSddBuilder,
        },
        BottomUpBuilder,
    },
    constants::primes,
    repr::{
        cnf::Cnf, ddnnf::DDNNFPtr, dtree::DTree, sdd::SddPtr, var_label::VarLabel,
        var_order::VarOrder, vtree::VTree,
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
    num_distinct_nodes: usize,
    num_nodes_alloc: usize,
    num_recursive_calls: usize,
    num_get_or_insert_bdd: usize,
    num_get_or_insert_sdd: usize,
    app_cache_rate: f32,
    app_cache_size: usize,
    num_compr: usize,
}

impl BenchStats {
    pub fn from_run<'a>(
        label: String,
        time: Duration,
        sdd: &SddPtr,
        builder: &impl SddBuilder<'a>,
    ) -> BenchStats {
        let stats = builder.stats();
        BenchStats {
            label,
            time,
            num_distinct_nodes: sdd.count_nodes(),
            num_nodes_alloc: builder.node_iter().len(),
            num_recursive_calls: stats.num_recursive_calls,
            num_get_or_insert_bdd: stats.num_get_or_insert_bdd,
            num_get_or_insert_sdd: stats.num_get_or_insert_sdd,
            app_cache_rate: stats.app_cache_hits as f32 / stats.num_recursive_calls as f32 * 100.0,
            app_cache_size: stats.app_cache_size,
            num_compr: stats.num_compressions,
        }
    }
}

impl Display for BenchStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: {:05} nodes | {:06} nodes alloc | {:05} num recur | {:05} g/i | app cache: {:05} hits, {:.1}% recur | {:05} #c | t: {:?}",
            self.label,
            self.num_distinct_nodes,
            self.num_nodes_alloc,
            self.num_recursive_calls,
            self.num_get_or_insert_bdd + self.num_get_or_insert_sdd,
            self.app_cache_size,
            self.app_cache_rate,
            self.num_compr,
            self.time,
        )
    }
}

fn run_compr_sem(cnf: &Cnf, vtree: &VTree) -> (BenchStats, BenchStats) {
    let start = Instant::now();
    let compr_builder = CompressionSddBuilder::new(vtree.clone());
    let compr_cnf = compr_builder.compile_cnf(cnf);
    let compr = BenchStats::from_run("c".to_owned(), start.elapsed(), &compr_cnf, &compr_builder);

    let start = Instant::now();
    let sem_builder = SemanticSddBuilder::<{ primes::U32_SMALL }>::new(vtree.clone());
    let sem_cnf = sem_builder.compile_cnf(cnf);
    let sem = BenchStats::from_run("s".to_owned(), start.elapsed(), &sem_cnf, &sem_builder);

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

    let mut avg_rec_sem = 0;
    let mut avg_rec_compr = 0;

    for _ in 0..num {
        let vtree = VTree::rand_split(order, bias);

        let (compr, sem) = run_compr_sem(&cnf, &vtree);
        println!(
            "c/s: {:.2}x nodes ({:.2}x b+sdd) | {:.2}x rec | {:.2}x g/i | | {:.2}x %app hits | {:.2}x % app size | r% {:.2}",
            sem.num_distinct_nodes as f32 / compr.num_distinct_nodes as f32,
            (sem.num_nodes_alloc) as f32 / (compr.num_nodes_alloc) as f32,
            sem.num_recursive_calls as f32 / compr.num_recursive_calls as f32,
            (sem.num_get_or_insert_bdd + sem.num_get_or_insert_sdd) as f32 / (compr.num_get_or_insert_bdd + compr.num_get_or_insert_sdd) as f32,
            sem.app_cache_rate / compr.app_cache_rate,
            sem.app_cache_size as f32 / compr.app_cache_size as f32,
            vtree_rightness(&vtree)
        );

        avg_nodes_cnf_sem += sem.num_distinct_nodes;
        avg_nodes_cnf_compr += compr.num_distinct_nodes;

        avg_rec_sem += sem.num_recursive_calls;
        avg_rec_compr += compr.num_recursive_calls;
    }
    println!("---");
    println!(
        "total c/s (n={}): {:.2}x nodes | {:.2}x rec",
        num,
        (avg_nodes_cnf_sem as f32 / avg_nodes_cnf_compr as f32),
        (avg_rec_sem as f32 / avg_rec_compr as f32)
    );
}

fn run_canonicalizer_experiment(c: Cnf, vtree: VTree, verbose: bool) {
    let start = Instant::now();

    let compr_builder = CompressionSddBuilder::new(vtree.clone());
    let compr_cnf = compr_builder.compile_cnf(&c);

    println!(" ");

    println!(
        "{}",
        BenchStats::from_run("c".to_owned(), start.elapsed(), &compr_cnf, &compr_builder)
    );

    let start = Instant::now();

    let sem_builder = SemanticSddBuilder::<{ primes::U64_LARGEST }>::new(vtree);
    let sem_cnf = sem_builder.compile_cnf(&c);

    println!(
        "{}\n",
        BenchStats::from_run("s".to_owned(), start.elapsed(), &sem_cnf, &sem_builder)
    );

    if !sem_builder.eq(compr_cnf, sem_cnf) {
        println!(" ");
        println!("not equal! test is broken...");
        println!("compr_cnf: {}", sem_builder.cached_semantic_hash(compr_cnf));
        println!("sem_cnf: {}", sem_builder.cached_semantic_hash(sem_cnf));
        println!("map: {:#?}", sem_builder.map());
        if verbose {
            eprintln!("=== COMPRESSED CNF ===");
            eprintln!("{}", sem_builder.print_sdd(compr_cnf));
            eprintln!("=== SEMANTIC CNF ===");
            eprintln!("{}", sem_builder.print_sdd(sem_cnf));
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

    println!(
        "num vars: {} | num clauses: {}",
        cnf.num_vars(),
        cnf.clauses().len()
    );

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
