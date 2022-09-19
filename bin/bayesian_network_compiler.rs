extern crate rsdd;
extern crate rsgm;

use clap::Parser;
use rsdd::builder::bdd_builder::BddWmc;
use rsdd::builder::decision_nnf_builder::DecisionNNFBuilder;
use rsdd::builder::repr::builder_bdd::BddPtr;

use std::collections::HashMap;
use std::time::Instant;

use rsdd::{
    builder::{bdd_builder::BddManager, dtree::DTree, sdd_builder, var_order::VarOrder},
    repr::{
        cnf::Cnf,
        var_label::{Literal, VarLabel},
    },
};
use rsgm::bayesian_network::BayesianNetwork;

/// Contains a Bayesian network that was compiled to a CNF
#[derive(Debug, Clone)]
pub struct BayesianNetworkCNF {
    cnf: Cnf,
    /// maps Variable Name -> (Variable Assignment -> Label)
    indicators: HashMap<String, HashMap<String, VarLabel>>,
    params: BddWmc<f64>,
}

impl BayesianNetworkCNF {
    pub fn new(network: &BayesianNetwork) -> BayesianNetworkCNF {
        let mut clauses: Vec<Vec<Literal>> = Vec::new();
        let mut wmc_params: HashMap<VarLabel, (f64, f64)> = HashMap::new();
        let mut var_count = 0;

        // create one indicator for every variable assignment
        // maps Variable Name -> (Variable Assignment -> Label)
        let mut indicators: HashMap<String, HashMap<String, VarLabel>> = HashMap::new();

        for v in network.topological_sort() {
            // create this variable's indicators and parameter clauses
            let mut cur_indic: Vec<Literal> = Vec::new();
            indicators.insert(v.clone(), HashMap::new());
            for varassgn in network.get_all_assignments(&v) {
                let cur_var = VarLabel::new_usize(var_count);
                let new_indic = Literal::new(cur_var, true);
                wmc_params.insert(cur_var, (1.0, 1.0));
                cur_indic.push(new_indic);
                indicators
                    .get_mut(&v)
                    .unwrap()
                    .insert(varassgn.clone(), cur_var);
                var_count += 1;

                for passgn in network.parent_assignments(&v) {
                    let cur_param = VarLabel::new_usize(var_count);
                    let cur_prob = network.get_conditional_prob(&v, varassgn, &passgn);
                    wmc_params.insert(cur_param, (1.0, cur_prob));
                    // println!("w({:?}) = {cur_prob}", cur_param);
                    var_count += 1;

                    // build cur_param <=> cur_assgn /\ cur_indic
                    let mut indic_vec: Vec<Literal> = passgn
                        .iter()
                        .map(|(varname, varval)| {
                            let varlabel = indicators[varname][varval];
                            Literal::new(varlabel, true)
                        })
                        .collect();
                    indic_vec.push(new_indic);

                    println!("{:?} <=> {:?}", cur_param, indic_vec);
                    let mut imp1 = implies(&vec![Literal::new(cur_param, true)], &indic_vec);
                    let mut imp2 = implies(&indic_vec, &vec![Literal::new(cur_param, true)]);
                    clauses.append(&mut imp1);
                    clauses.append(&mut imp2);
                }
            }
            // build exactly-one for indicator clause
            let mut exactly1 = exactly_one(cur_indic);
            clauses.append(&mut exactly1);
        }
        // println!("{:#?}", clauses);
        BayesianNetworkCNF {
            cnf: Cnf::new(clauses),
            indicators,
            params: BddWmc::new_with_default(0.0, 1.0, wmc_params),
        }
    }

    pub fn get_indicator(&self, var: &String, value: &String) -> VarLabel {
        self.indicators[var][value]
    }
}

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// An input Bayesian network file in JSON format
    #[clap(short, long, value_parser)]
    file: String,

    /// The compile mode (either 'bdd' or 'sdd')
    #[clap(short, long, value_parser)]
    mode: String,

    /// The elimination order (either 'topological' or 'force')
    #[clap(short, long, value_parser, default_value = "topological")]
    elim: String,

    /// The type of query to perform. Options: "marginal"
    #[clap(short, long, value_parser)]
    query: Option<String>,

    /// Name of a variable to query
    #[clap(long, value_parser)]
    query_var: Option<String>,

    /// Name of a variable value to query
    #[clap(long, value_parser)]
    query_value: Option<String>,
}

/// construct a CNF for the two TERMS (i.e., conjunctions of literals) t1 => t2
fn implies(t1: &Vec<Literal>, t2: &Vec<Literal>) -> Vec<Vec<Literal>> {
    let mut r: Vec<Vec<Literal>> = Vec::new();
    // negate the lhs
    let lhs: Vec<Literal> = t1
        .iter()
        .map(|l| Literal::new(l.get_label(), !l.get_polarity()))
        .collect();
    for v in t2.iter() {
        let mut new_clause = lhs.clone();
        new_clause.push(*v);
        r.push(new_clause);
    }
    // println!("{:?} => {:?}\n{:?}", t1, t2, r);
    // println!("");
    r
}

/// constructs a CNF constraint where exactly one of `lits` is true
fn exactly_one(lits: Vec<Literal>) -> Vec<Vec<Literal>> {
    let mut r: Vec<Vec<Literal>> = Vec::new();
    // one must be true
    r.push(lits.clone());
    // pairwise constraints
    for x in 0..(lits.len()) {
        for y in (x + 1)..(lits.len()) {
            r.push(vec![lits[x].negated(), lits[y].negated()]);
        }
    }
    // println!("exactly one of {:?}:\n{:?}", lits, r);
    r
}

fn compile_bdd_cnf(args: &Args, network: BayesianNetwork) {
    let bn = BayesianNetworkCNF::new(&network);
    let mut compiler = BddManager::new(VarOrder::linear_order(bn.cnf.num_vars()));

    println!("Compiling...");
    let start = Instant::now();
    let r = compiler.from_cnf(&bn.cnf);
    let duration = start.elapsed();
    let sz = compiler.count_nodes(r);
    println!("Compiled\n\tTime: {:?}\n\tSize: {sz}", duration);

    match &args.query {
        Some(q) if q.eq("marginal") => {
            let query_var = args
                .query_var
                .as_ref()
                .unwrap_or_else(|| panic!("Provide query variable for marginalization"));
            let query_val = args
                .query_value
                .as_ref()
                .unwrap_or_else(|| panic!("Provide query variable value for marginalization"));
            let indic = bn.get_indicator(query_var, query_val);
            // let cond = compiler.condition(r, indic, true);
            let v = compiler.var(indic, true);
            let cond = compiler.and(r, v);
            let p = compiler.wmc(cond, &bn.params);
            let z = compiler.wmc(r, &bn.params);
            println!(
                "Marginal query: Pr({query_var} = {query_val}) = {p}, z = {z}, p / z = {}",
                p / z
            );
        }
        _ => (),
    }
}

fn compile_bdd(_args: &Args, network: BayesianNetwork) {
    let mut compiler = BddManager::new_default_order(1);

    // let mut clauses : Vec<Vec<Literal>> = Vec::new();
    let mut wmc_params: HashMap<VarLabel, (f64, f64)> = HashMap::new();

    // create one indicator for every variable assignment
    // maps Variable Name -> (Variable Assignment -> Label)
    let mut indicators: HashMap<String, HashMap<String, VarLabel>> = HashMap::new();

    let mut cpts: Vec<BddPtr> = Vec::new();

    for v in network.topological_sort() {
        // create this variable's indicators and parameter clauses
        let mut cur_cpt = compiler.true_ptr();

        let mut cur_indic: Vec<Literal> = Vec::new();
        indicators.insert(v.clone(), HashMap::new());
        for varassgn in network.get_all_assignments(&v) {
            let cur_var = compiler.new_var();
            let new_indic = Literal::new(cur_var, true);
            wmc_params.insert(cur_var, (1.0, 1.0));
            cur_indic.push(new_indic);
            indicators
                .get_mut(&v)
                .unwrap()
                .insert(varassgn.clone(), cur_var);

            for passgn in network.parent_assignments(&v) {
                let cur_param = compiler.new_var();
                let cur_param_v = compiler.var(cur_param, true);
                let cur_prob = network.get_conditional_prob(&v, varassgn, &passgn);
                wmc_params.insert(cur_param, (1.0, cur_prob));

                // build cur_param => cur_assgn /\ cur_indic
                let mut indic: BddPtr =
                    passgn
                        .iter()
                        .fold(compiler.true_ptr(), |acc, (varname, varval)| {
                            let varlabel = indicators[varname][varval];
                            let v = compiler.var(varlabel, true);
                            compiler.and(acc, v)
                        });
                let indic_var = compiler.var(cur_var, true);
                indic = compiler.and(indic_var, indic);

                let iff = compiler.iff(indic, cur_param_v);
                cur_cpt = compiler.and(cur_cpt, iff);
            }
        }

        // build exactly-one for indicator clause
        let mut exactly_one = compiler.true_ptr();
        for x in 0..(cur_indic.len()) {
            for y in (x + 1)..(cur_indic.len()) {
                let v1 = compiler.var(cur_indic[x].get_label(), false);
                let v2 = compiler.var(cur_indic[y].get_label(), false);
                let or = compiler.or(v1, v2);
                exactly_one = compiler.and(exactly_one, or);
            }
        }
        let o2 = cur_indic.iter().fold(compiler.false_ptr(), |acc, i| {
            let v = compiler.var(i.get_label(), true);
            compiler.or(acc, v)
        });

        let new = compiler.and(cur_cpt, o2);
        cpts.push(new);
    }
    let r = cpts.iter().fold(compiler.true_ptr(), |acc, cpt| {
        println!(
            "cur size: {}, cpt size: {}",
            compiler.count_nodes(acc),
            compiler.count_nodes(*cpt)
        );
        compiler.and(acc, *cpt)
    });
    println!("final size: {}", compiler.count_nodes(r));
}

fn compile_sdd_cnf(network: BayesianNetwork) {
    println!("############################\n\tCompiling in SDD mode\n############################");
    let bn = BayesianNetworkCNF::new(&network);
    // println!("{}", cnf.to_dimacs());
    println!("Building dtree");
    let start = Instant::now();
    let dtree = DTree::from_cnf(&bn.cnf, &VarOrder::linear_order(bn.cnf.num_vars()));
    let duration = start.elapsed();
    println!("Dtree built\nNumber of variables: {}\n\tNumber of clauses: {}\n\tWidth: {}\n\tElapsed dtree time: {:?}",
        bn.cnf.num_vars(), bn.cnf.clauses().len(), dtree.width(), duration);

    let mut compiler = sdd_builder::SddManager::new(dtree.to_vtree().unwrap());

    println!("Compiling");
    let start = Instant::now();
    let r = compiler.from_cnf(&bn.cnf);
    let duration = start.elapsed();
    let sz = compiler.count_nodes(r);
    println!("Compiled\n\tCompile time: {:?}\n\tSize: {sz}", duration);
}

fn compile_topdown(network: BayesianNetwork) {
    println!("############################\n\tCompiling topdown\n############################");
    let bn = BayesianNetworkCNF::new(&network);
    let mut compiler = DecisionNNFBuilder::new(bn.cnf.num_vars());

    println!("Compiling");
    let start = Instant::now();
    let r = compiler.from_cnf_topdown(&VarOrder::linear_order(bn.cnf.num_vars()), &bn.cnf);
    let duration = start.elapsed();
    let sz = compiler.count_nodes(r);
    println!("Compiled\n\tCompile time: {:?}\n\tSize: {sz}", duration);
}

fn print_dimacs(bn: BayesianNetwork) {
    let bn = BayesianNetworkCNF::new(&bn);
    println!("p cnf {} {}", bn.cnf.clauses().len(), bn.cnf.num_vars());
    println!("{}", bn.cnf.to_dimacs());
}

fn main() {
    let args = Args::parse();
    let bn = BayesianNetwork::from_string(std::fs::read_to_string(&args.file).unwrap().as_str());
    match args.mode.as_str() {
        "sdd" => compile_sdd_cnf(bn),
        "bdd" => compile_bdd(&args, bn),
        "bdd_cnf" => compile_bdd_cnf(&args, bn),
        "topdown" => compile_topdown(bn),
        "print_dimacs" => print_dimacs(bn),
        _ => panic!("unrecognized mode"),
    }
}
