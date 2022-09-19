extern crate criterion;
extern crate rsdd;
extern crate rsgm;

use clap::Parser;
use criterion::black_box;

use std::collections::HashMap;
use std::time::{Duration, Instant};

use rsdd::{
    builder::{dtree::DTree, repr::builder_sdd::VTree, sdd_builder, var_order::VarOrder},
    repr::{
        cnf::Cnf,
        var_label::{Literal, VarLabel},
    },
};
use rsgm::bayesian_network::BayesianNetwork;

/// Test driver for comparing canonicalize implementations
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// An input Bayesian network file in JSON format
    #[clap(short, long, value_parser)]
    file: String,

    /// Number of iterations to compare against, for both types
    #[clap(short, long, value_parser, default_value_t = 4)]
    iterations: u32,
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
    r
}

/// Produce a Chavira-Darwiche encoding of a Bayesian network
fn bn_to_cnf(network: &BayesianNetwork) -> Cnf {
    let mut clauses: Vec<Vec<Literal>> = Vec::new();
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
            cur_indic.push(Literal::new(cur_var, true));
            indicators
                .get_mut(&v)
                .unwrap()
                .insert(varassgn.clone(), cur_var);
            var_count += 1;

            for passgn in network.parent_assignments(&v) {
                let cur_param = VarLabel::new_usize(var_count);
                var_count += 1;

                // build cur_param => cur_assgn /\ cur_indic
                let indic_vec: Vec<Literal> = passgn
                    .iter()
                    .map(|(varname, varval)| {
                        let varlabel = indicators[varname][varval];
                        Literal::new(varlabel, true)
                    })
                    .collect();

                let mut imp1 = implies(&vec![Literal::new(cur_param, true)], &indic_vec);
                let mut imp2 = implies(&indic_vec, &vec![Literal::new(cur_param, false)]);
                clauses.append(&mut imp1);
                clauses.append(&mut imp2);
            }
        }
        // build exactly-one for indicator clause
        let mut exactly1 = exactly_one(cur_indic);
        clauses.append(&mut exactly1);
    }
    Cnf::new(clauses)
}

fn build_dtree(cnf: &Cnf) -> DTree {
    let start = Instant::now();
    let dtree = DTree::from_cnf(cnf, &VarOrder::linear_order(cnf.num_vars()));
    let duration = start.elapsed();
    println!("Dtree built\nNumber of variables: {}\n\tNumber of clauses: {}\n\tWidth: {}\n\tElapsed dtree time: {:?}",
        cnf.num_vars(), cnf.clauses().len(), dtree.width(), duration);
    dtree
}

fn compile_sdd_benchmark(cnf: &Cnf, vtree: VTree, modified: bool) -> Duration {
    let mut compiler = sdd_builder::SddManager::new(vtree);
    compiler.set_compression(!modified);

    let start = Instant::now();
    compiler.from_cnf(cnf);
    // let r = compiler.from_cnf(&cnf);

    // let sz = compiler.count_nodes(r);

    start.elapsed()
}

fn main() {
    let args = Args::parse();

    let bn = BayesianNetwork::from_string(std::fs::read_to_string(args.file).unwrap().as_str());

    let cnf = bn_to_cnf(&bn);

    let dtree = build_dtree(&cnf);

    let mut originals: Vec<f64> = Vec::new();
    let mut modifieds: Vec<f64> = Vec::new();

    // TODO(mattxwang): can we add some sort of equality check here?
    // perhaps evaluate the same WMC on both trees with unsmoothed_wmc,
    // or maybe eval_sdd? not sure
    for _ in 0..args.iterations {
        originals.push(
            compile_sdd_benchmark(black_box(&cnf), dtree.to_vtree().unwrap(), false).as_secs_f64(),
        );
        modifieds.push(
            compile_sdd_benchmark(black_box(&cnf), dtree.to_vtree().unwrap(), true).as_secs_f64(),
        );
    }

    let originals_sum: f64 = originals.iter().sum();
    let og_time = originals_sum / args.iterations as f64;

    let modifieds_sum: f64 = modifieds.iter().sum();
    let md_time = modifieds_sum / args.iterations as f64;

    println!(
        "
Avg original time: {:?}s
Avg modified time: {:?}s
Avg ratio (od/mg): {:?}
  ",
        og_time,
        md_time,
        og_time / md_time
    )
}
