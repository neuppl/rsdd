extern crate rsdd;
extern crate rsgm;

use std::collections::HashMap;

use rsdd::{
    repr::{cnf::Cnf, var_label::{Literal, VarLabel}}, builder::{decision_nnf_builder, var_order::VarOrder, bdd_builder::BddManager}, 
};
use rsgm::bayesian_network::BayesianNetwork;


/// construct a CNF for the two TERMS (i.e., conjunctions of literals) t1 => t2
fn implies(t1: &Vec<Literal>, t2: &Vec<Literal>) -> Vec<Vec<Literal>> {
    let mut r : Vec<Vec<Literal>> = Vec::new();
    // negate the lhs
    let lhs : Vec<Literal> = t1.iter().map(|l| Literal::new(l.get_label(), !l.get_polarity())).collect();
    for v in t2.iter() {
        let mut new_clause = lhs.clone();
        new_clause.push(v.clone());
        r.push(new_clause);
    }
    r
}

/// constructs a CNF constraint where exactly one of `lits` is true
fn exactly_one(lits: Vec<Literal>) -> Vec<Vec<Literal>> {
    let mut r : Vec<Vec<Literal>> = Vec::new();
    // one must be true
    r.push(lits.clone());
    // pairwise constraints
    for x in 0..(lits.len()) {
        for y in (x+1)..(lits.len()) {
            r.push(vec![lits[x].negated(), lits[y].negated()]);
        }
    }
    r
}

/// Produce a Chavira-Darwiche encoding of a Bayesian network
fn bn_to_cnf(network: &BayesianNetwork) -> Cnf {
    let mut clauses : Vec<Vec<Literal>> = Vec::new();
    let mut var_count = 0;

    // create one indicator for every variable assignment
    // maps Variable Name -> (Variable Assignment -> Label)
    let mut indicators : HashMap<String, HashMap<String, VarLabel>> = HashMap::new();

    for v in network.topological_sort() {
        // create this variable's indicators and parameter clauses
        let mut cur_indic : Vec<Literal> = Vec::new();
        indicators.insert(v.clone(), HashMap::new());
        for varassgn in network.get_all_assignments(&v) {
            let cur_var = VarLabel::new_usize(var_count);
            cur_indic.push(Literal::new(cur_var, true));
            indicators.get_mut(&v).unwrap().insert(varassgn.clone(), cur_var);
            var_count += 1;
            
            for passgn in network.parent_assignments(&v) {
                let cur_param = VarLabel::new_usize(var_count);
                var_count += 1;

                // build cur_param => cur_assgn /\ cur_indic
                let indic_vec : Vec<Literal> = passgn.iter().map(|(varname, varval)| {
                    let varlabel = indicators[varname][varval];
                    Literal::new(varlabel, true)
                }).collect();

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

fn main() {
    let bn = BayesianNetwork::from_string(&include_str!("../bayesian_networks/water.json"));
    let cnf = bn_to_cnf(&bn);
    println!("# clauses: {}", cnf.clauses().len());
    // let mut compiler = decision_nnf_builder::DecisionNNFBuilder::new(cnf.num_vars());
    // compiler.from_cnf_topdown(&VarOrder::linear_order(cnf.num_vars()), &cnf);
    let mut compiler = BddManager::new(VarOrder::linear_order(cnf.num_vars()));
    compiler.from_cnf(&cnf);
}