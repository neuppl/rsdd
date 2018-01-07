use bdd;
use std::collections::{HashMap, HashSet};
use manager;
use sdd_manager;
use ref_table::ExternalRef;
use dimacs;

#[derive(Debug, Clone)]
pub enum BoolExpr {
    Var(usize, bool),
    And(Box<BoolExpr>, Box<BoolExpr>),
    Or(Box<BoolExpr>, Box<BoolExpr>),
}

/// Parses a CNF string into a Boolean expression
/// String is of the form:
/// p cnf 185 425
/// 19 -54 0
/// 54 -19 0
/// -54 37 0
/// ...
/// Where negative indicates a false variable, 0 is line end
pub fn parse_cnf(input: String) -> BoolExpr {
    use dimacs::*;
    let r = parse_dimacs(&input).unwrap();
    let (_, cvec) = match r {
        Instance::Cnf { num_vars, clauses } => (num_vars, clauses),
        _ => panic!()
    };
    let mut clause_vec : Vec<BoolExpr> = Vec::new();
    for itm in cvec.iter() {
        let mut lit_vec : Vec<BoolExpr> = Vec::new();
        for l in itm.lits().iter() {
            let b = match l.sign() {
                Sign::Neg => false,
                Sign::Pos => true
            };
            lit_vec.push(BoolExpr::Var(l.var().to_u64() as usize, b));
        }
        if lit_vec.len() == 1 {
            clause_vec.push(lit_vec.pop().unwrap());
        } else {
            let mut clause = lit_vec.pop().unwrap();
            for lit in lit_vec {
                clause = BoolExpr::Or(Box::new(clause), Box::new(lit));
            }
            clause_vec.push(clause);
        }
    }
    if clause_vec.len() == 1 {
        clause_vec.pop().unwrap()
    } else {
        let mut e = clause_vec.pop().unwrap();
        for clause in clause_vec {
            e = BoolExpr::And(Box::new(e), Box::new(clause))
        }
        e
    }
}

use rand;
use rand::distributions::{IndependentSample};
use rand::{StdRng};
/// Build a random CNF expression
pub fn rand_cnf(rng: &mut StdRng, num_vars: usize, num_clauses: usize) -> BoolExpr {
    assert!(num_clauses > 2, "requires at least 2 clauses in CNF");
    let vars: Vec<BoolExpr> = (1..num_vars)
        .map(|x| BoolExpr::Var(x, rand::random()))
        .collect();
    let range = rand::distributions::Range::new(0, vars.len());
    let clause_size = 3;
    // we generate a random cnf
    let mut clause_vec: Vec<BoolExpr> = Vec::new();
    for _ in 0..num_clauses {
        let num_vars = clause_size;
        if num_vars > 1 {
            let mut var_vec: Vec<BoolExpr> = Vec::new();
            for _ in 0..clause_size {
                let var = vars.get(range.ind_sample(rng)).unwrap().clone();
                var_vec.push(var);
            }
            let l1 = var_vec.pop().unwrap();
            let l2 = var_vec.pop().unwrap();
            let new_expr = var_vec.into_iter().fold(
                BoolExpr::Or(Box::new(l1), Box::new(l2)),
                |itm, acc| {
                    BoolExpr::Or(Box::new(itm), Box::new(acc))
                },
            );
            clause_vec.push(new_expr);
        } else {
            let var = vars.get(range.ind_sample(rng)).unwrap().clone();
            clause_vec.push(var);
        }
    }
    let l = clause_vec.pop().unwrap();
    let r = clause_vec.pop().unwrap();
    clause_vec.into_iter().fold(
        BoolExpr::And(
            Box::new(l),
            Box::new(r),
        ),
        |acc, itm| {
            BoolExpr::And(Box::new(itm), Box::new(acc))
        },
    )
}

impl BoolExpr {
    /// Evaluates a boolean expression
    pub fn eval(&self, values: &HashMap<bdd::VarLabel, bool>) -> bool {
        match self {
            &BoolExpr::Var(lbl, polarity) => {
                let v = match values.get(&(bdd::VarLabel::new(lbl as u64))) {
                    None => panic!("Variable {} not found in varset", lbl),
                    Some(a) => a,
                };
                if polarity { *v } else { !*v }
            }
            &BoolExpr::And(ref l, ref r) => {
                let l_v = (*l).eval(values);
                let r_v = (*r).eval(values);
                l_v && r_v
            }
            &BoolExpr::Or(ref l, ref r) => {
                let l_v = (*l).eval(values);
                let r_v = (*r).eval(values);
                l_v || r_v
            }
        }
    }

    /// pushes the BDD onto the top of the manager's stack
    pub fn into_bdd(&self, man: &mut manager::BddManager) -> bdd::BddPtr {
        match self {
            &BoolExpr::Var(lbl, polarity) => {
                man.var(bdd::VarLabel::new(lbl as u64), polarity)
            }
            &BoolExpr::And(ref l, ref r) => {
                let r1 = (*l).into_bdd(man);
                let r2 = (*r).into_bdd(man);
                man.and(r1, r2)
            }
            &BoolExpr::Or(ref l, ref r) => {
                let r1 = (*l).into_bdd(man);
                let r2 = (*r).into_bdd(man);
                man.or(r1, r2)
            }
        }
    }

    /// pushes the BDD onto the top of the manager's stack
    pub fn into_sdd(&self, man: &mut sdd_manager::SddManager) -> ExternalRef {
        match self {
            &BoolExpr::Var(lbl, polarity) => {
                man.var(bdd::VarLabel::new(lbl as u64), polarity)
            }
            &BoolExpr::And(ref l, ref r) => {
                let r1 = (*l).into_sdd(man);
                let r2 = (*r).into_sdd(man);
                // println!("and");
                man.apply(bdd::Op::BddAnd, r1, r2)
            }
            &BoolExpr::Or(ref l, ref r) => {
                let r1 = (*l).into_sdd(man);
                let r2 = (*r).into_sdd(man);
                // println!("or");
                man.apply(bdd::Op::BddOr, r1, r2)
            }
        }
    }



    pub fn varset(&self) -> HashSet<usize> {
        fn traverse(b: &BoolExpr, cur_set: &mut HashSet<usize>) -> () {
            match b {
                &BoolExpr::Var(lbl, _) => {
                    cur_set.insert(lbl);
                }
                &BoolExpr::And(ref l, ref r) |
                &BoolExpr::Or(ref l, ref r) => {
                    traverse(l, cur_set);
                    traverse(r, cur_set);
                }
            }
        }
        let mut r = HashSet::new();
        traverse(self, &mut r);
        r
    }
}

