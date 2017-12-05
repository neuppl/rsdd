use bdd;
use std::collections::{HashMap, HashSet};
use manager;

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
    let mut split = input.lines();
    split.next(); // skip first line
    let mut expr_vec: Vec<BoolExpr> = Vec::new();
    let mut cur_itm = split.next();
    while cur_itm.is_some() {
        let count = cur_itm.unwrap().split_whitespace().count();
        let mut vars: Vec<BoolExpr> = cur_itm
            .unwrap()
            .split_whitespace()
            .map(|itm| {
                let v = itm.parse::<i32>().unwrap();
                if v < 0 {
                    BoolExpr::Var(-v as usize, false)
                } else {
                    BoolExpr::Var(v as usize, true)
                }
            })
            .collect();
        let e = match count {
            0 | 1 => panic!("empty clause"),
            2 => vars.pop().unwrap(),
            _ => {
                let l = vars.pop().unwrap();
                let r = vars.pop().unwrap();
                vars.into_iter().fold(
                    BoolExpr::Or(Box::new(l), Box::new(r)),
                    |itm, acc| match itm {
                        BoolExpr::Var(0, _) => acc,
                        _ => BoolExpr::Or(Box::new(itm), Box::new(acc)),
                    },
                )
            }
        };
        expr_vec.push(e);
        cur_itm = split.next();
    }
    match expr_vec.len() {
        0 => panic!("empty cnf"),
        1 => expr_vec.pop().unwrap(),
        _ => {
            let l = expr_vec.pop().unwrap();
            let r = expr_vec.pop().unwrap();
            expr_vec.into_iter().fold(
                BoolExpr::And(Box::new(l), Box::new(r)),
                |itm, acc| {
                    BoolExpr::And(Box::new(itm), Box::new(acc))
                },
            )
        }
    }
}

use rand;
use rand::distributions::{IndependentSample, Range};
use rand::{SeedableRng, StdRng};
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
    pub fn into_bdd(&self, man: &mut manager::BddManager) -> () {
        match self {
            &BoolExpr::Var(lbl, polarity) => {
                man.var(bdd::VarLabel::new(lbl as u64), polarity);
            }
            &BoolExpr::And(ref l, ref r) => {
                (*l).into_bdd(man);
                (*r).into_bdd(man);
                man.apply(bdd::Op::BddAnd);
            }
            &BoolExpr::Or(ref l, ref r) => {
                (*l).into_bdd(man);
                (*r).into_bdd(man);
                man.apply(bdd::Op::BddOr);
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

