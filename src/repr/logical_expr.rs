//! A representation of an arbitrary logical formula

use crate::repr::var_label::VarLabel;
use dimacs::*;
use rand;
use rand::rngs::ThreadRng;
use rand::Rng;
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LogicalExpr {
    Literal(usize, bool),
    Not(Box<LogicalExpr>),
    And(Box<LogicalExpr>, Box<LogicalExpr>),
    Or(Box<LogicalExpr>, Box<LogicalExpr>),
    Iff(Box<LogicalExpr>, Box<LogicalExpr>),
    Xor(Box<LogicalExpr>, Box<LogicalExpr>),
    Ite {
        guard: Box<LogicalExpr>,
        thn: Box<LogicalExpr>,
        els: Box<LogicalExpr>,
    },
}

impl LogicalExpr {
    /// Parses a CNF string into a logical expr from DIMACS
    /// String is of the form:
    /// p cnf 185 425
    /// 19 -54 0
    /// 54 -19 0
    /// -54 37 0
    /// ...
    /// Where negative indicates a false variable, 0 is line end
    pub fn from_dimacs(input: String) -> LogicalExpr {
        let r = parse_dimacs(&input).unwrap();
        let (_, cvec) = match r {
            Instance::Cnf { num_vars, clauses } => (num_vars, clauses),
            _ => panic!(),
        };
        let mut clause_vec: Vec<LogicalExpr> = Vec::new();
        for itm in cvec.iter() {
            let mut lit_vec: Vec<LogicalExpr> = Vec::new();
            for l in itm.lits().iter() {
                let b = match l.sign() {
                    Sign::Neg => false,
                    Sign::Pos => true,
                };
                lit_vec.push(LogicalExpr::Literal(l.var().to_u64() as usize, b));
            }
            if lit_vec.len() == 1 {
                clause_vec.push(lit_vec.pop().unwrap());
            } else {
                let mut clause = lit_vec.pop().unwrap();
                for lit in lit_vec {
                    clause = LogicalExpr::Or(Box::new(clause), Box::new(lit));
                }
                clause_vec.push(clause);
            }
        }
        if clause_vec.len() == 1 {
            clause_vec.pop().unwrap()
        } else {
            let mut e = clause_vec.pop().unwrap();
            for clause in clause_vec {
                e = LogicalExpr::And(Box::new(e), Box::new(clause))
            }
            e
        }
    }

    /// Build a random CNF expression
    pub fn rand_cnf(rng: &mut ThreadRng, num_vars: usize, num_clauses: usize) -> LogicalExpr {
        assert!(num_clauses > 2, "requires at least 2 clauses in CNF");
        let vars: Vec<LogicalExpr> = (1..num_vars)
            .map(|x| LogicalExpr::Literal(x, rand::random()))
            .collect();
        // let range = rand::distributions::Range::new(0, vars.len());
        let clause_size = 3;
        // we generate a random cnf
        let mut clause_vec: Vec<LogicalExpr> = Vec::new();
        for _ in 0..num_clauses {
            let num_vars = clause_size;
            if num_vars > 1 {
                let mut var_vec: Vec<LogicalExpr> = Vec::new();
                for _ in 0..clause_size {
                    let var = vars.get(rng.gen_range(0..vars.len())).unwrap().clone();
                    var_vec.push(var);
                }
                let l1 = var_vec.pop().unwrap();
                let l2 = var_vec.pop().unwrap();
                let new_expr = var_vec
                    .into_iter()
                    .fold(LogicalExpr::Or(Box::new(l1), Box::new(l2)), |itm, acc| {
                        LogicalExpr::Or(Box::new(itm), Box::new(acc))
                    });
                clause_vec.push(new_expr);
            } else {
                let var = vars.get(rng.gen_range(0..vars.len())).unwrap().clone();
                clause_vec.push(var);
            }
        }
        let l = clause_vec.pop().unwrap();
        let r = clause_vec.pop().unwrap();
        clause_vec
            .into_iter()
            .fold(LogicalExpr::And(Box::new(l), Box::new(r)), |acc, itm| {
                LogicalExpr::And(Box::new(itm), Box::new(acc))
            })
    }

    /// Evaluates a boolean expression
    pub fn eval(&self, values: &HashMap<VarLabel, bool>) -> bool {
        match self {
            &LogicalExpr::Literal(lbl, polarity) => {
                let v = match values.get(&(VarLabel::new(lbl as u64))) {
                    None => panic!("Variable {} not found in varset", lbl),
                    Some(a) => a,
                };
                if polarity {
                    *v
                } else {
                    !*v
                }
            }
            &LogicalExpr::Not(ref l) => {
                let v = (*l).eval(values);
                !v
            }
            &LogicalExpr::And(ref l, ref r) => {
                let l_v = (*l).eval(values);
                let r_v = (*r).eval(values);
                l_v && r_v
            }
            &LogicalExpr::Or(ref l, ref r) => {
                let l_v = (*l).eval(values);
                let r_v = (*r).eval(values);
                l_v || r_v
            }
            &LogicalExpr::Iff(ref l, ref r) => {
                let l_v = (*l).eval(values);
                let r_v = (*r).eval(values);
                l_v == r_v
            }
            &LogicalExpr::Xor(ref l, ref r) => {
                let l_v = (*l).eval(values);
                let r_v = (*r).eval(values);
                (!l_v && r_v) || (l_v && !r_v)
            }
            &LogicalExpr::Ite {
                ref guard,
                ref thn,
                ref els,
            } => {
                let g = (*guard).eval(values);
                let thn = (*thn).eval(values);
                let els = (*els).eval(values);
                if g {
                    thn
                } else {
                    els
                }
            }
        }
    }
}
