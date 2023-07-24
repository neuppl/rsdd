//! A representation of an arbitrary logical formula

use crate::{repr::var_label::VarLabel, serialize::LogicalSExpr};
use dimacs::*;
use rand::{self, rngs::ThreadRng, Rng};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
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
    /// ```
    /// use rsdd::repr::logical_expr::LogicalExpr;
    ///
    /// static CNF: &str = r#"
    /// p cnf 3 1
    /// 1 -2 3 0
    /// "#;
    ///
    /// let expr = LogicalExpr::from_dimacs(CNF);
    /// assert!(matches!(expr, LogicalExpr::Or(_, _)));
    /// ```
    pub fn from_dimacs(input: &str) -> LogicalExpr {
        let (_, cvec) = match parse_dimacs(input).unwrap() {
            Instance::Cnf { num_vars, clauses } => (num_vars, clauses),
            Instance::Sat {
                num_vars: _,
                extensions: _,
                formula: _,
            } => panic!("Received (valid) SAT input, not CNF"),
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

    /// ```
    /// use rsdd::repr::logical_expr::LogicalExpr;
    /// use rsdd::serialize::LogicalSExpr;
    ///
    /// // this string represents X XOR Y. it exercises each branch of the match statement
    /// // within the from_sexpr helper
    /// let x_xor_y = String::from("(And (Or (Var X) (Var Y)) (Or (Not (Var X)) (Not (Var Y))))");
    /// let expr = serde_sexpr::from_str::<LogicalSExpr>(&x_xor_y).unwrap();
    ///
    /// let manually_constructed = LogicalExpr::And(
    ///     Box::new(LogicalExpr::Or(
    ///         Box::new(LogicalExpr::Literal(0, true)),
    ///         Box::new(LogicalExpr::Literal(1, true)),
    ///     )),
    ///     Box::new(LogicalExpr::Or(
    ///         Box::new(LogicalExpr::Literal(0, false)),
    ///         Box::new(LogicalExpr::Literal(1, false)),
    ///     )),
    /// );
    ///
    /// assert_eq!(LogicalExpr::from_sexpr(&expr), manually_constructed)
    /// ```
    pub fn from_sexpr(sexpr: &LogicalSExpr) -> LogicalExpr {
        let mapping = sexpr.variable_mapping();

        fn helper(sexpr: &LogicalSExpr, mapping: &HashMap<&String, usize>) -> LogicalExpr {
            match sexpr {
                LogicalSExpr::True => todo!(),
                LogicalSExpr::False => todo!(),
                LogicalSExpr::Var(s) => LogicalExpr::Literal(*mapping.get(s).unwrap(), true),
                LogicalSExpr::Not(l) => match l.as_ref() {
                    LogicalSExpr::Var(s) => LogicalExpr::Literal(*mapping.get(s).unwrap(), false),
                    _ => LogicalExpr::Not(Box::new(helper(l.as_ref(), mapping))),
                },
                LogicalSExpr::Or(l, r) => LogicalExpr::Or(
                    Box::new(helper(l.as_ref(), mapping)),
                    Box::new(helper(r.as_ref(), mapping)),
                ),
                LogicalSExpr::And(l, r) => LogicalExpr::And(
                    Box::new(helper(l.as_ref(), mapping)),
                    Box::new(helper(r.as_ref(), mapping)),
                ),
            }
        }

        helper(sexpr, &mapping)
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

    /// Evaluates a boolean expression.
    /// ```
    /// use rsdd::repr::logical_expr::LogicalExpr;
    /// use rsdd::repr::var_label::VarLabel;
    /// use std::collections::HashMap;
    ///
    /// static CNF: &str = r#"
    /// p cnf 3 1
    /// 1 -2 3 0
    /// "#;
    ///
    /// let expr = LogicalExpr::from_dimacs(CNF);
    ///
    /// assert!(expr.eval(
    ///     &HashMap::from([
    ///         (VarLabel::new(1), true),
    ///         (VarLabel::new(2), true),
    ///         (VarLabel::new(3), true)
    ///     ])
    /// ));
    ///
    /// assert!(!expr.eval(
    ///     &HashMap::from([
    ///         (VarLabel::new(1), false),
    ///         (VarLabel::new(2), true),
    ///         (VarLabel::new(3), false)
    ///     ])
    /// ));
    /// ```
    pub fn eval(&self, values: &HashMap<VarLabel, bool>) -> bool {
        match &self {
            LogicalExpr::Literal(lbl, polarity) => {
                let v = match values.get(&(VarLabel::new(*lbl as u64))) {
                    None => panic!("Variable {} not found in varset", lbl),
                    Some(a) => a,
                };
                if *polarity {
                    *v
                } else {
                    !*v
                }
            }
            LogicalExpr::Not(ref l) => {
                let v = (*l).eval(values);
                !v
            }
            LogicalExpr::And(ref l, ref r) => {
                let l_v = (*l).eval(values);
                let r_v = (*r).eval(values);
                l_v && r_v
            }
            LogicalExpr::Or(ref l, ref r) => {
                let l_v = (*l).eval(values);
                let r_v = (*r).eval(values);
                l_v || r_v
            }
            LogicalExpr::Iff(ref l, ref r) => {
                let l_v = (*l).eval(values);
                let r_v = (*r).eval(values);
                l_v == r_v
            }
            LogicalExpr::Xor(ref l, ref r) => {
                let l_v = (*l).eval(values);
                let r_v = (*r).eval(values);
                (!l_v && r_v) || (l_v && !r_v)
            }
            LogicalExpr::Ite {
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

#[test]
fn from_sexpr_e2e() {
    // this string represents X XOR Y. it exercises each branch of the match statement
    // within the from_sexpr helper
    let x_xor_y = String::from("(And (Or (Var X) (Var Y)) (Or (Not (Var X)) (Not (Var Y))))");
    let expr = serde_sexpr::from_str::<LogicalSExpr>(&x_xor_y).unwrap();

    let manually_constructed = LogicalExpr::And(
        Box::new(LogicalExpr::Or(
            Box::new(LogicalExpr::Literal(0, true)),
            Box::new(LogicalExpr::Literal(1, true)),
        )),
        Box::new(LogicalExpr::Or(
            Box::new(LogicalExpr::Literal(0, false)),
            Box::new(LogicalExpr::Literal(1, false)),
        )),
    );

    assert_eq!(LogicalExpr::from_sexpr(&expr), manually_constructed)
}
