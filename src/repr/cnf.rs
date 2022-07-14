//! A representation of a conjunctive normal form (CNF)

use crate::builder::var_order::VarOrder;
use im::Vector;
use rand;
use rand::rngs::ThreadRng;
use rand::Rng;
use crate::repr::var_label::{Literal, VarLabel};
use std::cmp::{max, min};
use std::collections::HashMap;
extern crate quickcheck;
use self::quickcheck::{Arbitrary, Gen};
use crate::repr::model::PartialModel;

const PRIMES: [u128; 4] = [64733603481794218985640164159, 79016979402926483817096290621, 46084029846212370199652019757, 49703069216273825773136967137];
// number of primes to consider during CNF hashing
const NUM_PRIMES : usize = 2;


#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct HashedCNF {
    v: [u128; NUM_PRIMES]
}

/// Probabilistically hashes a partially instantiated CNF, used primarily for 
/// component caching during bottom-up compilation (a component is a partially 
/// instantiated CNF).
/// 
/// The hash function satisfies the invariant that, with (arbitrarily) high
/// probability, two components are syntactically equal if and only if they have
/// the same hash.
/// 
/// It works by associating each literal in the CNF with a distinct prime
/// number. To compute the hash function, one takes the product (modulo a prime
/// field, the base of which is specified in PRIMES) of all unset literals in
/// clauses that are not satisfied.
/// 
/// Example: Assume we have the following CNF, with its literals annotated with
/// distinct primes:
/// ```
/// // `(a \/ b) /\ (!a \/ c)`
/// //   ^    ^      ^     ^
/// //   2    3      5     7
/// ```
/// Then, if we were to hash this CNF with the partial model (a = T), would 
/// get the value 5*7 = 35
pub struct CnfHasher {
    weighted_cnf: Vec<Vec<(usize, Literal)>>
}

impl CnfHasher {
    pub fn new(cnf: &Cnf) -> CnfHasher {
        let mut primes = primal::Primes::all();
        CnfHasher { weighted_cnf: cnf.clauses.iter().map({|clause|
            clause.iter().map(|lit| {
                (primes.next().unwrap(), *lit)
            }).collect()
        }).collect()}
    }

    pub fn hash(&self, m: &PartialModel) -> HashedCNF {
        let mut v : [u128; NUM_PRIMES] = [1; NUM_PRIMES];
        'outer: for clause in self.weighted_cnf.iter() {
            let mut cur_clause_v : u128 = 1;
            for (ref weight, ref lit) in clause.iter() {
                if m.lit_implied(*lit) {
                    // move onto the next clause without updating the
                    // accumulator
                    continue 'outer;
                } else if m.lit_neg_implied(*lit) {
                    // skip this literal and move onto the next one
                    continue; 
                } else {
                    cur_clause_v = cur_clause_v * (*weight as u128);
                }
            }
            for i in 0..NUM_PRIMES {
                // TODO at the moment this modular multiplication is very slow; we should use a 
                // crate that supports fast modular multiplication for fixed prime fields
                // using just 128-bit for now, but this is a hack
                // v[i] = (v[i]* (cur_clause_v)) % PRIMES[i];
                v[i] = v[i].wrapping_mul(cur_clause_v);
            }
        }
        HashedCNF { v }
    }
}


#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Cnf {
    clauses: Vec<Vec<Literal>>,
    imm_clauses: Vector<Vector<Literal>>,
    num_vars: usize,
}

pub struct AssignmentIter {
    cur: Option<Vec<bool>>,
    num_vars: usize,
}

impl AssignmentIter {
    pub fn new(num_vars: usize) -> AssignmentIter {
        AssignmentIter {
            cur: None,
            num_vars,
        }
    }
}

impl Iterator for AssignmentIter {
    type Item = Vec<bool>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.cur.is_none() {
            self.cur = Some((0..self.num_vars).map(|_| false).collect());
            return self.cur.clone();
        } else {
            // attempt to do a binary increment of the current state
            let (new_c, carry) = self.cur.as_ref().unwrap().iter().fold(
                (Vec::new(), true),
                |(mut cur_l, carry), cur_assgn| {
                    // half-adder
                    let new_itm = cur_assgn ^ carry;
                    let new_carry = *cur_assgn && carry;
                    cur_l.push(new_itm);
                    (cur_l, new_carry)
                },
            );

            self.cur = Some(new_c);
            if carry {
                None
            } else {
                self.cur.clone()
            }
        }
    }
}

impl Cnf {
    pub fn from_file(v: String) -> Cnf {
        use dimacs::*;
        let r = parse_dimacs(&v).unwrap();
        let (_, cvec) = match r {
            Instance::Cnf { num_vars, clauses } => (num_vars, clauses),
            _ => panic!(),
        };
        let mut clause_vec: Vec<Vec<Literal>> = Vec::new();
        let mut m = 0;
        for itm in cvec.iter() {
            let mut lit_vec: Vec<Literal> = Vec::new();
            for l in itm.lits().iter() {
                let b = match l.sign() {
                    Sign::Neg => false,
                    Sign::Pos => true,
                };
                // subtract 1, we are 0-indexed
                let lbl = VarLabel::new(l.var().to_u64() - 1);
                m = max(l.var().to_u64() as usize, m);
                lit_vec.push(Literal::new(lbl, b));
            }
            clause_vec.push(lit_vec);
        }
        Cnf::new(clause_vec)
    }

    pub fn rand_cnf(rng: &mut ThreadRng, num_vars: usize, num_clauses: usize) -> Cnf {
        assert!(num_clauses > 2, "requires at least 2 clauses in CNF");
        let vars: Vec<Literal> = (1..num_vars)
            .map(|x| Literal::new(VarLabel::new(x as u64), rand::random()))
            .collect();
        // let range = rand::distributions iRange::new(0, vars.len());
        let clause_size = 3;
        // we generate a random cnf
        let mut clause_vec: Vec<Vec<Literal>> = Vec::new();
        for _ in 0..num_clauses {
            let num_vars = clause_size;
            if num_vars > 1 {
                let mut var_vec: Vec<Literal> = Vec::new();
                for _ in 0..clause_size {
                    let var = vars.get(rng.gen_range(0..vars.len())).unwrap().clone();
                    var_vec.push(var);
                }
                clause_vec.push(var_vec);
            } else {
                let var = vars.get(rng.gen_range(0..vars.len())).unwrap().clone();
                clause_vec.push(vec![var]);
            }
        }
        Cnf::new(clause_vec)
    }

    pub fn num_vars(&self) -> usize {
        self.num_vars
    }

    pub fn clauses(&self) -> &[Vec<Literal>] {
        self.clauses.as_slice()
    }

    /// evaluate this CNF on an assignment
    /// assignment[x] is the assignment to VarLabel(x)
    pub fn eval(&self, assignment: &Vec<bool>) -> bool {
        assert!(assignment.len() >= self.num_vars());
        for clause in self.clauses.iter() {
            let mut clause_sat = false;
            for lit in clause.iter() {
                let assgn = assignment[lit.label() as usize];
                if lit.get_polarity() == assgn {
                    clause_sat = true;
                }
            }
            if !clause_sat {
                return false;
            }
        }
        // no unsat clauses
        return true;
    }

    /// true if the partial model implies the CNF
    pub fn is_sat_partial(&self, partial_assignment: &PartialModel) -> bool {
        for clause in self.clauses.iter() {
            let mut clause_sat = false;
            for lit in clause.iter() {
                match partial_assignment.get(lit.get_label()) {
                    Some(assgn) => {
                        if lit.get_polarity() == assgn {
                            clause_sat = true;
                        }
                    },
                    None => ()
                };
            }
            if !clause_sat {
                return false;
            }
        }
        return true;
    }

    pub fn new(mut clauses: Vec<Vec<Literal>>) -> Cnf {
        let mut m = 0;
        // filter out empty clauses
        clauses.retain(|x| x.len() > 0);
        for clause in clauses.iter_mut() {
            for lit in clause.iter() {
                m = max(lit.get_label().value() + 1, m);
            }
            // remove duplicate literals
            clause.sort_by(|a, b| a.get_label().value().cmp(&b.get_label().value()));
            clause.dedup();
        }

        Cnf {
            clauses: clauses.clone(),
            num_vars: m as usize,
            imm_clauses: clauses.iter().map(|x| Vector::from(x)).collect()
        }
    }

    /// compute the average span of the clauses with the ordering given by
    /// `lbl_to_pos`, which is a mapping from variable labels to their position
    /// in the ordering
    fn average_span(&self, lbl_to_pos: &[usize]) -> f64 {
        let mut total = 0;
        for clause in self.clauses.iter() {
            let mut min_pos = lbl_to_pos.len();
            let mut max_pos = 0;
            // find the two variables in the clause which are farthest
            // from each other in the order
            for lit in clause.iter() {
                let this_pos = lbl_to_pos[lit.get_label().value() as usize];
                min_pos = min(this_pos, min_pos);
                max_pos = max(this_pos, max_pos);
            }
            total += max_pos - min_pos;
        }
        (total as f64) / (self.clauses.len() as f64)
    }

    /// computes the center of gravity of a particular clause for a given order
    /// Aloul, Fadi A., Igor L. Markov, and Karem A. Sakallah. "FORCE: a fast
    /// and easy-to-implement variable-ordering heuristic." Proceedings of the
    /// 13th ACM Great Lakes symposium on VLSI. 2003.
    fn center_of_gravity(&self, clause: &[Literal], lbl_to_pos: &[usize]) -> f64 {
        let sum = clause.iter().fold(0, |acc, &lbl| {
            lbl_to_pos[lbl.get_label().value() as usize] + acc
        });
        let r = (sum as f64) / (clause.len() as f64);
        r
    }

    /// compute a weighted model count of a CNF
    /// Note: not efficient! this is exponential in #variables
    /// mostly for internal testing purposes
    pub fn wmc(&self, weights: &HashMap<VarLabel, (usize, usize)>) -> usize {
        let mut total = 0;
        let mut weight_vec = Vec::new();
        for i in 0..self.num_vars() {
            weight_vec.push(weights[&VarLabel::new(i as u64)]);
        }
        for assgn in AssignmentIter::new(self.num_vars()) {
            if assgn.is_empty() {
                break;
            };
            if self.eval(&assgn) {
                let assgn_w = assgn.iter().enumerate().fold(1, |v, (idx, &polarity)| {
                    let (loww, highw) = weight_vec[idx];
                    v * (if polarity { highw } else { loww })
                });
                total += assgn_w;
            }
        }
        return total;
    }

    pub fn linear_order(&self) -> VarOrder {
        let v = (0..(self.num_vars))
            .into_iter()
            .map(|x| VarLabel::new(x as u64))
            .collect();
        return VarOrder::new(v);
    }

    /// heuristically generate a variable ordering which minimizes the average
    /// clause span
    pub fn force_order(&self) -> VarOrder {
        // map from position -> label (i.e., first element is the first in the
        // order)
        let mut lbl_to_pos: Vec<usize> = (0..(self.num_vars)).collect();
        // let mut rng = thread_rng();
        // rng.shuffle(&mut lbl_to_pos);
        // perform 100 iterations of force-update
        let mut cur_span: f64 = self.average_span(&lbl_to_pos);
        let mut prev_span;
        loop {
            prev_span = cur_span;
            let mut cog: Vec<f64> = Vec::with_capacity(self.clauses.len());
            for clause in self.clauses.iter() {
                cog.push(self.center_of_gravity(clause, &lbl_to_pos));
            }
            // compute average centers of gravity for each variable
            // a vector which holds (1) the running CoG, (2) the number of clauses
            // which contain a given variable
            let mut update: Vec<(f64, usize)> = Vec::with_capacity(self.num_vars);
            for _ in 0..self.num_vars {
                update.push((0.0, 0));
            }
            for (idx, clause) in self.clauses.iter().enumerate() {
                for &lit in clause.iter() {
                    let (cur_total, num_edges) = update[lit.get_label().value() as usize];
                    update[lit.get_label().value() as usize] =
                        (cur_total + cog[idx], num_edges + 1);
                }
            }
            let avg_cog: Vec<f64> = update
                .into_iter()
                .map(|(total, cnt)| if cnt == 0 { 0.0 } else { total / (cnt as f64) })
                .collect();

            let l = avg_cog.len();
            let mut avg_cog: Vec<(f64, usize)> = avg_cog.into_iter().zip(0..l).collect();
            // now sort avg_cog on the centers of gravity
            avg_cog.sort_by(|&(ref c1, _), &(ref c2, _)| c1.partial_cmp(c2).unwrap());
            // update positions
            let pos_to_lbl: Vec<usize> = avg_cog.into_iter().map(|(_, p)| p).collect();
            // now convert to lbl_to_pos
            for (idx, lbl) in pos_to_lbl.into_iter().enumerate() {
                lbl_to_pos[lbl] = idx;
            }
            cur_span = self.average_span(&lbl_to_pos);
            if prev_span - cur_span < 1.0 {
                break;
            }
        }
        let final_order: Vec<VarLabel> = lbl_to_pos
            .into_iter()
            .map(|v| VarLabel::new(v as u64))
            .collect();
        VarOrder::new(final_order)
    }

    /// Updates the CNF to a new CNF that results from conditioning on the supplied literal
    pub fn condition(&mut self, lit: Literal) -> Cnf {
        let new_cnf: Vec<Vec<Literal>> = self
            .clauses()
            .iter()
            .filter_map(|clause| {
                // first, check if there is a true literal -- if there is, filter out this clause
                if clause
                    .iter()
                    .find(|outer| {
                        outer.get_label() == lit.get_label()
                            && outer.get_polarity() == lit.get_polarity()
                    })
                    .is_some()
                {
                    None
                } else {
                    // next, filter out clauses with false literals
                    let filtered: Vec<Literal> = clause
                        .into_iter()
                        .filter(|outer| {
                            !(lit.get_label() == outer.get_label()
                                && lit.get_polarity() != outer.get_polarity())
                        })
                        .map(|x| *x)
                        .collect();
                    Some(filtered)
                }
            })
            .collect();
        return Cnf::new(new_cnf);
    }

    pub fn to_string(&self) -> String {
        let mut r = String::new();
        for clause in self.clauses.iter() {
            let mut clause_str = String::new();
            for lit in clause.iter() {
                let lit_str = format!(
                    "{}{}",
                    if lit.get_polarity() { "" } else { "!" },
                    lit.get_label().value()
                );
                if clause_str.is_empty() {
                    clause_str = lit_str;
                } else {
                    clause_str = format!("{} || {}", clause_str, lit_str);
                }
            }
            if r.is_empty() {
                r = format!("({})", clause_str);
            } else {
                r = format!(" {} && ({})", r, clause_str);
            }
        }
        return r;
    }

    /// Generates the sub-cnf that is the result of subsuming all assigned literals in `m`
    pub fn sub_cnf(&self, m: &PartialModel) -> Vector<Vector<Literal>> {
        self.imm_clauses.clone().into_iter().filter_map(|clause| {
            // first filter out all satisfied clauses
            for lit in clause.iter() {
                if m.lit_implied(*lit) {
                    return None
                }
            }
            // then filter out unsat literals in this clause
            Some(clause.into_iter().filter(|lit| !m.lit_neg_implied(*lit)).collect())
        }).collect()
    }
}

impl Arbitrary for Cnf {
    fn arbitrary(g: &mut Gen) -> Cnf {
        let num_vars = (u64::arbitrary(g) % 8) + 1;
        let num_clauses = (usize::arbitrary(g) % 16) + 1;
        let mut clauses = Vec::new();
        for _ in 0..num_clauses {
            let clause_size = (usize::arbitrary(g) % 3) + 1;
            let mut clause: Vec<Literal> = Vec::new();
            for _ in 0..clause_size {
                clause.push(Literal::new(
                    VarLabel::new(u64::arbitrary(g) % num_vars),
                    bool::arbitrary(g),
                ));
            }
            clauses.push(clause);
        }
        Cnf::new(clauses)
    }
}


#[test]
fn test_cnf_wmc() {
    use maplit::*;

    let v = vec![vec![
        Literal::new(VarLabel::new(0), true),
        Literal::new(VarLabel::new(1), false),
    ]];
    let cnf = Cnf::new(v);
    let weights = hashmap! {
        VarLabel::new(0) => (1, 1),
        VarLabel::new(1) => (1, 1),
    };
    assert_eq!(cnf.wmc(&weights), 3);
}
