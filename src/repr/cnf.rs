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

type clause_idx = usize;
type lit_idx = usize;

static PRIMES: [u128; 6] = [232013001714829759185289990985465319691, 
                            333137558950085144382304432995343556989, 
                            316319869697248684341211015143689017453, 
                            324167418430705647122534085039661688929, 
                            329367681630224674729296290250973223983, 
                            238174019593728996209712393113822161931];

/// A data-structure for efficient implementation of unit propagation with CNFs.
/// It implements a two-literal watching scheme.
/// For instance, for the CNF:
///     c0             c1
/// (A \/ !B) /\ (C \/ !A \/ B)
///  ^     ^      ^    ^
/// The literals A and !B are watched in c0, and C and !A  are watched in c1
/// This is stored in the watch lists as:
/// watch_list_pos:
///    A: [c0]
///    B: []
///    C: [c1]
/// watch_list_neg:
///    A: [c1]
///    B: [c0] 
///    C: []
/// 
/// The invariant maintained by the watched literals is that, for a given
/// partial model state, the watched literal is either (1) satisfied by the
/// model; or (2) unset in the model.
/// 
/// Every clause (except for unit clauses) watch exactly 2 unique literals at all times.
/// When a variable is decided, we decide how to update the watched literals according 
/// to the following cases
/// 
/// Case 1. We decide C=False. In this case, we deduce no units, and update the literal watches 
/// as:
///     c0             c1
/// (A \/ !B) /\ (C \/ !A \/ B)
///  ^     ^            ^    ^
/// I.e., we pick a new literal to watch in c1
/// Case 2. Decide C=True, In this case, the clause c1 is satisfied, so  we do not make 
/// any changes to its watched literals.
/// Case 3. Decide A=False. In this case there are no remaining literals to
/// watch in c0, so we deduce a unit !B and propagate. The resulting watcher
/// state unchanged in this case.
#[derive(Debug, Clone)]
pub struct UnitPropagate<'a> {
    // watch_list_pos[i] is a list of the clauses that are watching the positive
    // literal of varlabel i
    watch_list_pos: Vec<Vec<clause_idx>>,
    // similar to the above, but for negative label
    watch_list_neg: Vec<Vec<clause_idx>>,
    // stack of assignment states (all implied and decided literals)
    state: Vec<PartialModel>,
    cnf: &'a Cnf,
}

impl<'a> UnitPropagate<'a> {
    /// Returns None if UNSAT discovered during initial unit propagation
    pub fn new(cnf: &'a Cnf) -> Option<UnitPropagate<'a>> {
        let mut watch_list_pos: Vec<Vec<usize>> = Vec::new();
        let mut watch_list_neg: Vec<Vec<usize>> = Vec::new();
        let mut assignments: Vec<Option<bool>> = Vec::new();
        for _ in 0..cnf.num_vars {
            watch_list_pos.push(Vec::new());
            watch_list_neg.push(Vec::new());
            assignments.push(None);
        }

        // do initial unit propagation
        let mut implied: Vec<Literal> = Vec::new();
        for (idx, c) in cnf.clauses().iter().enumerate() {
            if c.len() == 0 {
                return None;
            }
            if c.len() == 1 {
                implied.push(c[0]);
                continue;
            }
            if c[1].get_polarity() {
                watch_list_pos[c[1].get_label().value() as usize].push(idx)
            } else {
                watch_list_neg[c[1].get_label().value() as usize].push(idx)
            }
            if c[0].get_polarity() {
                watch_list_pos[c[0].get_label().value() as usize].push(idx)
            } else {
                watch_list_neg[c[0].get_label().value() as usize].push(idx)
            }
        }

        let mut cur = UnitPropagate {
            watch_list_pos,
            watch_list_neg,
            state: vec![PartialModel::from_vec(assignments)],
            cnf,
        };

        for unit in implied {
            let r = cur.set(unit);
            if !r {
                return None;
            }
        }
        return Some(cur);
    }
    
    fn cur_state(&self) -> &PartialModel {
        &self.state.last().unwrap()
    }

    fn cur_state_mut(&mut self) -> &mut PartialModel {
        let n = self.state.len();
        &mut self.state[n - 1]
    }

    /// Sets a variable to a particular value
    pub fn decide(&mut self, new_assignment: Literal) -> bool {
        // push a fresh state onto the stack and propagate
        self.state.push(self.cur_state().clone());
        // run until no new discovered units
        // let mut prev_num_assigned = self.cur_state().assignment_iter().count();
        // loop {
            if !self.set(new_assignment) {
                return false;
            }
            // let new_num_assigned = self.cur_state().assignment_iter().count();
            // if new_num_assigned == prev_num_assigned {
            //     return true;
            // }
            // prev_num_assigned = new_num_assigned;
        // }
        return true
    }

    /// Returns an iterator over the literals that were decided at the previous decision step
    /// Panics if no previous decision step was made
    pub fn get_decided_literals(&self) -> impl Iterator<Item=Literal> + '_ {
        // the most recently decided literals is the diff between the new
        let n = self.state.len();
        let cur_state_i = self.state[n-1].get_vec().iter();
        let prev_state_i = self.state[n-2].get_vec().iter();
        let iter = cur_state_i.zip(prev_state_i).enumerate().filter_map(|(idx, (cur, prev))| {
            if *cur == *prev {
                None
            } else {
                Some(Literal::new(VarLabel::new_usize(idx), cur.unwrap()))
            }
        });
        iter
    }

    /// Backtracks to the previous decision
    pub fn backtrack(&mut self) -> () {
        assert!(self.state.len() > 1, "Unit Propagate cannot backtrack past first decision");
        self.state.pop();
    }

    /// Set a variable to a particular value and propagates
    /// returns true if success, false if UNSAT
    fn set(&mut self, new_assignment: Literal) -> bool {
        // if already assigned, check if consistent -- if not, return unsat
        match self.cur_state().get(new_assignment.get_label()) {
            None => (),
            Some(v) => {
                if new_assignment.get_polarity() != v {
                    return false;
                } else {
                    return true;
                }
            }
        };

        // update the value of the decided variable in the partial model
        self.cur_state_mut()
            .set(new_assignment.get_label(), new_assignment.get_polarity());

        let var_idx = new_assignment.get_label().value() as usize;

        // track a list of all discovered implications
        // indexes over the watchers for the current literal
        let mut watcher_idx = 0;
        // some of the internal structure here is weird to accommodate the
        // borrow checker.
        loop {
            // first check if there are any watchers; if no watchers left, break 

            if new_assignment.get_polarity() {
                if watcher_idx >= self.watch_list_neg[var_idx].len() {
                    break;
                }
            } else {
                if watcher_idx >= self.watch_list_pos[var_idx].len() {
                    break;
                }
            }
            let clause = if new_assignment.get_polarity() {
                &self.cnf.clauses()[self.watch_list_neg[var_idx][watcher_idx]]
            } else {
                &self.cnf.clauses()[self.watch_list_pos[var_idx][watcher_idx]]
            };

            // if clause is satisfied, do not change its watched literals and
            // move onto the next watcher
            let mut is_sat = false;
            for lit in clause.iter() {
                match self.cur_state().get(lit.get_label()) {
                    Some(v) if lit.get_polarity() == v => {
                        watcher_idx += 1;
                        is_sat = true;
                        break;
                    }
                    _ => (),
                }
            }
            if is_sat {
                continue;
            }

            // gather a list of all remaining unassigned literals in the current clause
            let mut remaining_lits =
                clause
                    .iter()
                    .filter(|x| {
                        match self.cur_state().get(x.get_label()) {
                            None => true,
                            Some(_) => false,
                        }
                    });

            let num_remaining = remaining_lits.clone().count();
            if num_remaining == 0 {
                // UNSAT -- need to move a watcher and there are no remaining
                // unassigned literals to watch
                return false;
            } else if num_remaining == 1 {
                // unit propagate and move onto the next watcher
                let new_unit = remaining_lits.nth(0).unwrap();
                if !self.set(*new_unit) {
                    return false;
                }
                watcher_idx += 1;
            } else {
                // num_remaining > 1, find a new literal to watch
                // first, find a new literal to watch
                let candidate_unwatched : lit_idx = remaining_lits.clone().nth(0).unwrap().get_label().value_usize();
                // check if candidate_unwatched is already being watched; if it
                // is, pick another literal to watch
                let prev_watcher : clause_idx = if new_assignment.get_polarity() {
                    self.watch_list_neg[var_idx][watcher_idx]
                } else {
                    self.watch_list_pos[var_idx][watcher_idx]
                };

                let new_lit : &Literal = if new_assignment.get_polarity() {
                    if self.watch_list_pos[candidate_unwatched].contains(&prev_watcher) {
                        remaining_lits.nth(1).unwrap()
                    } else {
                        remaining_lits.nth(0).unwrap()
                    }
                } else {
                    if self.watch_list_neg[candidate_unwatched].contains(&prev_watcher) {
                        remaining_lits.nth(1).unwrap()
                    } else {
                        remaining_lits.nth(0).unwrap()
                    }
                };

                let new_loc = new_lit.get_label().value_usize();

                if new_assignment.get_polarity() {
                    self.watch_list_neg[var_idx].swap_remove(watcher_idx);
                } else {
                    self.watch_list_pos[var_idx].swap_remove(watcher_idx);
                };

                // now add the new one
                if new_lit.get_polarity() {
                    self.watch_list_pos[new_loc].push(prev_watcher);
                } else {
                    self.watch_list_neg[new_loc].push(prev_watcher);
                }
                // do not increment watcher_idx (since we decreased the total number of watchers, we have made progress)
            }
        }
        return true;
    }

    pub fn get_assgn(&self) -> &PartialModel {
        &self.cur_state()
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct HashedCNF {
    v: [u128; 2]
}

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
        let mut v0 : u128 = 1;
        let mut v1 : u128 = 1;
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
                    cur_clause_v = cur_clause_v.wrapping_mul(*weight as u128);
                    // cur_clause_v = cur_clause_v * (*weight as u128);
                }
            }
            v0 = v0.wrapping_mul(cur_clause_v) % PRIMES[0];
            v1 = v0.wrapping_mul(cur_clause_v) % PRIMES[1];
            // v = v * (cur_clause_v);
        }
        HashedCNF { v: [v0, v1] }
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
fn test_unit_propagate_1() {
    let v = vec![
        vec![Literal::new(VarLabel::new(0), false)],
        vec![
            Literal::new(VarLabel::new(0), true),
            Literal::new(VarLabel::new(1), false),
        ],
        vec![
            Literal::new(VarLabel::new(1), true),
            Literal::new(VarLabel::new(2), true),
        ],
    ];

    let mut cnf = Cnf::new(v);
    let mut up = UnitPropagate::new(&cnf).unwrap();
    let assgn = up.get_assgn().get_vec();
    assert_eq!(assgn[0], Some(false));
    assert_eq!(assgn[1], Some(false));
    assert_eq!(assgn[2], Some(true));
}

#[test]
fn test_unit_propagate_2() {
    let v = vec![
        vec![
            Literal::new(VarLabel::new(0), true),
            Literal::new(VarLabel::new(1), false),
        ],
        vec![
            Literal::new(VarLabel::new(1), true),
            Literal::new(VarLabel::new(2), true),
        ],
    ];

    let cnf = Cnf::new(v);
    let up = UnitPropagate::new(&cnf).unwrap();
    let assgn = up.get_assgn().get_vec();
    assert_eq!(assgn[0], None);
    assert_eq!(assgn[1], None);
    assert_eq!(assgn[2], None);
}

#[test]
fn test_unit_propagate_3() {
    let v = vec![
        vec![Literal::new(VarLabel::new(0), false)],
        vec![
            Literal::new(VarLabel::new(0), false),
            Literal::new(VarLabel::new(1), false),
        ],
        vec![
            Literal::new(VarLabel::new(0), false),
            Literal::new(VarLabel::new(1), true),
        ],
        vec![
            Literal::new(VarLabel::new(1), true),
            Literal::new(VarLabel::new(2), true),
        ],
    ];

    let cnf = Cnf::new(v);
    let up = UnitPropagate::new(&cnf).unwrap();
    let assgn = up.get_assgn().get_vec();
    assert_eq!(assgn[0], Some(false));
    assert_eq!(assgn[1], None);
    assert_eq!(assgn[2], None);
}

#[test]
fn test_unit_propagate_4() {
    let v = vec![
        vec![
            Literal::new(VarLabel::new(0), false),
            Literal::new(VarLabel::new(1), false),
        ],
    ];

    let cnf = Cnf::new(v);
    let mut up = UnitPropagate::new(&cnf).unwrap();
    let v1 = up.decide(Literal::new(VarLabel::new(0), false));
    assert!(v1);
    let v = up.decide(Literal::new(VarLabel::new(1), true));
    assert_eq!(v, true);
    up.backtrack();
    let v = up.decide(Literal::new(VarLabel::new(1), false));
    assert_eq!(v, true);
}

#[test]
fn test_unit_propagate_5() {
    let v = vec![vec![Literal::new(VarLabel::new(1), true), Literal::new(VarLabel::new(3), true)],
                 vec![Literal::new(VarLabel::new(3), false), Literal::new(VarLabel::new(2), true), Literal::new(VarLabel::new(4), true)]];
    let cnf = Cnf::new(v);
    let mut up = UnitPropagate::new(&cnf).unwrap();
    let v1 = up.decide(Literal::new(VarLabel::new(3), true));
    assert!(v1);
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
