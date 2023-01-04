//! Top-down decision DNNF compiler and manipulator

use crate::{backing_store::*, repr::ddnnf::DDNNFPtr};
use bumpalo::Bump;
use num::Num;
use rand::Rng;
use rustc_hash::FxHashMap;

use crate::{
    backing_store::bump_table::BackedRobinhoodTable,
    repr::{
        bdd::{BddNode, BddPtr},
        cnf::*,
        var_label::{Literal, VarLabel},
        var_order::VarOrder,
    },
};



// /// Probabilistically hashes a partially instantiated CNF, used primarily for
// /// component caching during bottom-up compilation (a component is a partially
// /// instantiated CNF).
// ///
// /// The hash function satisfies the invariant that, with (arbitrarily) high
// /// probability, two components are syntactically equal if and only if they have
// /// the same hash.
// ///
// /// It works by associating each literal in the CNF with a distinct prime
// /// number. To compute the hash function, one takes the product (modulo a prime
// /// field, the base of which is specified in PRIMES) of all unset literals in
// /// clauses that are not satisfied.
// ///
// /// Example: Assume we have the following CNF, with its literals annotated with
// /// distinct primes:
// /// ```
// /// // `(a \/ b) /\ (!a \/ c)`
// /// //   ^    ^      ^     ^
// /// //   2    3      5     7
// /// ```
// /// Then, if we were to hash this CNF with the partial model (a = T), would
// /// get the value 7
// #[derive(Eq, PartialEq, Debug, Clone)]
// pub struct CnfHasher {
//     weighted_cnf: Vec<Vec<(usize, Literal)>>,
//     /// state[x] contains a list of unsatisfied clauses in the CNF (indexed into weighted_cnf)
//     state: Vec<HashSet<usize>>,
//     /// pos_lits[l] contains the list of indexes of clauses that contain the positive literal l
//     pos_lits: Vec<Vec<usize>>,
//     /// neg_lits[l] contains the list of indexes of clauses that contain the negative literal l
//     neg_lits: Vec<Vec<usize>>,
// }

// impl CnfHasher {
//     pub fn new(cnf: &Cnf) -> CnfHasher {
//         let mut primes = primal::Primes::all();
//         let weighted_cnf = cnf
//             .clauses
//             .iter()
//             .map({
//                 |clause| {
//                     clause
//                         .iter()
//                         .map(|lit| (primes.next().unwrap(), *lit))
//                         .collect()
//                 }
//             })
//             .collect();

//         let sat_clauses: HashSet<usize> = cnf
//             .clauses
//             .iter()
//             .enumerate()
//             .filter_map({
//                 |(clause_idx, clause)| {
//                     if clause.len() > 1 {
//                         Some(clause_idx)
//                     } else {
//                         None
//                     }
//                 } // ignore units
//             })
//             .collect();

//         let pos_lits: Vec<Vec<usize>> = (0..cnf.num_vars())
//             .map(|lit_idx| {
//                 cnf.clauses
//                     .iter()
//                     .enumerate()
//                     .filter_map(|(clause_idx, clause)| {
//                         if clause.contains(&Literal::new(VarLabel::new_usize(lit_idx), true)) {
//                             Some(clause_idx)
//                         } else {
//                             None
//                         }
//                     })
//                     .collect()
//             })
//             .collect();

//         let neg_lits: Vec<Vec<usize>> = (0..cnf.num_vars())
//             .map(|lit_idx| {
//                 cnf.clauses
//                     .iter()
//                     .enumerate()
//                     .filter_map(|(clause_idx, clause)| {
//                         if clause.contains(&Literal::new(VarLabel::new_usize(lit_idx), false)) {
//                             Some(clause_idx)
//                         } else {
//                             None
//                         }
//                     })
//                     .collect()
//             })
//             .collect();

//         CnfHasher {
//             weighted_cnf,
//             state: vec![sat_clauses],
//             pos_lits,
//             neg_lits,
//         }
//     }

//     pub fn decide(&mut self, lit: Literal) {
//         if lit.get_polarity() {
//             for clause_idx in self.pos_lits[lit.get_label().value_usize()].iter() {
//                 self.state.last_mut().unwrap().remove(clause_idx);
//             }
//         } else {
//             for clause_idx in self.neg_lits[lit.get_label().value_usize()].iter() {
//                 self.state.last_mut().unwrap().remove(clause_idx);
//             }
//         }
//     }

//     pub fn push(&mut self) {
//         let n = self.state.last().unwrap().clone();
//         self.state.push(n);
//     }

//     pub fn pop(&mut self) {
//         self.state.pop();
//     }

//     pub fn hash(&self, m: &PartialModel) -> HashedCNF {
//         let mut v: [u128; NUM_PRIMES] = [1; NUM_PRIMES];
//         'outer: for clause_idx in self.state.last().unwrap().iter() {
//             let mut cur_clause_v: u128 = 1;
//             let cur_clause = &self.weighted_cnf[*clause_idx];
//             for (ref weight, ref lit) in cur_clause.iter() {
//                 if m.lit_implied(*lit) {
//                     // move onto the next clause without updating the
//                     // accumulator
//                     continue 'outer;
//                 } else if m.lit_neg_implied(*lit) {
//                     // skip this literal and move onto the next one
//                     continue;
//                 } else {
//                     // cur_clause_v = cur_clause_v * (*weight as u128);
//                     cur_clause_v = cur_clause_v.wrapping_mul(*weight as u128);
//                 }
//             }
//             for i in 0..NUM_PRIMES {
//                 // TODO at the moment this modular multiplication is very slow; we should use a
//                 // crate that supports fast modular multiplication for fixed prime fields
//                 // using just 128-bit for now, but this is a hack
//                 // v[i] = (v[i]* (cur_clause_v)) % PRIMES[i];
//                 v[i] = v[i].wrapping_mul(cur_clause_v);
//             }
//         }
//         HashedCNF { v }
//     }
// }

pub struct DecisionNNFBuilder {
    compute_table: BackedRobinhoodTable<BddNode>,
}

impl DecisionNNFBuilder {
    pub fn new() -> DecisionNNFBuilder {
        DecisionNNFBuilder {
            compute_table: BackedRobinhoodTable::new(),
        }
    }

    /// Normalizes and fetches a node from the store
    fn get_or_insert(&mut self, bdd: BddNode) -> BddPtr {
        if bdd.high.is_neg() {
            let bdd = BddNode::new(bdd.var, bdd.low.neg(), bdd.high.neg());
            BddPtr::new_compl(self.compute_table.get_or_insert(bdd))
        } else {
            let bdd = BddNode::new(bdd.var, bdd.low, bdd.high);
            BddPtr::new_reg(self.compute_table.get_or_insert(bdd))
        }
    }

    /// Get a pointer to the variable with label `lbl` and polarity `polarity`
    pub fn var(&mut self, lbl: VarLabel, polarity: bool) -> BddPtr {
        let bdd = BddNode::new(lbl, BddPtr::false_ptr(), BddPtr::true_ptr());
        let r = self.get_or_insert(bdd);
        if polarity {
            r
        } else {
            r.neg()
        }
    }

    fn conjoin_implied(&mut self, literals: impl Iterator<Item = Literal>, nnf: BddPtr) -> BddPtr {
        if nnf.is_false() {
            return BddPtr::false_ptr();
        }
        let mut sub = nnf;
        for l in literals {
            let node = if l.get_polarity() {
                BddNode::new(l.get_label(), BddPtr::false_ptr(), sub)
            } else {
                BddNode::new(l.get_label(), sub, BddPtr::false_ptr())
            };
            sub = self.get_or_insert(node);
        }
        sub
    }

    // /// Returns A BDD that represents `cnf` conditioned on all
    // ///     variables set in the current top model
    // /// We need both of these BDDs for sound CNF caching
    // /// `cache`: a map from hashed CNFs to their compiled BDDs
    // fn topdown_h(
    //     &mut self,
    //     cnf: &Cnf,
    //     sat: &mut SATSolver,
    //     level: usize,
    //     order: &VarOrder,
    //     hasher: &mut CnfHasher,
    //     cache: &mut FxHashMap<HashedCNF, BddPtr>,
    // ) -> BddPtr {
    //     // check for base case
    //     let assgn = sat.get_implied_units();
    //     if level >= cnf.num_vars() || cnf.is_sat_partial(&assgn) {
    //         return BddPtr::true_ptr();
    //     }
    //     let cur_v = order.var_at_level(level);

    //     // check if this literal is currently set in unit propagation; if
    //     // it is, skip it
    //     if assgn.is_set(cur_v) {
    //         return self.topdown_h(cnf, sat, level + 1, order, hasher, cache);
    //     }

    //     // check cache
    //     let hashed = hasher.hash(&assgn);
    //     match cache.get(&hashed) {
    //         None => (),
    //         Some(v) => {
    //             return *v;
    //         }
    //     }

    //     // recurse on both values of cur_v
    //     sat.push();
    //     hasher.push();
    //     sat.decide(Literal::new(cur_v, true));
    //     hasher.decide(Literal::new(cur_v, true));
    //     let unsat = sat.unsat_unit();
    //     let high_bdd = if !unsat {
    //         let new_assgn = sat.get_implied_units();

    //         let sub = self.topdown_h(cnf, sat, level + 1, order, hasher, cache);
    //         let implied_lits = new_assgn
    //             .get_vec()
    //             .iter()
    //             .enumerate()
    //             .zip(assgn.get_vec())
    //             .filter_map(|((idx, new), prev)| {
    //                 if new != prev && idx != cur_v.value_usize() {
    //                     Some(Literal::new(VarLabel::new_usize(idx), new.unwrap()))
    //                 } else {
    //                     None
    //                 }
    //             });
    //         self.conjoin_implied(implied_lits, sub)
    //     } else {
    //         BddPtr::false_ptr()
    //     };
    //     sat.pop();
    //     hasher.pop();

    //     sat.push();
    //     hasher.push();
    //     sat.decide(Literal::new(cur_v, false));
    //     hasher.decide(Literal::new(cur_v, false));
    //     let unsat = sat.unsat_unit();
    //     let low_bdd = if !unsat {
    //         let new_assgn = sat.get_implied_units();

    //         let sub = self.topdown_h(cnf, sat, level + 1, order, hasher, cache);
    //         let implied_lits = new_assgn
    //             .get_vec()
    //             .iter()
    //             .enumerate()
    //             .zip(assgn.get_vec())
    //             .filter_map(|((idx, new), prev)| {
    //                 if new != prev && idx != cur_v.value_usize() {
    //                     Some(Literal::new(VarLabel::new_usize(idx), new.unwrap()))
    //                 } else {
    //                     None
    //                 }
    //             });
    //         self.conjoin_implied(implied_lits, sub)
    //     } else {
    //         BddPtr::false_ptr()
    //     };
    //     sat.pop();
    //     hasher.pop();

    //     let r = if high_bdd == low_bdd {
    //         high_bdd
    //     } else {
    //         let bdd = BddNode::new(cur_v, low_bdd, high_bdd);
    //         self.get_or_insert(bdd)
    //     };
    //     cache.insert(hashed, r);
    //     r
    // }

    // /// compile a decision DNNF top-down from a CNF with the searching order
    // /// specified by `order`
    // pub fn from_cnf_topdown(&mut self, order: &VarOrder, cnf: &Cnf) -> BddPtr {
    //     let mut sat = SATSolver::new(cnf);
    //     if sat.unsat_unit() {
    //         return BddPtr::false_ptr();
    //     }

    //     let mut r = self.topdown_h(
    //         cnf,
    //         &mut sat,
    //         0,
    //         order,
    //         &mut CnfHasher::new(cnf),
    //         &mut FxHashMap::default(),
    //     );

    //     // conjoin in any initially implied literals
    //     for l in sat.get_implied_units().assignment_iter() {
    //         let node = if l.get_polarity() {
    //             BddNode::new(l.get_label(), BddPtr::false_ptr(), r)
    //         } else {
    //             BddNode::new(l.get_label(), r, BddPtr::false_ptr())
    //         };
    //         r = self.get_or_insert(node);
    //     }
    //     r
    // }

    /// Compute the Boolean function `f | var = value`
    pub fn condition(&mut self, bdd: BddPtr, lbl: VarLabel, value: bool) -> BddPtr {
        let r = self.cond_helper(bdd, lbl, value, &mut Bump::new());
        bdd.clear_scratch();
        r
    }

    fn cond_helper(&mut self, bdd: BddPtr, lbl: VarLabel, value: bool, alloc: &mut Bump) -> BddPtr {
        if bdd.is_const() {
            bdd
        } else if bdd.var() == lbl {
            let node = bdd.into_node();
            let r = if value { node.high } else { node.low };
            if bdd.is_neg() {
                r.neg()
            } else {
                r
            }
        } else {
            // check cache
            let idx = match bdd.get_scratch::<BddPtr>() {
                None => (),
                Some(v) => return if bdd.is_neg() { v.neg() } else { *v },
            };

            // recurse on the children
            let n = bdd.into_node();
            let l = self.cond_helper(n.low, lbl, value, alloc);
            let h = self.cond_helper(n.high, lbl, value, alloc);
            if l == h {
                if bdd.is_neg() {
                    return l.neg();
                } else {
                    return l;
                };
            };
            let res = if l != n.low || h != n.high {
                // cache and return the new BDD
                let new_bdd = BddNode::new(bdd.var(), l, h);
                let r = self.get_or_insert(new_bdd);
                if bdd.is_neg() {
                    r.neg()
                } else {
                    r
                }
            } else {
                // nothing changed
                bdd
            };
            bdd.set_scratch(alloc, if bdd.is_neg() { res.neg() } else { res });
            res
        }
    }
}

// #[test]
// fn test_dnnf() {
//     let clauses = vec![
//         vec![
//             Literal::new(VarLabel::new(0), true),
//             Literal::new(VarLabel::new(1), false),
//         ],
//         // vec![
//         // Literal::new(VarLabel::new(0), false),
//         // Literal::new(VarLabel::new(1), true)
//         // ]
//     ];
//     let cnf = Cnf::new(clauses);
//     let mut mgr = DecisionNNFBuilder::new();
//     let c2 = mgr.from_cnf_topdown(&VarOrder::linear_order(cnf.num_vars()), &cnf);
//     println!("c2: {}", c2.to_string_debug());
// }
