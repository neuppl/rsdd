//! Top-down decision DNNF compiler and manipulator

use std::{cell::RefCell, collections::HashSet};

use crate::{
    backing_store::*,
    repr::{
        ddnnf::DDNNFPtr,
        robdd::create_semantic_hash_map,
        unit_prop::{DecisionResult, SATSolver},
    },
};
use rustc_hash::FxHashMap;

use crate::{
    backing_store::bump_table::BackedRobinhoodTable,
    repr::{
        cnf::*,
        robdd::{BddNode, BddPtr},
        var_label::{Literal, VarLabel},
        var_order::VarOrder,
    },
};

pub struct DecisionNNFBuilder<'a> {
    compute_table: RefCell<BackedRobinhoodTable<'a, BddNode<'a>>>,
    order: VarOrder,
}

impl<'a> DecisionNNFBuilder<'a> {
    pub fn new(order: VarOrder) -> DecisionNNFBuilder<'a> {
        DecisionNNFBuilder {
            order,
            compute_table: RefCell::new(BackedRobinhoodTable::new()),
        }
    }

    /// Normalizes and fetches a node from the store
    fn get_or_insert(&'a self, bdd: BddNode<'a>) -> BddPtr<'a> {
        // TODO make this safe
        unsafe {
            let tbl = &mut *self.compute_table.as_ptr();
            if bdd.high.is_neg() {
                let bdd = BddNode::new(bdd.var, bdd.low.neg(), bdd.high.neg());
                BddPtr::new_compl(tbl.get_or_insert(bdd))
            } else {
                let bdd = BddNode::new(bdd.var, bdd.low, bdd.high);
                BddPtr::new_reg(tbl.get_or_insert(bdd))
            }
        }
    }

    /// Get a pointer to the variable with label `lbl` and polarity `polarity`
    pub fn var(&'a self, lbl: VarLabel, polarity: bool) -> BddPtr<'a> {
        let bdd = BddNode::new(lbl, BddPtr::false_ptr(), BddPtr::true_ptr());
        let r = self.get_or_insert(bdd);
        if polarity {
            r
        } else {
            r.neg()
        }
    }

    fn conjoin_implied(
        &'a self,
        literals: impl Iterator<Item = Literal>,
        nnf: BddPtr<'a>,
    ) -> BddPtr<'a> {
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

    /// Returns A BDD that represents `cnf` conditioned on all
    ///     variables set in the current top model
    /// We need both of these BDDs for sound CNF caching
    /// `cache`: a map from hashed CNFs to their compiled BDDs
    fn topdown_h(
        &'a self,
        cnf: &Cnf,
        sat: &mut SATSolver,
        level: usize,
        cache: &mut FxHashMap<u128, BddPtr<'a>>,
    ) -> BddPtr<'a> {
        // check for base case
        if level >= cnf.num_vars() || sat.is_sat() {
            return BddPtr::true_ptr();
        }
        let cur_v = self.order.var_at_level(level);

        // check if this literal is currently set in unit propagation; if
        // it is, skip it
        if sat.is_set(cur_v) {
            return self.topdown_h(cnf, sat, level + 1, cache);
        }

        // check cache
        let hashed = sat.get_cur_hash();
        match cache.get(&hashed) {
            None => (),
            Some(v) => {
                return *v;
            }
        }

        // recurse on both values of cur_v
        let high_bdd = match sat.decide(Literal::new(cur_v, true)) {
            DecisionResult::UNSAT => BddPtr::false_ptr(),
            DecisionResult::SAT => {
                let new_assgn = sat.get_difference().filter(|x| x.get_label() != cur_v);
                let r = self.conjoin_implied(new_assgn, BddPtr::true_ptr());
                sat.pop();
                r
            }
            DecisionResult::Unknown => {
                let sub = self.topdown_h(cnf, sat, level + 1, cache);
                let new_assgn = sat.get_difference().filter(|x| x.get_label() != cur_v);
                let r = self.conjoin_implied(new_assgn, sub);
                sat.pop();
                r
            }
        };
        let low_bdd = match sat.decide(Literal::new(cur_v, false)) {
            DecisionResult::UNSAT => BddPtr::false_ptr(),
            DecisionResult::SAT => {
                let new_assgn = sat.get_difference().filter(|x| x.get_label() != cur_v);
                let r = self.conjoin_implied(new_assgn, BddPtr::true_ptr());
                sat.pop();
                r
            }
            DecisionResult::Unknown => {
                let sub = self.topdown_h(cnf, sat, level + 1, cache);
                let new_assgn = sat.get_difference().filter(|x| x.get_label() != cur_v);
                let r = self.conjoin_implied(new_assgn, sub);
                sat.pop();
                r
            }
        };

        let r = if high_bdd == low_bdd {
            high_bdd
        } else {
            let bdd = BddNode::new(cur_v, low_bdd, high_bdd);
            self.get_or_insert(bdd)
        };
        cache.insert(hashed, r);
        r
    }

    /// compile a decision DNNF top-down from a CNF
    pub fn from_cnf_topdown(&'a self, cnf: &Cnf) -> BddPtr<'a> {
        let mut sat = match SATSolver::new(cnf.clone()) {
            Some(v) => v,
            None => return BddPtr::false_ptr(),
        };

        let mut r = self.topdown_h(cnf, &mut sat, 0, &mut FxHashMap::default());

        // conjoin in any initially implied literals
        for l in sat.get_difference() {
            let node = if l.get_polarity() {
                BddNode::new(l.get_label(), BddPtr::false_ptr(), r)
            } else {
                BddNode::new(l.get_label(), r, BddPtr::false_ptr())
            };
            r = self.get_or_insert(node);
        }
        r
    }

    /// Compute the Boolean function `f | var = value`
    pub fn condition(&'a self, bdd: BddPtr<'a>, lbl: VarLabel, value: bool) -> BddPtr<'a> {
        let r = self.cond_helper(bdd, lbl, value);
        bdd.clear_scratch();
        r
    }

    fn cond_helper(&'a self, bdd: BddPtr<'a>, lbl: VarLabel, value: bool) -> BddPtr<'a> {
        if bdd.is_const() {
            bdd
        } else if bdd.var() == lbl {
            let r = if value { bdd.high() } else { bdd.low() };
            if bdd.is_neg() {
                r.neg()
            } else {
                r
            }
        } else {
            // check cache
            if let Some(v) = bdd.get_scratch::<BddPtr>() {
                return if bdd.is_neg() { v.neg() } else { v };
            }

            // recurse on the children
            let l = self.cond_helper(bdd.low(), lbl, value);
            let h = self.cond_helper(bdd.high(), lbl, value);
            if l == h {
                if bdd.is_neg() {
                    return l.neg();
                } else {
                    return l;
                };
            };
            let res = if l != bdd.low() || h != bdd.high() {
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
            // TODO: fix scratch lifetime issue
            // bdd.set_scratch(if bdd.is_neg() { res.neg() } else { res });
            res
        }
    }

    pub fn num_logically_redundant(&self) -> usize {
        let mut num_collisions = 0;
        let mut seen_hashes = HashSet::new();
        let map = create_semantic_hash_map::<10000000049>(self.order.num_vars());
        for bdd in self.compute_table.borrow().iter() {
            let h = BddPtr::new_reg(bdd).semantic_hash(&self.order, &map);
            if seen_hashes.contains(&(h.value())) {
                num_collisions += 1;
            } else {
                seen_hashes.insert(h.value());
            }
        }
        num_collisions
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
//     assert!(false)
// }
