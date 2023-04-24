//! Top-down decision DNNF compiler and manipulator

use std::{
    collections::HashSet,
    hash::{Hash, Hasher},
};

use crate::{
    backing_store::*,
    repr::{
        bdd::{create_semantic_hash_map, WmcParams},
        ddnnf::DDNNFPtr,
        unit_prop::{DecisionResult, SATSolver},
    },
    util::semiring::FiniteField,
};
use bumpalo::Bump;
use rustc_hash::{FxHashMap, FxHasher};

use crate::{
    backing_store::bump_table::BackedRobinhoodTable,
    repr::{
        bdd::{BddNode, BddPtr},
        cnf::*,
        var_label::{Literal, VarLabel},
        var_order::VarOrder,
    },
};

pub struct BddSemanticUniqueTableHasher<const P: u128> {
    map: WmcParams<FiniteField<P>>,
    order: VarOrder,
}

impl<const P: u128> BddSemanticUniqueTableHasher<P> {
    pub fn new(order: VarOrder, map: WmcParams<FiniteField<P>>) -> Self {
        Self { order, map }
    }
}

impl<const P: u128> UniqueTableHasher<BddNode> for BddSemanticUniqueTableHasher<P> {
    // TODO(matt): we should be able to de-duplicate this with fold/wmc
    fn u64hash(&self, elem: &BddNode) -> u64 {
        let mut hasher = FxHasher::default();

        let (low_w, high_w) = self.map.get_var_weight(elem.var);

        // TODO(matt): investigate if this works properly!
        FiniteField::<P>::new(
            elem.low.semantic_hash(&self.order, &self.map).value() * low_w.value()
            // (P - elem.low().semantic_hash(&self.vtree, &self.map).value() + 1) * low_w.value()
                + elem.high.semantic_hash(&self.order, &self.map).value() * high_w.value(),
        )
        .value()
        .hash(&mut hasher);
        hasher.finish()
    }
}

pub struct DecisionNNFBuilder {
    compute_table: BackedRobinhoodTable<BddNode>,
    hasher: BddSemanticUniqueTableHasher<10000000000063>,
    // hasher: DefaultUniqueTableHasher,
    order: VarOrder,
}

impl DecisionNNFBuilder {
    pub fn new(order: VarOrder) -> DecisionNNFBuilder {
        DecisionNNFBuilder {
            order: order.clone(),
            compute_table: BackedRobinhoodTable::new(),
            // hasher: DefaultUniqueTableHasher::default(),
            hasher: BddSemanticUniqueTableHasher {
                map: create_semantic_hash_map(order.num_vars()),
                order: order,
            },
        }
    }

    /// Normalizes and fetches a node from the store
    fn get_or_insert(&mut self, bdd: BddNode) -> BddPtr {
        if bdd.high.is_neg() {
            let bdd = BddNode::new(bdd.var, bdd.low.neg(), bdd.high.neg());
            BddPtr::new_compl(self.compute_table.get_or_insert(bdd, &self.hasher))
        } else {
            let bdd = BddNode::new(bdd.var, bdd.low, bdd.high);
            BddPtr::new_reg(self.compute_table.get_or_insert(bdd, &self.hasher))
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

    /// Returns A BDD that represents `cnf` conditioned on all
    ///     variables set in the current top model
    /// We need both of these BDDs for sound CNF caching
    /// `cache`: a map from hashed CNFs to their compiled BDDs
    fn topdown_h(
        &mut self,
        cnf: &Cnf,
        sat: &mut SATSolver,
        level: usize,
        cache: &mut FxHashMap<u128, BddPtr>,
    ) -> BddPtr {
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
    pub fn from_cnf_topdown(&mut self, cnf: &Cnf) -> BddPtr {
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
            let _idx = match bdd.get_scratch::<BddPtr>() {
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

    pub fn num_logically_redundant(&self) -> usize {
        let mut num_collisions = 0;
        let mut seen_hashes = HashSet::new();
        let map = create_semantic_hash_map::<10000000049>(self.order.num_vars());
        for bdd in self.compute_table.iter() {
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
