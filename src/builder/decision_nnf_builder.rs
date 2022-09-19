//! Top-down decision DNNF compiler and manipulator

use num::Num;
use rand::Rng;
use std::collections::HashMap;

use crate::{
    backing_store::bdd_table_robinhood::BddTable,
    builder::repr::builder_bdd::PointerType,
    repr::{
        cnf::*,
        var_label::{Literal, VarLabel},
    },
};

use super::{
    bdd_builder::BddWmc,
    repr::builder_bdd::{Bdd, BddNode, BddPtr},
    var_order::VarOrder,
};

use crate::repr::sat_solver::SATSolver;

#[derive(Clone)]
pub struct DecisionNNFBuilder {
    compute_table: BddTable,
}

impl DecisionNNFBuilder {
    pub fn new(num_vars: usize) -> DecisionNNFBuilder {
        DecisionNNFBuilder {
            compute_table: BddTable::new(num_vars),
        }
    }

    pub fn true_ptr(&self) -> BddPtr {
        BddPtr::true_node()
    }

    pub fn false_ptr(&self) -> BddPtr {
        BddPtr::false_node()
    }

    /// Dereference a BddPtr into a Bdd
    fn deref_bdd(&self, ptr: BddPtr) -> Bdd {
        self.compute_table.deref(ptr)
    }

    /// Fetch the BDD pointed to by the low-node of `ptr`
    /// panics on constant BDDs
    pub fn low(&self, ptr: BddPtr) -> BddPtr {
        assert!(
            !ptr.is_const(),
            "Attempting to get low pointer of constant BDD"
        );
        let b = self.deref_bdd(ptr).into_node();
        b.low
    }

    /// Fetch the BDD pointed to by the high-node of `ptr`, panics on constant
    /// BDDs
    pub fn high(&self, ptr: BddPtr) -> BddPtr {
        assert!(
            !ptr.is_const(),
            "Attempting to get high pointer of constant BDD"
        );
        let b = self.deref_bdd(ptr).into_node();
        b.high
    }

    /// normalizes and fetches a node from the store
    fn get_or_insert(&mut self, bdd: BddNode) -> BddPtr {
        if bdd.high.is_compl() {
            let bdd = Bdd::new_node(bdd.low.neg(), bdd.high.neg(), bdd.var);
            self.compute_table.get_or_insert(bdd).neg()
        } else {
            let bdd = Bdd::new_node(bdd.low, bdd.high, bdd.var);
            self.compute_table.get_or_insert(bdd)
        }
    }

    pub fn to_string_debug(&self, ptr: BddPtr) -> String {
        fn print_bdd_helper(t: &DecisionNNFBuilder, ptr: BddPtr) -> String {
            match ptr.ptr_type() {
                PointerType::PtrTrue => String::from("T"),
                PointerType::PtrFalse => String::from("F"),
                PointerType::PtrNode => {
                    let l_p = if ptr.is_compl() {
                        t.low(ptr).neg()
                    } else {
                        t.low(ptr)
                    };
                    let h_p = if ptr.is_compl() {
                        t.high(ptr).neg()
                    } else {
                        t.high(ptr)
                    };
                    let l_s = print_bdd_helper(t, l_p);
                    let h_s = print_bdd_helper(t, h_p);
                    format!("({}, {}, {})", ptr.var(), h_s, l_s)
                }
            }
        }
        print_bdd_helper(self, ptr)
    }

    pub fn negate(&mut self, ptr: BddPtr) -> BddPtr {
        ptr.neg()
    }

    /// Clear the internal scratch space for a BddPtr
    fn clear_scratch(&mut self, ptr: BddPtr) {
        if ptr.is_const() {
        } else if self.compute_table.get_scratch(ptr).is_none() {
        } else {
            self.compute_table.set_scratch(ptr, None);
            self.clear_scratch(self.low(ptr));
            self.clear_scratch(self.high(ptr));
        }
    }

    /// Get a pointer to the variable with label `lbl` and polarity `polarity`
    pub fn var(&mut self, lbl: VarLabel, polarity: bool) -> BddPtr {
        let bdd = BddNode::new(BddPtr::false_node(), BddPtr::true_node(), lbl);
        let r = self.get_or_insert(bdd);
        if polarity {
            r
        } else {
            r.neg()
        }
    }

    fn conjoin_implied(&mut self, literals: impl Iterator<Item = Literal>, nnf: BddPtr) -> BddPtr {
        if nnf.is_false() {
            return self.false_ptr();
        }
        let mut sub = nnf;
        for l in literals {
            let node = if l.get_polarity() {
                BddNode::new(self.false_ptr(), sub, l.get_label())
            } else {
                BddNode::new(sub, self.false_ptr(), l.get_label())
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
        order: &VarOrder,
        hasher: &mut CnfHasher,
        cache: &mut HashMap<HashedCNF, BddPtr>,
    ) -> BddPtr {
        // check for base case
        let assgn = sat.get_implied_units();
        if level >= cnf.num_vars() || cnf.is_sat_partial(&assgn) {
            return self.true_ptr();
        }
        let cur_v = order.var_at_level(level);

        // check if this literal is currently set in unit propagation; if
        // it is, skip it
        if assgn.is_set(cur_v) {
            return self.topdown_h(cnf, sat, level + 1, order, hasher, cache);
        }

        // check cache
        let hashed = hasher.hash(&assgn);
        match cache.get(&hashed) {
            None => (),
            Some(v) => {
                return *v;
            }
        }

        // recurse on both values of cur_v
        sat.push();
        hasher.push();
        sat.decide(Literal::new(cur_v, true));
        hasher.decide(Literal::new(cur_v, true));
        let unsat = sat.unsat_unit();
        let high_bdd = if !unsat {
            let new_assgn = sat.get_implied_units();

            let sub = self.topdown_h(cnf, sat, level + 1, order, hasher, cache);
            let implied_lits = new_assgn
                .get_vec()
                .iter()
                .enumerate()
                .zip(assgn.get_vec())
                .filter_map(|((idx, new), prev)| {
                    if new != prev && idx != cur_v.value_usize() {
                        Some(Literal::new(VarLabel::new_usize(idx), new.unwrap()))
                    } else {
                        None
                    }
                });
            self.conjoin_implied(implied_lits, sub)
        } else {
            self.false_ptr()
        };
        sat.pop();
        hasher.pop();

        sat.push();
        hasher.push();
        sat.decide(Literal::new(cur_v, false));
        hasher.decide(Literal::new(cur_v, false));
        let unsat = sat.unsat_unit();
        let low_bdd = if !unsat {
            let new_assgn = sat.get_implied_units();

            let sub = self.topdown_h(cnf, sat, level + 1, order, hasher, cache);
            let implied_lits = new_assgn
                .get_vec()
                .iter()
                .enumerate()
                .zip(assgn.get_vec())
                .filter_map(|((idx, new), prev)| {
                    if new != prev && idx != cur_v.value_usize() {
                        Some(Literal::new(VarLabel::new_usize(idx), new.unwrap()))
                    } else {
                        None
                    }
                });
            self.conjoin_implied(implied_lits, sub)
        } else {
            self.false_ptr()
        };
        sat.pop();
        hasher.pop();

        let r = if high_bdd == low_bdd {
            high_bdd
        } else {
            let bdd = BddNode::new(low_bdd, high_bdd, cur_v);
            self.get_or_insert(bdd)
        };
        cache.insert(hashed, r);
        r
    }

    /// compile a decision DNNF top-down from a CNF with the searching order
    /// specified by `order`
    pub fn from_cnf_topdown(&mut self, order: &VarOrder, cnf: &Cnf) -> BddPtr {
        let mut sat = SATSolver::new(cnf);
        if sat.unsat_unit() {
            return self.false_ptr();
        }

        let mut r = self.topdown_h(
            cnf,
            &mut sat,
            0,
            order,
            &mut CnfHasher::new(cnf),
            &mut HashMap::new(),
        );

        // conjoin in any initially implied literals
        for l in sat.get_implied_units().assignment_iter() {
            let node = if l.get_polarity() {
                BddNode::new(self.false_ptr(), r, l.get_label())
            } else {
                BddNode::new(r, self.false_ptr(), l.get_label())
            };
            r = self.get_or_insert(node);
        }
        r
    }

    /// get the top variable in a ptr
    pub fn topvar(&self, ptr: BddPtr) -> VarLabel {
        self.deref_bdd(ptr).into_node().var
    }

    /// Gets the probability that variable `lbl` is true in `bdd` according to
    /// the weight given in `wmc`
    /// TODO: resolve dead code
    #[allow(dead_code)]
    fn prob_true(&mut self, wmc: &BddWmc<f64>, lbl: VarLabel, bdd: BddPtr) -> f64 {
        let z = self.unsmsoothed_wmc(bdd, wmc);
        let cond = self.condition(bdd, lbl, true);
        let pos_weight = wmc.get_var_weight(lbl).1;
        let p = self.unsmsoothed_wmc(cond, wmc) * pos_weight;
        if z > 0.0 {
            p / z
        } else {
            0.0
        }
    }

    /// Generates `n` top-down samples
    /// Returns a vector of results that are pairs (BddPtr, importance weight)
    pub fn from_cnf_topdown_sample(
        &mut self,
        order: &VarOrder,
        cnf: &Cnf,
        _wmc: &BddWmc<f64>,
        n: usize,
    ) -> Vec<(BddPtr, f64)> {
        let cache = &mut HashMap::new();
        let mut hasher = CnfHasher::new(cnf);
        let mut res = Vec::new();
        let mut rng = rand::thread_rng();
        let num_samples = 25;

        for _ in 0..n {
            let mut sat = SATSolver::new(cnf);
            for i in 100..num_samples {
                sat.decide(Literal::new(VarLabel::new_usize(i), rng.gen_bool(0.5)));
            }
            let r = self.topdown_h(cnf, &mut sat, 0, order, &mut hasher, cache);
            res.push((r, 0.5));
        }
        res
    }

    /// Fill the scratch space of each node with a counter that
    /// indexes an in-order left-first depth-first traversal
    /// returns the new count (will be #nodes in the BDD at the end)
    ///
    /// Pre-condition: cleared scratch
    fn unique_label_nodes(&mut self, ptr: BddPtr, count: usize) -> usize {
        if ptr.is_const() {
            count
        } else if self.compute_table.get_scratch(ptr).is_some() {
            count
        } else {
            self.compute_table.set_scratch(ptr, Some(count));
            let new_count = self.unique_label_nodes(self.low(ptr), count + 1);
            self.unique_label_nodes(self.high(ptr), new_count)
        }
    }

    /// Pre-condition: Nodes are unique numbered
    fn wmc_helper<T: Num + Clone + core::fmt::Debug + Copy>(
        &mut self,
        ptr: BddPtr,
        wmc: &BddWmc<T>,
        smooth: bool,
        tbl: &mut Vec<(Option<T>, Option<T>)>, // (non-compl, compl)
    ) -> T {
        match ptr.ptr_type() {
            PointerType::PtrTrue => wmc.one,
            PointerType::PtrFalse => wmc.zero,
            PointerType::PtrNode => {
                let idx = self.compute_table.get_scratch(ptr).unwrap();
                // let order = self.get_order();
                match tbl[idx] {
                    (_, Some(v)) if ptr.is_compl() => v,
                    (Some(v), _) if !ptr.is_compl() => v,
                    (cur_reg, cur_compl) => {
                        let bdd = self.deref_bdd(ptr).into_node();
                        let (low, high) = if ptr.is_compl() {
                            (bdd.low.neg(), bdd.high.neg())
                        } else {
                            (bdd.low, bdd.high)
                        };
                        let low_v = self.wmc_helper(low, wmc, smooth, tbl);
                        let high_v = self.wmc_helper(high, wmc, smooth, tbl);
                        // compute new
                        let (low_factor, high_factor) = wmc.get_var_weight(bdd.var);
                        let res = (low_v * *low_factor) + (high_v * *high_factor);
                        tbl[idx] = if ptr.is_compl() {
                            (cur_reg, Some(res))
                        } else {
                            (Some(res), cur_compl)
                        };
                        res
                    }
                }
            }
        }
    }

    /// count the number of nodes in the decision DNNF
    pub fn count_nodes(&mut self, ptr: BddPtr) -> usize {
        let s = self.unique_label_nodes(ptr, 0);
        self.clear_scratch(ptr);
        s
    }

    /// Weighted-model count
    pub fn unsmsoothed_wmc<T: Num + Clone + core::fmt::Debug + Copy>(
        &mut self,
        ptr: BddPtr,
        params: &BddWmc<T>,
    ) -> T {
        let n = self.unique_label_nodes(ptr, 0);
        let mut v = vec![(None, None); n];
        let r = self.wmc_helper(ptr, params, true, &mut v);
        self.clear_scratch(ptr);
        r
    }

    /// Compute the Boolean function `f | var = value`
    pub fn condition(&mut self, bdd: BddPtr, lbl: VarLabel, value: bool) -> BddPtr {
        let n = self.unique_label_nodes(bdd, 0);
        let r = self.cond_helper(bdd, lbl, value, &mut vec![None; n]);
        self.clear_scratch(bdd);
        r
    }

    fn cond_helper(
        &mut self,
        bdd: BddPtr,
        lbl: VarLabel,
        value: bool,
        cache: &mut Vec<Option<BddPtr>>,
    ) -> BddPtr {
        if bdd.is_const() {
            bdd
        } else if bdd.label() == lbl {
            let node = self.deref_bdd(bdd).into_node();
            let r = if value { node.high } else { node.low };
            if bdd.is_compl() {
                r.neg()
            } else {
                r
            }
        } else {
            // check cache
            let idx = self.compute_table.get_scratch(bdd).unwrap();
            match cache[idx] {
                None => (),
                Some(v) => return if bdd.is_compl() { v.neg() } else { v },
            };

            // recurse on the children
            let n = self.deref_bdd(bdd).into_node();
            let l = self.cond_helper(n.low, lbl, value, cache);
            let h = self.cond_helper(n.high, lbl, value, cache);
            if l == h {
                if bdd.is_compl() {
                    return l.neg();
                } else {
                    return l;
                };
            };
            let res = if l != n.low || h != n.high {
                // cache and return the new BDD
                let new_bdd = BddNode {
                    low: l,
                    high: h,
                    var: bdd.label(),
                };
                let r = self.get_or_insert(new_bdd);
                if bdd.is_compl() {
                    r.neg()
                } else {
                    r
                }
            } else {
                // nothing changed
                bdd
            };
            cache[idx] = Some(if bdd.is_compl() { res.neg() } else { res });
            res
        }
    }
}

#[test]
fn test_dnnf() {
    let clauses = vec![
        vec![
            Literal::new(VarLabel::new(0), true),
            Literal::new(VarLabel::new(1), false),
        ],
        // vec![
        // Literal::new(VarLabel::new(0), false),
        // Literal::new(VarLabel::new(1), true)
        // ]
    ];
    let cnf = Cnf::new(clauses);
    let mut mgr = DecisionNNFBuilder::new(cnf.num_vars());
    let c2 = mgr.from_cnf_topdown(&VarOrder::linear_order(cnf.num_vars()), &cnf);
    println!("c2: {}", mgr.to_string_debug(c2));
}
