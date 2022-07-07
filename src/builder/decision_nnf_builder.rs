use std::collections::HashMap;
use num::Num;

use crate::{repr::{var_label::{VarLabel, Literal}, cnf::*, model::PartialModel}, backing_store::{bdd_table_robinhood::BddTable}, builder::repr::builder_bdd::PointerType};

use super::{repr::builder_bdd::{BddPtr, BddNode, Bdd}, var_order::VarOrder, bdd_builder::BddWmc};

use picorust::picosat;

#[derive(Clone)]
pub struct DecisionNNFBuilder {
    compute_table: BddTable,
}

impl DecisionNNFBuilder {
    pub fn new(num_vars: usize) -> DecisionNNFBuilder {
        DecisionNNFBuilder { compute_table: BddTable::new(num_vars) }
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
    fn clear_scratch(&mut self, ptr: BddPtr) -> () {
        if ptr.is_const() {
            return;
        } else {
            if self.compute_table.get_scratch(ptr).is_none() {
                return;
            } else {
                self.compute_table.set_scratch(ptr, None);
                self.clear_scratch(self.low(ptr));
                self.clear_scratch(self.high(ptr));
            }
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
        up: &mut UnitPropagate,
        level: usize,
        order: &VarOrder,
        hasher: &CnfHasher,
        cache: &mut HashMap<HashedCNF, BddPtr>,
    ) -> BddPtr {
        // check for base case
        if level >= cnf.num_vars() || cnf.is_sat_partial(up.get_assgn()) {
            return self.true_ptr();
        }
        let cur_v = order.var_at_pos(level);

        // check if this literal is currently set in unit propagation; if 
        // it is, skip it
        if up.get_assgn().is_set(cur_v) {
            return self.topdown_h(cnf, up, level+1, order, hasher, cache);
        }

        // check cache
        let hashed = hasher.hash(up.get_assgn());
        match cache.get(&hashed.clone()) {
            None => (),
            Some(v) => {
                return *v;
            }
        }

        // recurse on both values of cur_v
        let success = up.decide(Literal::new(cur_v, true));
        let high_bdd = if success {
            let sub = self.topdown_h(cnf, up, level + 1, order, hasher, cache);
            let lits = up.get_decided_literals().filter(|l| l.get_label() != cur_v);
            self.conjoin_implied(lits, sub)
        } else {
            self.false_ptr()
        };

        up.backtrack();
        let success = up.decide(Literal::new(cur_v, false));
        let low_bdd = if success {
            let sub = self.topdown_h(cnf, up, level + 1, order, hasher, cache);
            let lits = up.get_decided_literals().filter(|l| l.get_label() != cur_v);
            self.conjoin_implied(lits, sub)
        } else {
            self.false_ptr()
        };


        up.backtrack();
        let r = if high_bdd == low_bdd {
            high_bdd
        } else {
            let bdd = BddNode::new(low_bdd, high_bdd, cur_v);
            self.get_or_insert(bdd)
        };
        cache.insert(hashed, r);
        r
    }

    pub fn from_cnf_topdown(&mut self, order: &VarOrder, cnf: &Cnf) -> BddPtr {
        let mut up = match UnitPropagate::new(cnf) {
            Some(v) => v,
            None => return self.false_ptr(),
        };
        let mut r = self.topdown_h(cnf, &mut up, 0, order, &CnfHasher::new(cnf), &mut HashMap::new());

        // conjoin in any initially implied literals
        for l in up.get_assgn().assignment_iter() {
            let node = if l.get_polarity() { 
                BddNode::new(self.false_ptr(), r, l.get_label())
            } else {
                BddNode::new(r, self.false_ptr(), l.get_label())
            };
            r = self.get_or_insert(node);
        }
        r
    }

    fn topdown_sample_h(
        &mut self,
        cnf: &Cnf,
        up: &mut UnitPropagate,
        level: usize,
        order: &VarOrder,
        hasher: &CnfHasher,
        sampled_vars: &mut PartialModel,
        cache: &mut HashMap<HashedCNF, BddPtr>,
    ) -> BddPtr {
        // check for base case
        if level >= cnf.num_vars() || cnf.is_sat_partial(up.get_assgn()) {
            return self.true_ptr();
        }
        let cur_v = order.var_at_pos(level);

        // check if this literal is currently set in unit propagation; if 
        // it is, skip it
        if up.get_assgn().is_set(cur_v) {
            return self.topdown_h(cnf, up, level+1, order, hasher, cache);
        }

        // check cache
        let hashed = hasher.hash(up.get_assgn());
        match cache.get(&hashed.clone()) {
            None => (),
            Some(v) => {
                return *v;
            }
        }

        // recurse on both values of cur_v
        let success = up.decide(Literal::new(cur_v, true));
        let high_bdd = if success {
            let sub = self.topdown_h(cnf, up, level + 1, order, hasher, cache);
            let lits = up.get_decided_literals().filter(|l| l.get_label() != cur_v);
            self.conjoin_implied(lits, sub)
        } else {
            self.false_ptr()
        };

        up.backtrack();
        let success = up.decide(Literal::new(cur_v, false));
        let low_bdd = if success {
            let sub = self.topdown_h(cnf, up, level + 1, order, hasher, cache);
            let lits = up.get_decided_literals().filter(|l| l.get_label() != cur_v);
            self.conjoin_implied(lits, sub)
        } else {
            self.false_ptr()
        };


        up.backtrack();
        let r = if high_bdd == low_bdd {
            high_bdd
        } else {
            let bdd = BddNode::new(low_bdd, high_bdd, cur_v);
            self.get_or_insert(bdd)
        };
        cache.insert(hashed, r);
        r
    }

    pub fn from_cnf_topdown_sample(&mut self, order: &VarOrder, cnf: &Cnf) -> BddPtr {
        let mut up = match UnitPropagate::new(cnf) {
            Some(v) => v,
            None => return self.false_ptr(),
        };
        let mut r = self.topdown_h(cnf, &mut up, 0, order, &CnfHasher::new(cnf), &mut HashMap::new());

        // conjoin in any initially implied literals
        for l in up.get_assgn().assignment_iter() {
            let node = if l.get_polarity() { 
                BddNode::new(self.false_ptr(), r, l.get_label())
            } else {
                BddNode::new(r, self.false_ptr(), l.get_label())
            };
            r = self.get_or_insert(node);
        }
        r
    }



    /// Fill the scratch space of each node with a counter that
    /// indexes an in-order left-first depth-first traversal
    /// returns the new count (will be #nodes in the BDD at the end)
    ///
    /// Pre-condition: cleared scratch
    fn unique_label_nodes(&mut self, ptr: BddPtr, count: usize) -> usize {
        if ptr.is_const() {
            return count;
        } else {
            if self.compute_table.get_scratch(ptr).is_some() {
                return count;
            } else {
                self.compute_table.set_scratch(ptr, Some(count));
                let new_count = self.unique_label_nodes(self.low(ptr), count + 1);
                self.unique_label_nodes(self.high(ptr), new_count)
            }
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
            PointerType::PtrTrue => wmc.one.clone(),
            PointerType::PtrFalse => wmc.zero.clone(),
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
                        let res = (low_v * low_factor.clone()) + (high_v * high_factor.clone());
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

    pub fn count_nodes(&mut self, ptr: BddPtr) -> usize {
        let s = self.unique_label_nodes(ptr, 0);
        self.clear_scratch(ptr);
        s
    }

    /// Weighted-model count
    pub fn unsmsoothed_wmc<T: Num + Clone + core::fmt::Debug + Copy>(&mut self, ptr: BddPtr, params: &BddWmc<T>) -> T {
        let n = self.unique_label_nodes(ptr, 0);
        let mut v = vec![(None, None); n];
        let r = self.wmc_helper(ptr, params, true, &mut v);
        self.clear_scratch(ptr);
        r
    }
}