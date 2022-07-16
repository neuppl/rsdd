//! Top-down decision DNNF compiler and manipulator

use std::{collections::HashMap, iter::FromIterator};
use num::Num;

use crate::{repr::{var_label::{VarLabel, Literal}, cnf::*, model::PartialModel}, backing_store::{bdd_table_robinhood::BddTable}, builder::repr::builder_bdd::PointerType};

use super::{repr::builder_bdd::{BddPtr, BddNode, Bdd}, var_order::VarOrder, bdd_builder::BddWmc};

use crate::repr::sat_solver::SATSolver;

const THRESHOLD: usize = 1000;

#[derive(Copy, Clone, Debug)]
enum SampledResult {
    Bdd(BddPtr),
    SampledLit(Literal, f64) // the sampled value and the probability of that sample
}

impl SampledResult {
    fn is_sampled(&self) -> bool {
        match self {
            &SampledResult::SampledLit(_, _) => true,
            _ => false
        }
    }

    fn unwrap_sample(&self) -> (Literal, f64) {
        match self {
            &SampledResult::SampledLit(l, p) => (l, p),
            _ => panic!()
        }
    }


    fn unwrap_bdd(&self) -> BddPtr {
        match self {
            &SampledResult::Bdd(b) => b,
            _ => panic!()
        }
    }
}

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
        sat: &mut SATSolver,
        level: usize,
        order: &VarOrder,
        hasher: &CnfHasher,
        cache: &mut HashMap<HashedCNF, BddPtr>,
    ) -> BddPtr {
        // check for base case
        let assgn = sat.get_implied_units();
        if level >= cnf.num_vars() || cnf.is_sat_partial(&assgn) {
            return self.true_ptr();
        }
        let cur_v = order.var_at_pos(level);

        // check if this literal is currently set in unit propagation; if 
        // it is, skip it
        if assgn.is_set(cur_v) {
            return self.topdown_h(cnf, sat, level+1, order, hasher, cache);
        }

        // check cache
        let hashed = hasher.hash(&assgn);
        match cache.get(&hashed.clone()) {
            None => (),
            Some(v) => {
                return *v;
            }
        }

        // recurse on both values of cur_v
        sat.push();
        sat.decide(Literal::new(cur_v, true));
        let unsat = sat.unsat();
        let high_bdd = if !unsat {
            let new_assgn = sat.get_implied_units();

            let sub = self.topdown_h(cnf, sat, level + 1, order, hasher, cache);
            let implied_lits = new_assgn.get_vec().iter().enumerate().zip(assgn.get_vec()).filter_map(|((idx, new), prev)| {
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

        sat.push();
        sat.decide(Literal::new(cur_v, false));
        let unsat = sat.unsat();
        let low_bdd = if !unsat {
            let new_assgn = sat.get_implied_units();

            let sub = self.topdown_h(cnf, sat, level + 1, order, hasher, cache);
            let implied_lits = new_assgn.get_vec().iter().enumerate().zip(assgn.get_vec()).filter_map(|((idx, new), prev)| {
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
        let mut sat = SATSolver::new(&cnf);
        if sat.unsat() {
            return self.false_ptr()
        }

        let mut r = self.topdown_h(cnf, &mut sat, 0, order, &CnfHasher::new(cnf), &mut HashMap::new());

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

    pub fn topvar(&self, ptr: BddPtr) -> VarLabel {
        self.deref_bdd(ptr).into_node().var
    }


    /// Gets the probability that variable `lbl` is true in `bdd` according to
    /// the weight given in `wmc`
    fn prob_true(&mut self, wmc: &BddWmc<f64>, lbl: VarLabel, bdd: BddPtr) -> f64 {
        let z = self.unsmsoothed_wmc(bdd, wmc);
        let cond = self.condition(bdd, lbl, true);
        let pos_weight = wmc.get_var_weight(lbl).1;
        let p = self.unsmsoothed_wmc(cond, wmc) * pos_weight;
        // println!("ddnnf: {}", self.to_string_debug(bdd));
        // println!("sampling var {:?}, p: {p}, z: {z}", lbl);
        if z > 0.0 {
            p / z
        } else {
            0.0
        }
    }

    /// Returns A BDD that represents `cnf` conditioned on all 
    ///     variables set in the current top model
    /// We need both of these BDDs for sound CNF caching
    /// `cache`: a map from hashed CNFs to their compiled BDDs
    fn topdown_sample_h(
        &mut self,
        cnf: &Cnf,
        sat: &mut SATSolver,
        level: usize,
        order: &VarOrder,
        hasher: &CnfHasher,
        wmc: &BddWmc<f64>,
        cache: &mut HashMap<HashedCNF, BddPtr>,
    ) -> SampledResult {
        use SampledResult::*;
        // check for base case
        let assgn = sat.get_implied_units();
        if level >= cnf.num_vars() || cnf.is_sat_partial(&assgn) {
            return SampledResult::Bdd(self.true_ptr());
        }
        let cur_v = order.var_at_pos(level);

        // check if this literal is currently set in unit propagation; if 
        // it is, skip it
        if assgn.is_set(cur_v) {
            return self.topdown_sample_h(cnf, sat, level+1, order, hasher, wmc, cache);
        }

        // check cache
        let hashed = hasher.hash(&assgn);
        match cache.get(&hashed.clone()) {
            None => (),
            Some(v) => {
                return Bdd(*v);
            }
        }

        fn decide(mgr: &mut DecisionNNFBuilder, 
                 cnf: &Cnf,
                 sat: &mut SATSolver,
                 level: usize,
                 order: &VarOrder,
                 hasher: &CnfHasher,
                 cur_v: VarLabel,
                 assgn: &PartialModel,
                 wmc: &BddWmc<f64>,
                 cache: &mut HashMap<HashedCNF, BddPtr>,
                 polarity: bool) -> SampledResult {
            // recurse on both values of cur_v
            sat.push();
            sat.decide(Literal::new(cur_v, polarity));
            let unsat = sat.unsat();
            if unsat {
                sat.pop();
                return SampledResult::Bdd(mgr.false_ptr());
            } 

            let new_assgn = sat.get_implied_units();
            let r = mgr.topdown_sample_h(cnf, sat, level + 1, order, hasher, wmc, cache);
            sat.pop();
            match r {
                Bdd(sub) => {
                    // check if size threshold exceeded; if so, sample
                    if mgr.count_nodes(sub) > THRESHOLD {
                        let mut rand = rand::thread_rng();
                        let topvar = mgr.topvar(sub);
                        let p = mgr.prob_true(wmc, topvar, sub);
                        let v = rand::Rng::gen_bool(&mut rand, p);
                        // println!("sampled value {:?} = {v} with probability {p}", topvar);
                        return SampledLit(Literal::new(topvar, v), if v { p } else {1.0 - p });
                    }

                    let implied_lits = new_assgn.get_vec().iter().enumerate().zip(assgn.get_vec()).filter_map(|((idx, new), prev)| {
                    if new != prev && idx != cur_v.value_usize() {
                        Some(Literal::new(VarLabel::new_usize(idx), new.unwrap()))
                    } else {
                        None
                    }});
                    Bdd(mgr.conjoin_implied(implied_lits, sub))
                }, 
                SampledLit(_, _) => r
            }
        }

        let high_r = decide(self, cnf, sat, level, order, hasher, cur_v, &assgn, wmc, cache, true);
        let high_bdd = match high_r {
            Bdd(b) => b,
            _ => return high_r 
        };
        let low_r = decide(self, cnf, sat, level, order, hasher, cur_v, &assgn, wmc, cache, false);
        let low_bdd = match low_r {
            Bdd(b) => b,
            _ => return low_r 
        };

        let r = if high_bdd == low_bdd {
            high_bdd
        } else {
            let bdd = BddNode::new(low_bdd, high_bdd, cur_v);
            self.get_or_insert(bdd)
        };
        cache.insert(hashed, r);
        Bdd(r)
    }

    /// Generates `n` top-down samples
    /// Returns a vector of results that are pairs (BddPtr, importance weight)
    pub fn from_cnf_topdown_sample(&mut self, order: &VarOrder, cnf: &Cnf, wmc: &BddWmc<f64>, n: usize) -> Vec<(BddPtr, f64)> {
        let cache = &mut HashMap::new();
        let hasher = CnfHasher::new(cnf);
        let mut res = Vec::new();

        for _ in 0..n {
            let mut sat = SATSolver::new(&cnf);
            // let cache = &mut HashMap::new();
            let mut r = self.topdown_sample_h(cnf, &mut sat, 0, order, &hasher, wmc, cache);
            let mut prob_q = 1.0; // proposal probability
            let mut prob_p = 1.0; // unnormalized probability
            let mut n = 1;
            while r.is_sampled() {
                n = n + 1;
                // let cache = &mut HashMap::new();
                let (sampled_lit, prob) = r.unwrap_sample();
                prob_q = prob_q * prob;
                prob_p = prob_p * (if sampled_lit.get_polarity() { wmc.get_var_weight(sampled_lit.get_label()).1 } else { wmc.get_var_weight(sampled_lit.get_label()).0 });
                sat.decide(sampled_lit);
                r = self.topdown_sample_h(cnf, &mut sat, 0, order, &hasher, wmc, cache);
            }
            let mut bdd = r.unwrap_bdd();
            // println!("prob p: {}, prob q: {}, # samples: {n}, bdd: {}",  prob_p, prob_q, self.to_string_debug(bdd));

            // conjoin in any initially implied literals
            for l in sat.get_implied_units().assignment_iter() {
                let node = if l.get_polarity() { 
                    BddNode::new(self.false_ptr(), bdd, l.get_label())
                } else {
                    BddNode::new(bdd, self.false_ptr(), l.get_label())
                };
                bdd = self.get_or_insert(node);
            }
            res.push((bdd, prob_p / prob_q));
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

    pub fn estimate_marginal(&mut self, n: usize, order: &VarOrder, query_var: VarLabel, wmc: &BddWmc<f64>, cnf: &Cnf) -> f64 {
        let c2 = self.from_cnf_topdown_sample(order, &cnf, &wmc, n);

        let z = c2.iter().fold(0.0, |acc, (_, w)| {
            acc + w
        });

        let p = c2.iter().fold(0.0, |acc, (bdd, w)| {
            let p_bdd = self.prob_true(&wmc, query_var, *bdd);
            acc + (w * p_bdd)
        });

        return p / z;
    }
}


#[test]
fn test_dnnf() {
       let clauses = vec![
            vec![
                Literal::new(VarLabel::new(0), true),
                Literal::new(VarLabel::new(1), false)
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

#[test]
fn test_dnnf_sample() {
    let clauses = vec![
        vec![
            Literal::new(VarLabel::new(0), true),
            Literal::new(VarLabel::new(1), true),
            Literal::new(VarLabel::new(2), true),
        ],
        vec![
            Literal::new(VarLabel::new(0), false),
            Literal::new(VarLabel::new(1), true),
            Literal::new(VarLabel::new(3), true),
            Literal::new(VarLabel::new(6), false),
        ],
        vec![
            Literal::new(VarLabel::new(1), false),
            Literal::new(VarLabel::new(3), true),
            Literal::new(VarLabel::new(4), false),
            Literal::new(VarLabel::new(5), false),
            Literal::new(VarLabel::new(6), false),
        ]
    ];
    let cnf = Cnf::new(clauses);
    let mut mgr = DecisionNNFBuilder::new(cnf.num_vars());
    let weight_map : HashMap<VarLabel, (f64, f64)> = HashMap::from_iter(
        (0..cnf.num_vars()).map(|x| (VarLabel::new(x as u64), (0.2, 0.8))));
    let wmc : BddWmc<f64> = BddWmc::new_with_default(0.0, 1.0, weight_map);
    let order = VarOrder::linear_order(cnf.num_vars());

    let p = mgr.estimate_marginal(1000, &order, VarLabel::new_usize(0), &wmc, &cnf);

    println!("sample prob result: {p}");
    assert!(false);

    // let p = c2.iter().fold(0.0, |((_, w), acc)| {
    //     acc + w
    // });

    // println!("c2: {}", mgr.to_string_debug(c2));
}