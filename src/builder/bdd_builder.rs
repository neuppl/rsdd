//! Primary interface for manipulating and constructing BDDs. Contains the BDD
//! manager, which manages the global state necessary for constructing canonical
//! binary decision diagrams.

use crate::repr::bdd::{BddPtr, BddNode};
use crate::repr::dtree;
use crate::repr::model::PartialModel;
use crate::repr::sat_solver::SATSolver;
use crate::repr::var_order::VarOrder;
use crate::{
    backing_store::bump_table::BumpTable,
    builder::cache::bdd_app::*,
    repr, repr::cnf::*, repr::logical_expr::LogicalExpr, repr::model,
    repr::var_label::Literal, repr::var_label::VarLabel,
};

use crate::backing_store::*;
use bit_set::BitSet;
use num::traits::Num;
use rand::rngs::ThreadRng;
use rand::Rng;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::{BinaryHeap, HashSet};
use std::fmt::Debug;
use std::iter::FromIterator;

use super::bdd_plan::BddPlan;

#[derive(Eq, PartialEq, Debug)]
struct CompiledCNF {
    ptr: BddPtr,
    sz: usize,
}

// The priority queue depends on `Ord`.
// Explicitly implement the trait so the queue becomes a min-heap
// instead of a max-heap.
impl Ord for CompiledCNF {
    fn cmp(&self, other: &Self) -> Ordering {
        // Notice that the we flip the ordering on costs.
        // In case of a tie we compare positions - this step is necessary
        // to make implementations of `PartialEq` and `Ord` consistent.
        other
            .sz
            .cmp(&self.sz)
    }
}

// `PartialOrd` needs to be implemented as well.
impl PartialOrd for CompiledCNF {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
pub struct Assignment {
    assignments: Vec<bool>,
}

impl Assignment {
    pub fn new(assignments: Vec<bool>) -> Assignment {
        Assignment { assignments }
    }

    pub fn get_assignment(&self, var: VarLabel) -> bool {
        self.assignments[var.value() as usize]
    }
}


/// An auxiliary data structure for tracking statistics about BDD manager
/// performance (for fine-tuning)
struct BddManagerStats {
    /// For now, always track the number of recursive calls. In the future,
    /// this should probably be gated behind a debug build (since I suspect
    /// it may have non-trivial performance overhead and synchronization cost)
    num_recursive_calls: usize,
}

impl BddManagerStats {
    pub fn new() -> BddManagerStats {
        BddManagerStats {
            num_recursive_calls: 0,
        }
    }
}

pub struct BddManager {
    compute_table: BumpTable<BddNode>,
    apply_table: BddApplyTable,
    stats: BddManagerStats,
    order: VarOrder,
}

impl BddManager {
    /// Make a BDD manager with a default variable ordering
    pub fn new_default_order(num_vars: usize) -> BddManager {
        let default_order = VarOrder::linear_order(num_vars);
        BddManager::new(default_order)
    }


    /// Creates a new variable manager with the specified order
    pub fn new(order: VarOrder) -> BddManager {
        let l = order.num_vars();
        BddManager {
            compute_table: BumpTable::new(),
            order,
            apply_table: BddApplyTable::new(l),
            stats: BddManagerStats::new(),
        }
    }

    /// Returns the number of variables in the manager
    pub fn num_vars(&self) -> usize {
        return self.get_order().num_vars();
    }

    /// Generate a new variable which was not in the original order. Places the
    /// new variable at the end of the current order. Returns the label of the
    /// new variable.
    pub fn new_var(&mut self) -> VarLabel {
        self.apply_table.push_table();
        self.order.new_last()
    }

    /// Get the current variable order
    pub fn get_order(&self) -> &VarOrder {
        &self.order
    }

    /// Get a pointer to the variable with label `lbl` and polarity `polarity`
    pub fn var(&mut self, lbl: VarLabel, polarity: bool) -> BddPtr {
        let bdd = BddNode::new(lbl, BddPtr::false_ptr(), BddPtr::true_ptr());
        let r = self.get_or_insert(bdd);
        if polarity {
            r
        } else {
            r.compl()
        }
    }

    /// Normalizes and fetches a node from the store
    fn get_or_insert(&mut self, bdd: BddNode) -> BddPtr {
        if bdd.high.is_compl() {
            let bdd = BddNode::new(bdd.var, bdd.low.compl(), bdd.high.compl());
            BddPtr::new_compl(self.compute_table.get_or_insert(bdd)) 
        } else {
            let bdd = BddNode::new(bdd.var, bdd.low, bdd.high);
            BddPtr::new_reg(self.compute_table.get_or_insert(bdd))
        }
    }

    /// Compose `g` into `f` by substituting for `lbl`
    pub fn compose(&mut self, f: BddPtr, lbl: VarLabel, g: BddPtr) -> BddPtr {
        // TODO this can be optimized with a specialized implementation to make
        // it a single traversal
        let var = self.var(lbl, true);
        let iff = self.iff(var, g);
        let a = self.and(iff, f);

        self.exists(a, lbl)
    }

    // condition a BDD *only* if the top variable is `v`; used in `ite`
    fn condition_essential(&self, f: BddPtr, lbl: VarLabel, v: bool) -> BddPtr {
        if f.is_const() || f.var() != lbl {
            return f;
        };
        let r = if v { f.high() } else { f.low() };
        if f.is_compl() {
            r.compl()
        } else {
            r
        }
    }

    fn ite_helper(&mut self, f: BddPtr, g: BddPtr, h: BddPtr) -> BddPtr {
        self.stats.num_recursive_calls += 1;
        // a wise man once said: there are parts of the code that are easier to
        // prove correct than they are to debug or test. This is one of those
        // parts.  Is it proven correct? Unfortunately, no.
        //
        // standardize
        // See pgs. 115-117 of "Algorithms and Data Structures in VLSI Design"
        // attempt a base case
        match self.apply_table.get(f, g, h) {
            Some(v) => return v,
            None => (),
        };
        let f_orig = f;
        let g_orig = g;
        let h_orig = h;

        match (f, g, h) {
            (_, g, h) if g.is_false() && h.is_false() => return BddPtr::false_ptr(),
            (_, g, h) if g.is_true() && h.is_true() => return BddPtr::true_ptr(),
            (f, g, h) if g.is_true() && h.is_false() => return f,
            (f, g, h) if g.is_false() && h.is_true() => return f.compl(),
            (f, g, _) if f.is_true() => return g,
            (f, _, h) if f.is_false() => return h,
            (_, g, h) if g == h => return g,
            _ => (),
        };

        // attempt to place the variable that comes first in the order as f
        let (f, g, h) = match (f, g, h) {
            (f, g, h) if g.is_true() && self.get_order().lt(h.var(), f.var()) => (h, g, f),
            (f, g, h) if h.is_false() && self.get_order().lt(g.var(), f.var()) => (g, f, h),
            (f, g, h) if h.is_true() && self.get_order().lt(g.var(), f.var()) => {
                (g.compl(), f.compl(), h)
            }
            (f, g, h) if g.is_false() && self.get_order().lt(h.var(), f.var()) => {
                (h.compl(), g, f.compl())
            }
            (f, g, h) if g == h && self.get_order().lt(g.var(), f.var()) => (g, f, f.compl()),
            _ => (f, g, h),
        };

        // ok the work!
        // find the first essential variable for f, g, or h
        let lbl = self.get_order().first_essential(f, g, h);
        let fx = self.condition_essential(f, lbl, true);
        let gx = self.condition_essential(g, lbl, true);
        let hx = self.condition_essential(h, lbl, true);
        let fxn = self.condition_essential(f, lbl, false);
        let gxn = self.condition_essential(g, lbl, false);
        let hxn = self.condition_essential(h, lbl, false);
        let t = self.ite(fx, gx, hx);
        let f = self.ite(fxn, gxn, hxn);

        if t == f {
            return t;
        };

        // now we have a new BDD
        let node = BddNode::new(lbl, f, t);
        let r = self.get_or_insert(node);
        self.apply_table.insert(f_orig, g_orig, h_orig, r);
        r
    }

    /// if f then g else h
    pub fn ite(&mut self, f: BddPtr, g: BddPtr, h: BddPtr) -> BddPtr {
        self.ite_helper(f, g, h)
    }

    /// Produce a new BDD that is the result of conjoining `f` and `g`
    /// ```
    /// # use rsdd::builder::bdd_builder::BddManager;
    /// # use rsdd::repr::var_label::VarLabel;
    /// let mut mgr = BddManager::new_default_order(10);
    /// let lbl_a = mgr.new_var();
    /// let a = mgr.var(lbl_a, true);
    /// let a_and_not_a = mgr.and(a, a.neg());
    /// assert!(mgr.is_false(a_and_not_a));
    /// ```
    pub fn and(&mut self, f: BddPtr, g: BddPtr) -> BddPtr {
        self.stats.num_recursive_calls += 1;
        // base case
        let reg_f = f.to_reg();
        let reg_g = g.to_reg();
        if reg_f == reg_g {
            if f == g {
                return f;
            } else {
                return BddPtr::false_ptr();
            }
        }
        if reg_f.is_true() {
            if f.is_true() {
                return g;
            } else {
                return f;
            }
        }
        if reg_g.is_true() {
            if g.is_true() {
                return f;
            } else {
                return g;
            }
        }

        // now, both of the nodes are not constant
        // normalize the nodes to increase cache efficiency
        //
        // TODO is this a redundant normalization?
        // let (f, g, reg_f, _) = if reg_f < reg_g {
        //     (f, g, reg_f, reg_g)
        // } else {
        //     (g, f, reg_g, reg_f)
        // };

        // check the cache
        match self.apply_table.get(f, g, BddPtr::false_ptr()) {
            Some(v) => {
                return v;
            }
            None => {}
        };

        // now we know that these are nodes, compute the cofactors
        let topf = self.get_order().get(f.var());
        let topg = self.get_order().get(g.var());
        let index; // will hold the top variable
        let mut fv;
        let mut gv;
        let mut fnv;
        let mut gnv;
        if topf <= topg {
            index = f.var();
            fv = reg_f.high();
            fnv = reg_f.low();
            if f.is_compl() {
                fv = fv.compl();
                fnv = fnv.compl();
            }
        } else {
            index = g.var();
            fv = f;
            fnv = f;
        }

        if topg <= topf {
            gv = g.high();
            gnv = g.low();
            if g.is_compl() {
                gv = gv.compl();
                gnv = gnv.compl();
            }
        } else {
            gv = g;
            gnv = g;
        }

        // now recurse
        let new_h = self.and(fv, gv);
        let new_l = self.and(fnv, gnv);

        // now normalize the result
        if new_h == new_l {
            new_h
        } else {
            let n = BddNode::new(index, new_l, new_h);
            let r = self.get_or_insert(n);
            self.apply_table.insert(f, g, BddPtr::false_ptr(), r);
            r
        }
    }

    /// Compute the Boolean function `f || g`
    pub fn or(&mut self, f: BddPtr, g: BddPtr) -> BddPtr {
        self.and(f.compl(), g.compl()).compl()
    }

    /// disjoins a list of BDDs
    pub fn or_lst(&mut self, f: &[BddPtr]) -> BddPtr {
        let mut cur_bdd = BddPtr::false_ptr();
        for &itm in f {
            cur_bdd = self.or(cur_bdd, itm);
        }
        cur_bdd
    }

    /// disjoins a list of BDDs
    pub fn and_lst(&mut self, f: &[BddPtr]) -> BddPtr {
        let mut cur_bdd = BddPtr::true_ptr();
        for &itm in f {
            cur_bdd = self.and(cur_bdd, itm);
        }
        cur_bdd
    }

    /// Compute the Boolean function `f iff g`
    pub fn iff(&mut self, f: BddPtr, g: BddPtr) -> BddPtr {
        self.ite(f, g, g.compl())
    }

    pub fn xor(&mut self, f: BddPtr, g: BddPtr) -> BddPtr {
        self.ite(f, g.compl(), g)
    }

    fn cond_helper(
        &mut self,
        bdd: BddPtr,
        lbl: VarLabel,
        value: bool,
        cache: &mut Vec<Option<BddPtr>>,
    ) -> BddPtr {
        self.stats.num_recursive_calls += 1;
        if self.get_order().lt(lbl, bdd.var()) || bdd.is_const() {
            // we passed the variable in the order, we will never find it
            bdd
        } else if bdd.var() == lbl {
            let node = bdd.into_node();
            let r = if value { node.high } else { node.low };
            if bdd.is_compl() {
                r.compl()
            } else {
                r
            }
        } else {
            // check cache
            let idx = bdd.get_scratch().unwrap();
            match cache[idx] {
                None => (),
                Some(v) => return if bdd.is_compl() { v.compl() } else { v },
            };

            // recurse on the children
            let n = bdd.into_node();
            let l = self.cond_helper(n.low, lbl, value, cache);
            let h = self.cond_helper(n.high, lbl, value, cache);
            if l == h {
                if bdd.is_compl() {
                    return l.compl();
                } else {
                    return l;
                };
            };
            let res = if l != n.low || h != n.high {
                // cache and return the new BDD
                let new_bdd = BddNode::new(bdd.var(), l, h);
                let r = self.get_or_insert(new_bdd);
                if bdd.is_compl() {
                    r.compl()
                } else {
                    r
                }
            } else {
                // nothing changed
                bdd
            };
            cache[idx] = Some(if bdd.is_compl() { res.compl() } else { res });
            res
        }
    }

    /// Compute the Boolean function `f | var = value`
    pub fn condition(&mut self, bdd: BddPtr, lbl: VarLabel, value: bool) -> BddPtr {
        let n = bdd.unique_label_nodes(0);
        let r = self.cond_helper(bdd, lbl, value, &mut vec![None; n]);
        bdd.clear_scratch();
        r
    }

    fn cond_model_h(
        &mut self,
        bdd: BddPtr,
        m: &PartialModel,
        cache: &mut Vec<Option<BddPtr>>,
    ) -> BddPtr {
        self.stats.num_recursive_calls += 1;
        if bdd.is_const() {
            return bdd;
        }
        match m.get(bdd.var()) {
            Some(value) => {
                let node = bdd.into_node();
                let r = if value {
                    self.cond_model_h(node.high, m, cache)
                } else {
                    self.cond_model_h(node.low, m, cache)
                };
                if bdd.is_compl() {
                    r.compl()
                } else {
                    r
                }
            }
            None => {
                // check cache
                let idx = bdd.get_scratch().unwrap();
                match cache[idx] {
                    None => (),
                    Some(v) => return if bdd.is_compl() { v.compl() } else { v },
                };

                // recurse on the children
                let n = bdd.into_node();
                let l = self.cond_model_h(n.low, m, cache);
                let h = self.cond_model_h(n.high, m, cache);
                if l == h {
                    if bdd.is_compl() {
                        return l.compl();
                    } else {
                        return l;
                    };
                };
                let res = if l != n.low || h != n.high {
                    // cache and return the new BDD
                    let new_bdd = BddNode::new(bdd.var(), l, h);
                    let r = self.get_or_insert(new_bdd);
                    if bdd.is_compl() {
                        r.compl()
                    } else {
                        r
                    }
                } else {
                    // nothing changed
                    bdd
                };
                cache[idx] = Some(if bdd.is_compl() { res.compl() } else { res });
                res
            }
        }
    }

    /// Compute the Boolean function `f | var = value` for every set value in
    /// the partial model `m`
    pub fn condition_model(&mut self, bdd: BddPtr, m: &PartialModel) -> BddPtr {
        let n = bdd.unique_label_nodes(0);
        let r = self.cond_model_h(bdd, m, &mut vec![None; n]);
        bdd.clear_scratch();
        r
    }

    /// Existentially quantifies out the variable `lbl` from `f`
    pub fn exists(&mut self, bdd: BddPtr, lbl: VarLabel) -> BddPtr {
        // TODO this can be optimized by specializing it
        let v1 = self.condition(bdd, lbl, true);
        let v2 = self.condition(bdd, lbl, false);
        self.or(v1, v2)
    }



    /// Returns true if `a` == `b`
    pub fn eq_bdd(&self, a: BddPtr, b: BddPtr) -> bool {
        // the magic of BDDs!
        a == b
    }


    pub fn from_cnf_with_assignments(&mut self, cnf: &Cnf, assgn: &model::PartialModel) -> BddPtr {
        let clauses = cnf.clauses();
        if clauses.is_empty() {
            return BddPtr::true_ptr();
        }
        let mut compiled_heap: BinaryHeap<CompiledCNF> = BinaryHeap::new();
        // push each clause onto the compiled_heap
        for clause in clauses.iter() {
            let mut cur_ptr = BddPtr::false_ptr();
            for lit in clause.iter() {
                match assgn.get(lit.get_label()) {
                    None => {
                        let new_v = self.var(lit.get_label(), lit.get_polarity());
                        cur_ptr = self.or(new_v, cur_ptr);
                    }
                    Some(v) if v == lit.get_polarity() => {
                        cur_ptr = BddPtr::true_ptr();
                        break;
                    }
                    _ => {
                        continue;
                    }
                }
            }
            let sz = cur_ptr.count_nodes();
            compiled_heap.push(CompiledCNF { ptr: cur_ptr, sz });
        }

        while compiled_heap.len() > 1 {
            let CompiledCNF { ptr: ptr1, sz: _sz } = compiled_heap.pop().unwrap();
            let CompiledCNF { ptr: ptr2, sz: _sz } = compiled_heap.pop().unwrap();
            let ptr = self.and(ptr1, ptr2);
            let sz = ptr.count_nodes();
            compiled_heap.push(CompiledCNF { ptr, sz })
        }

        let CompiledCNF { ptr, sz: _sz } = compiled_heap.pop().unwrap();
        ptr
    }

    pub fn from_dtree(&mut self, dtree: &dtree::DTree) -> BddPtr {
        use dtree::DTree;
        match &dtree {
            &DTree::Leaf {
                v: c,
                cutset: _,
                vars: _,
            } => {
                // compile the clause
                c.iter().fold(BddPtr::false_ptr(), |acc, i| {
                    let v = self.var(i.get_label(), i.get_polarity());
                    self.or(acc, v)
                })
            }
            &DTree::Node {
                ref l,
                ref r,
                cutset: _,
                vars: _,
            } => {
                let l = self.from_dtree(l);
                let r = self.from_dtree(r);
                self.and(l, r)
            }
        }
    }

    /// Compile a BDD from a CNF
    pub fn from_cnf(&mut self, cnf: &Cnf) -> BddPtr {
        // let dtree = dtree::DTree::from_cnf(cnf, &self.order);
        // return self.from_dtree(&dtree);
        let mut cvec: Vec<BddPtr> = Vec::with_capacity(cnf.clauses().len());
        if cnf.clauses().is_empty() {
            return BddPtr::true_ptr();
        }
        // check if there is an empty clause -- if so, UNSAT
        if cnf.clauses().iter().any(|x| x.is_empty()) {
            return BddPtr::false_ptr();
        }

        // sort the clauses based on a best-effort bottom-up ordering of clauses
        let mut cnf_sorted = cnf.clauses().to_vec();
        let order = self.get_order();
        cnf_sorted.sort_by(|c1, c2| {
            // order the clause with the first-most variable last
            let fst1 = c1
                .iter()
                .max_by(|l1, l2| {
                    if order.lt(l1.get_label(), l2.get_label()) {
                        Ordering::Less
                    } else {
                        Ordering::Equal
                    }
                })
                .unwrap();
            let fst2 = c2
                .iter()
                .max_by(|l1, l2| {
                    if order.lt(l1.get_label(), l2.get_label()) {
                        Ordering::Less
                    } else {
                        Ordering::Equal
                    }
                })
                .unwrap();
            if order.lt(fst1.get_label(), fst2.get_label()) {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        });

        for lit_vec in cnf_sorted.iter() {
            let (vlabel, val) = (lit_vec[0].get_label(), lit_vec[0].get_polarity());
            let mut bdd = self.var(vlabel, val);
            for i in 1..lit_vec.len() {
                let (vlabel, val) = (lit_vec[i].get_label(), lit_vec[i].get_polarity());
                let var = self.var(vlabel, val);
                bdd = self.or(bdd, var);
            }
            cvec.push(bdd);
        }
        // now cvec has a list of all the clauses; collapse it down
        fn helper(vec: &[BddPtr], man: &mut BddManager) -> Option<BddPtr> {
            if vec.is_empty() {
                None
            } else if vec.len() == 1 {
                Some(vec[0])
            } else {
                let (l, r) = vec.split_at(vec.len() / 2);
                let sub_l = helper(l, man);
                let sub_r = helper(r, man);
                match (sub_l, sub_r) {
                    (None, None) => None,
                    (Some(v), None) | (None, Some(v)) => Some(v),
                    (Some(l), Some(r)) => Some(man.and(l, r)),
                }
            }
        }
        let r = helper(&cvec, self);
        if r.is_none() {
            BddPtr::true_ptr()
        } else {
            r.unwrap()
        }
    }

    pub fn from_boolexpr(&mut self, expr: &LogicalExpr) -> BddPtr {
        match expr {
            &LogicalExpr::Literal(lbl, polarity) => self.var(VarLabel::new(lbl as u64), polarity),
            &LogicalExpr::And(ref l, ref r) => {
                let r1 = self.from_boolexpr(l);
                let r2 = self.from_boolexpr(r);
                self.and(r1, r2)
            }
            &LogicalExpr::Or(ref l, ref r) => {
                let r1 = self.from_boolexpr(l);
                let r2 = self.from_boolexpr(r);
                self.or(r1, r2)
            }
            &LogicalExpr::Not(ref e) => self.from_boolexpr(e).compl(),
            &LogicalExpr::Iff(ref l, ref r) => {
                let r1 = self.from_boolexpr(l);
                let r2 = self.from_boolexpr(r);
                self.iff(r1, r2)
            }
            &LogicalExpr::Xor(ref l, ref r) => {
                let r1 = self.from_boolexpr(l);
                let r2 = self.from_boolexpr(r);
                self.xor(r1, r2)
            }
            &LogicalExpr::Ite {
                ref guard,
                ref thn,
                ref els,
            } => {
                let g = self.from_boolexpr(guard);
                let t = self.from_boolexpr(thn);
                let e = self.from_boolexpr(els);
                self.ite(g, t, e)
            }
        }
    }

    /// Compiles a plan into a BDD
    pub fn compile_plan(&mut self, expr: &BddPlan) -> BddPtr {
        match expr {
            &BddPlan::Literal(var, polarity) => self.var(VarLabel::new(var as u64), polarity),
            &BddPlan::And(ref l, ref r) => {
                let r1 = self.compile_plan(l);
                let r2 = self.compile_plan(r);
                self.and(r1, r2)
            }
            &BddPlan::Or(ref l, ref r) => {
                let r1 = self.compile_plan(l);
                let r2 = self.compile_plan(r);
                self.or(r1, r2)
            }
            &BddPlan::Iff(ref l, ref r) => {
                let r1 = self.compile_plan(l);
                let r2 = self.compile_plan(r);
                self.iff(r1, r2)
            }
            &BddPlan::Ite(ref f, ref g, ref h) => {
                let f = self.compile_plan(f);
                let g = self.compile_plan(g);
                let h = self.compile_plan(h);
                self.ite(f, g, h)
            }
            &BddPlan::Not(ref f) => {
                let f = self.compile_plan(f);
                f.compl()
            }
            &BddPlan::ConstTrue => BddPtr::true_ptr(),
            &BddPlan::ConstFalse => BddPtr::false_ptr(),
        }
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
        cache: &mut HashMap<HashedCNF, BddPtr>,
    ) -> BddPtr {
        // check for base case
        let assgn = sat.get_implied_units();
        if level >= cnf.num_vars() || cnf.is_sat_partial(&assgn) {
            return BddPtr::true_ptr();
        }

        let cur_v = self.get_order().var_at_level(level);

        // check if this literal is currently set in unit propagation; if
        // it is, skip it
        if assgn.is_set(cur_v) {
            return self.topdown_h(cnf, sat, level + 1, cache);
        }

        // check cache
        let hashed = cnf.get_hasher().hash(&assgn);
        match cache.get(&hashed) {
            None => (),
            Some(v) => {
                return *v;
            }
        }

        // recurse on both values of cur_v
        sat.push();
        sat.decide(Literal::new(cur_v, true));
        let unsat = sat.unsat_unit();
        let high_bdd = if !unsat {
            let new_assgn = sat.get_implied_units();
            let mut lit_cube = BddPtr::true_ptr();
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

            for l in implied_lits {
                if l.get_label() == cur_v {
                    continue;
                }
                let v = self.var(l.get_label(), l.get_polarity());
                lit_cube = self.and(lit_cube, v);
            }
            let sub = self.topdown_h(cnf, sat, level + 1, cache);
            self.and(sub, lit_cube)
        } else {
            BddPtr::false_ptr()
        };

        sat.pop();
        sat.push();
        sat.decide(Literal::new(cur_v, false));
        let unsat = sat.unsat_unit();
        let low_bdd = if !unsat {
            let new_assgn = sat.get_implied_units();
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
            let mut lit_cube = BddPtr::true_ptr();
            for l in implied_lits {
                if l.get_label() == cur_v {
                    continue;
                }
                let v = self.var(l.get_label(), l.get_polarity());
                lit_cube = self.and(lit_cube, v);
            }
            let sub = self.topdown_h(cnf, sat, level + 1, cache);
            self.and(sub, lit_cube)
        } else {
            BddPtr::false_ptr()
        };

        sat.pop();
        let r = if high_bdd == low_bdd {
            high_bdd
        } else {
            let bdd = BddNode::new(cur_v, low_bdd, high_bdd);
            self.get_or_insert(bdd)
        };
        cache.insert(hashed, r);
        r
    }

    pub fn from_cnf_topdown(&mut self, cnf: &Cnf) -> BddPtr {
        self.from_cnf_topdown_partial(cnf, &PartialModel::from_litvec(&Vec::new(), cnf.num_vars()))
    }

    /// Compile a CNF to a BDD top-down beginning from the partial model given in `model`
    pub fn from_cnf_topdown_partial(&mut self, cnf: &Cnf, model: &PartialModel) -> BddPtr {
        self.from_cnf_topdown_partial_cached(cnf, model, &mut HashMap::new())
    }

    /// Compile a CNF to a BDD top-down beginning from the partial model given in `model`
    /// takes a cache as an argument
    pub fn from_cnf_topdown_partial_cached(
        &mut self,
        cnf: &Cnf,
        model: &PartialModel,
        cache: &mut HashMap<HashedCNF, BddPtr>,
    ) -> BddPtr {
        let mut sat = SATSolver::new(cnf);
        for assgn in model.assignment_iter() {
            sat.decide(assgn);
        }

        if sat.unsat_unit() {
            return BddPtr::false_ptr();
        }

        let mut lit_cube = BddPtr::true_ptr();
        let assign_set: HashSet<Literal> = model.assignment_iter().collect();
        for lit in sat.get_implied_units().assignment_iter() {
            // conjoin in literals that are implied but not initially set
            if !assign_set.contains(&lit) {
                let v = self.var(lit.get_label(), lit.get_polarity());
                lit_cube = self.and(v, lit_cube);
            }
        }

        let r = self.topdown_h(cnf, &mut sat, 0, cache);

        // conjoin in any initially implied literals
        self.and(r, lit_cube)
    }

    /// Prints the total number of recursive calls executed so far by the BddManager
    /// This is a stable way to track performance
    pub fn num_recursive_calls(&self) -> usize {
        self.stats.num_recursive_calls
    }
}

#[cfg(test)]
mod tests {

    use libc::NOTE_EXIT_DECRYPTFAIL;
    use maplit::*;

    use crate::{
        repr::{
            cnf::Cnf,
            var_label::{Literal, VarLabel}, bdd::{BddPtr, BddWmc},
        }, builder::bdd_builder::BddManager,
    };

    // check that (a \/ b) /\ a === a
    #[test]
    fn simple_equality() {
        let mut man = BddManager::new_default_order(3);
        let v1 = man.var(VarLabel::new(0), true);
        let v2 = man.var(VarLabel::new(1), true);
        let r1 = man.or(v1, v2);
        let r2 = man.and(r1, v1);
        assert!(
            man.eq_bdd(v1, r2),
            "Not eq:\n {}\n{}",
            v1.to_string_debug(),
            r2.to_string_debug()
        );
    }

    #[test]
    fn simple_ite1() {
        let mut man = BddManager::new_default_order(3);
        let v1 = man.var(VarLabel::new(0), true);
        let v2 = man.var(VarLabel::new(1), true);
        let r1 = man.or(v1, v2);
        let r2 = man.ite(r1, v1, BddPtr::false_ptr());
        assert!(
            man.eq_bdd(v1, r2),
            "Not eq:\n {}\n{}",
            v1.to_string_debug(),
            r2.to_string_debug()
        );
    }

    #[test]
    fn test_newvar() {
        let mut man = BddManager::new_default_order(0);
        let l1 = man.new_var();
        let l2 = man.new_var();
        let v1 = man.var(l1, true);
        let v2 = man.var(l2, true);
        let r1 = man.or(v1, v2);
        let r2 = man.and(r1, v1);
        assert!(
            man.eq_bdd(v1, r2),
            "Not eq:\n {}\n{}",
            v1.to_string_debug(),
            r2.to_string_debug()
        );
    }

    #[test]
    fn test_wmc() {
        let mut man = BddManager::new_default_order(2);
        let v1 = man.var(VarLabel::new(0), true);
        let v2 = man.var(VarLabel::new(1), true);
        let r1 = man.or(v1, v2);
        let weights = hashmap! {VarLabel::new(0) => (2,3),
        VarLabel::new(1) => (5,7)};
        let params = BddWmc::new_with_default(0, 1, weights);
        let wmc = r1.wmc(&man.order, &params);
        assert_eq!(wmc, 50);
    }

    #[test]
    fn test_wmc_smooth() {
        let mut man = BddManager::new_default_order(3);
        let v1 = man.var(VarLabel::new(0), true);
        let v2 = man.var(VarLabel::new(2), true);
        let r1 = man.or(v1, v2);
        let weights = hashmap! {
        VarLabel::new(0) => (2,3),
        VarLabel::new(1) => (5,7),
        VarLabel::new(2) => (11,13)};
        let params = BddWmc::new_with_default(0, 1, weights);
        let wmc = r1.wmc(&man.order, &params);
        assert_eq!(wmc, 1176);
    }

    #[test]
    fn test_wmc_smooth2() {
        let mut man = BddManager::new_default_order(3);
        let r1 = BddPtr::true_ptr();
        let weights = hashmap! {
        VarLabel::new(0) => (2,3),
        VarLabel::new(1) => (5,7),
        VarLabel::new(2) => (11,13)};
        let params = BddWmc::new_with_default(0, 1, weights);
        let wmc = r1.wmc(&man.order, &params);
        assert_eq!(wmc, 1440);
    }

    #[test]
    fn test_condition() {
        let mut man = BddManager::new_default_order(3);
        let v1 = man.var(VarLabel::new(0), true);
        let v2 = man.var(VarLabel::new(1), true);
        let r1 = man.or(v1, v2);
        let r3 = man.condition(r1, VarLabel::new(1), false);
        assert!(man.eq_bdd(r3, v1));
    }

    #[test]
    fn test_condition_compl() {
        let mut man = BddManager::new_default_order(3);
        let v1 = man.var(VarLabel::new(0), false);
        let v2 = man.var(VarLabel::new(1), false);
        let r1 = man.and(v1, v2);
        let r3 = man.condition(r1, VarLabel::new(1), false);
        assert!(
            man.eq_bdd(r3, v1),
            "Not eq:\nOne: {}\nTwo: {}",
            r3.to_string_debug(),
            v1.to_string_debug()
        );
    }

    #[test]
    fn test_exist() {
        let mut man = BddManager::new_default_order(3);
        // 1 /\ 2 /\ 3
        let v1 = man.var(VarLabel::new(0), true);
        let v2 = man.var(VarLabel::new(1), true);
        let v3 = man.var(VarLabel::new(2), true);
        let a1 = man.and(v1, v2);
        let r1 = man.and(a1, v3);
        let r_expected = man.and(v1, v3);
        let res = man.exists(r1, VarLabel::new(1));
        assert!(
            man.eq_bdd(r_expected, res),
            "Got:\nOne: {}\nExpected: {}",
            res.to_string_debug(), 
            r_expected.to_string_debug()
        );
    }

    #[test]
    fn test_exist_compl() {
        let mut man = BddManager::new_default_order(3);
        // 1 /\ 2 /\ 3
        let v1 = man.var(VarLabel::new(0), false);
        let v2 = man.var(VarLabel::new(1), false);
        let v3 = man.var(VarLabel::new(2), false);
        let a1 = man.and(v1, v2);
        let r1 = man.and(a1, v3);
        let r_expected = man.and(v1, v3);
        let res = man.exists(r1, VarLabel::new(1));
        // let res = r1;
        assert!(
            man.eq_bdd(r_expected, res),
            "Got:\n: {}\nExpected: {}",
            res.to_string_debug(),
            r_expected.to_string_debug()
        );
    }

    #[test]
    fn test_compose() {
        let mut man = BddManager::new_default_order(3);
        let v0 = man.var(VarLabel::new(0), true);
        let v1 = man.var(VarLabel::new(1), true);
        let v2 = man.var(VarLabel::new(2), true);
        let v0_and_v1 = man.and(v0, v1);
        let v0_and_v2 = man.and(v0, v2);
        let res = man.compose(v0_and_v1, VarLabel::new(1), v2);
        assert!(
            man.eq_bdd(res, v0_and_v2),
            "\nGot: {}\nExpected: {}",
            res.to_string_debug(), 
            v0_and_v2.to_string_debug()
        );
    }

    #[test]
    fn test_compose_2() {
        let mut man = BddManager::new_default_order(4);
        let v0 = man.var(VarLabel::new(0), true);
        let v1 = man.var(VarLabel::new(1), true);
        let v2 = man.var(VarLabel::new(2), true);
        let v3 = man.var(VarLabel::new(3), true);
        let v0_and_v1 = man.and(v0, v1);
        let v2_and_v3 = man.and(v2, v3);
        let v0v2v3 = man.and(v0, v2_and_v3);
        let res = man.compose(v0_and_v1, VarLabel::new(1), v2_and_v3);
        assert!(
            man.eq_bdd(res, v0v2v3),
            "\nGot: {}\nExpected: {}",
            res.to_string_debug(), 
            v0v2v3.to_string_debug()
        );
    }

    #[test]
    fn test_compose_3() {
        let mut man = BddManager::new_default_order(4);
        let v0 = man.var(VarLabel::new(0), true);
        let v1 = man.var(VarLabel::new(1), true);
        let v2 = man.var(VarLabel::new(2), true);
        let f = man.ite(v0, BddPtr::false_ptr(), v1);
        let res = man.compose(f, VarLabel::new(1), v2);
        let expected = man.ite(v0, BddPtr::false_ptr(), v2);
        assert!(
            man.eq_bdd(res, expected),
            "\nGot: {}\nExpected: {}",
            res.to_string_debug(),
            expected.to_string_debug()
        );
    }

    #[test]
    fn test_compose_4() {
        let mut man = BddManager::new_default_order(20);
        let v0 = man.var(VarLabel::new(4), true);
        let v1 = man.var(VarLabel::new(5), true);
        let v2 = man.var(VarLabel::new(6), true);
        let f = man.ite(v1, BddPtr::false_ptr(), v2);
        let res = man.compose(f, VarLabel::new(6), v0);
        let expected = man.ite(v1, BddPtr::false_ptr(), v0);
        assert!(
            man.eq_bdd(res, expected),
            "\nGot: {}\nExpected: {}",
            res.to_string_debug(),
            expected.to_string_debug()
        );
    }

    #[test]
    fn test_new_var() {
        let mut man = BddManager::new_default_order(0);
        let vlbl1 = man.new_var();
        let vlbl2 = man.new_var();
        let v1 = man.var(vlbl1, false);
        let v2 = man.var(vlbl2, false);
        let r1 = man.and(v1, v2);
        let r3 = man.condition(r1, VarLabel::new(1), false);
        assert!(
            man.eq_bdd(r3, v1),
            "Not eq:\nOne: {}\nTwo: {}",
            r3.to_string_debug(),
            v1.to_string_debug()
        );
    }

    #[test]
    fn circuit1() {
        let mut man = BddManager::new_default_order(3);
        let x = man.var(VarLabel::new(0), false);
        let y = man.var(VarLabel::new(1), true);
        let delta = man.and(x, y);
        let yp = man.var(VarLabel::new(2), true);
        let inner = man.iff(yp, y);
        let conj = man.and(inner, delta);
        let res = man.exists(conj, VarLabel::new(1));

        let expected = man.and(x, yp);
        assert!(
            man.eq_bdd(res, expected),
            "Not eq:\nGot: {}\nExpected: {}",
            res.to_string_debug(),
            expected.to_string_debug()
        );
    }

    #[test]
    fn simple_cond() {
        let mut man = BddManager::new_default_order(3);
        let x = man.var(VarLabel::new(0), true);
        let y = man.var(VarLabel::new(1), false);
        let z = man.var(VarLabel::new(2), false);
        let r1 = man.and(x, y);
        let r2 = man.and(r1, z);
        // now r2 is x /\ !y /\ !z

        let res = man.condition(r2, VarLabel::new(1), true);
        let expected = BddPtr::false_ptr();
        assert!(
            man.eq_bdd(res, expected),
            "\nOriginal BDD: {}\nNot eq:\nGot: {}\nExpected: {}",
            r2.to_string_debug(),
            res.to_string_debug(),
            expected.to_string_debug()
        );
    }

    #[test]
    fn wmc_test_2() {
        let mut man = BddManager::new_default_order(4);
        let x = man.var(VarLabel::new(0), true);
        let y = man.var(VarLabel::new(1), true);
        let f1 = man.var(VarLabel::new(2), true);
        let f2 = man.var(VarLabel::new(3), true);
        let map = hashmap! { VarLabel::new(0) => (1.0, 1.0),
        VarLabel::new(1) => (1.0, 1.0),
        VarLabel::new(2) => (0.8, 0.2),
        VarLabel::new(3) => (0.7, 0.3) };
        let wmc = BddWmc::new_with_default(0.0, 1.0, map);
        let iff1 = man.iff(x, f1);
        let iff2 = man.iff(y, f2);
        let obs = man.or(x, y);
        let and1 = man.and(iff1, iff2);
        let f = man.and(and1, obs);
        assert_eq!(f.wmc(&man.order, &wmc), 0.2 * 0.3 + 0.2 * 0.7 + 0.8 * 0.3);
    }

    #[test]
    fn iff_regression() {
        let mut man = BddManager::new_default_order(0);
        let mut ptrvec = Vec::new();
        for _ in 0..40 {
            let vlab = man.new_var();
            let flab = man.new_var();
            let vptr = man.var(vlab, true);
            let fptr = man.var(flab, true);
            let sent = man.iff(vptr, fptr);
            ptrvec.push(sent);
        }
        let _resptr = ptrvec
            .iter()
            .fold(BddPtr::true_ptr(), |acc, x| man.and(acc, *x));
        assert!(true);
    }

    #[test]
    fn test_topdown_1() {
        let clauses = vec![vec![
            Literal::new(VarLabel::new(0), true),
            Literal::new(VarLabel::new(1), true),
        ]];
        let cnf = Cnf::new(clauses);
        let mut mgr = BddManager::new_default_order(2);
        let c1 = mgr.from_cnf(&cnf);
        let c2 = mgr.from_cnf_topdown(&cnf);
        assert_eq!(
            c1,
            c2,
            "BDD not equal: got {}, expected {}",
            c2.to_string_debug(),
            c1.to_string_debug()
        );
    }

    #[test]
    fn test_topdown_2() {
        let clauses = vec![vec![
            Literal::new(VarLabel::new(0), false),
            Literal::new(VarLabel::new(1), false),
        ]];
        let cnf = Cnf::new(clauses);
        let mut mgr = BddManager::new_default_order(2);
        let c1 = mgr.from_cnf(&cnf);
        let c2 = mgr.from_cnf_topdown(&cnf);
        assert_eq!(
            c1,
            c2,
            "BDD not equal: got {}, expected {}",
            c2.to_string_debug(),
            c1.to_string_debug()
        );
    }

    #[test]
    fn test_topdown_3() {
        let clauses = vec![
            vec![
                Literal::new(VarLabel::new(1), true),
                Literal::new(VarLabel::new(3), true),
            ],
            vec![
                Literal::new(VarLabel::new(3), false),
                Literal::new(VarLabel::new(2), true),
                Literal::new(VarLabel::new(4), true),
            ],
        ];
        let cnf = Cnf::new(clauses);
        let mut mgr = BddManager::new_default_order(cnf.num_vars());
        let c1 = mgr.from_cnf(&cnf);
        let c2 = mgr.from_cnf_topdown(&cnf);
        assert_eq!(
            c1,
            c2,
            "BDD not equal: got {}, expected {}",
            c2.to_string_debug(),
            c1.to_string_debug()
        );
    }

    #[test]
    fn test_topdown_4() {
        let clauses = vec![
            vec![Literal::new(VarLabel::new(0), true)],
            vec![Literal::new(VarLabel::new(0), true)],
        ];
        let cnf = Cnf::new(clauses);
        let mut mgr = BddManager::new_default_order(cnf.num_vars());
        let c1 = mgr.from_cnf(&cnf);
        let c2 = mgr.from_cnf_topdown(&cnf);
        assert_eq!(
            c1,
            c2,
            "BDD not equal: got {}, expected {}",
            c2.to_string_debug(),
            c1.to_string_debug()
        );
    }
}
