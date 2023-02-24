//! Primary interface for manipulating and constructing BDDs. Contains the BDD
//! manager, which manages the global state necessary for constructing canonical
//! binary decision diagrams.

use bumpalo::Bump;

use crate::repr::bdd::{BddNode, BddPtr};
use crate::repr::ddnnf::DDNNFPtr;
use crate::repr::dtree;
use crate::repr::model::PartialModel;
use crate::repr::sat_solver::SATSolver;
use crate::repr::var_order::VarOrder;
use crate::{
    backing_store::bump_table::BackedRobinhoodTable, repr::cnf::*, repr::logical_expr::LogicalExpr,
    repr::model, repr::var_label::Literal, repr::var_label::VarLabel,
};

use super::cache::all_app::AllTable;
use super::cache::ite::Ite;
use super::cache::lru_app::BddApplyTable;
use super::cache::*;
use crate::backing_store::*;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::{BinaryHeap, HashSet};
use std::fmt::Debug;

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
        other.sz.cmp(&self.sz)
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

pub struct BddManager<T: LruTable<BddPtr>> {
    compute_table: BackedRobinhoodTable<BddNode>,
    apply_table: T,
    stats: BddManagerStats,
    order: VarOrder,
}

impl<T: LruTable<BddPtr>> BddManager<T> {
    /// Make a BDD manager with a default variable ordering
    pub fn new_default_order(num_vars: usize) -> BddManager<AllTable<BddPtr>> {
        let default_order = VarOrder::linear_order(num_vars);
        BddManager::new(default_order, AllTable::new())
    }

    pub fn new_default_order_lru(num_vars: usize) -> BddManager<BddApplyTable<BddPtr>> {
        let default_order = VarOrder::linear_order(num_vars);
        BddManager::new(default_order, BddApplyTable::new(21))
    }

    /// Creates a new variable manager with the specified order
    pub fn new(order: VarOrder, table: T) -> BddManager<T> {
        BddManager {
            compute_table: BackedRobinhoodTable::new(),
            order,
            apply_table: table,
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
            r.neg()
        }
    }

    /// Normalizes and fetches a node from the store
    fn get_or_insert(&mut self, bdd: BddNode) -> BddPtr {
        if bdd.high.is_neg() || bdd.high.is_false() {
            let bdd = BddNode::new(bdd.var, bdd.low.neg(), bdd.high.neg());
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
        let r = if v { f.high_raw() } else { f.low_raw() };
        if f.is_neg() {
            r.neg()
        } else {
            r
        }
    }

    fn ite_helper(&mut self, f: BddPtr, g: BddPtr, h: BddPtr) -> BddPtr {
        self.stats.num_recursive_calls += 1;
        let o = |a: BddPtr, b: BddPtr| {
            if a.is_const() {
                return true;
            }
            if b.is_const() {
                return false;
            }
            return self.get_order().lt(a.var(), b.var());
        };

        let ite = Ite::new(o, f, g, h);

        if let Ite::IteConst(f) = ite {
            return f;
        }

        let hash = self.apply_table.hash(&ite);
        if let Some(v) = self.apply_table.get(ite, hash) {
            return v;
        }

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
        self.apply_table.insert(ite, r, hash);
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
    /// # use crate::rsdd::repr::ddnnf::DDNNFPtr;
    /// # use rsdd::builder::cache::all_app::AllTable;
    /// # use rsdd::repr::bdd::BddPtr;
    /// let mut mgr = BddManager::<AllTable<BddPtr>>::new_default_order(10);
    /// let lbl_a = mgr.new_var();
    /// let a = mgr.var(lbl_a, true);
    /// let a_and_not_a = mgr.and(a, a.neg());
    /// assert!(a_and_not_a.is_false());
    /// ```
    pub fn and(&mut self, f: BddPtr, g: BddPtr) -> BddPtr {
        self.ite(f, g, BddPtr::false_ptr())
    }

    /// Compute the Boolean function `f || g`
    pub fn or(&mut self, f: BddPtr, g: BddPtr) -> BddPtr {
        self.and(f.neg(), g.neg()).neg()
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
        self.ite(f, g, g.neg())
    }

    pub fn xor(&mut self, f: BddPtr, g: BddPtr) -> BddPtr {
        self.ite(f, g.neg(), g)
    }

    fn cond_helper(&mut self, bdd: BddPtr, lbl: VarLabel, value: bool, alloc: &mut Bump) -> BddPtr {
        self.stats.num_recursive_calls += 1;
        if bdd.is_const() || self.get_order().lt(lbl, bdd.var()) {
            // we passed the variable in the order, we will never find it
            bdd
        } else if bdd.var() == lbl {
            let r = if value { bdd.high_raw() } else { bdd.low_raw() };
            if bdd.is_neg() {
                r.neg()
            } else {
                r
            }
        } else {
            // check cache
            match bdd.get_scratch::<BddPtr>() {
                None => (),
                Some(v) => return if bdd.is_neg() { v.neg() } else { *v },
            };

            // recurse on the children
            let l = self.cond_helper(bdd.low_raw(), lbl, value, alloc);
            let h = self.cond_helper(bdd.high_raw(), lbl, value, alloc);

            if l == h {
                // reduce the BDD -- two children identical
                if bdd.is_neg() {
                    return l.neg();
                } else {
                    return l;
                };
            };
            let res = if l != bdd.low_raw() || h != bdd.high_raw() {
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

    /// Compute the Boolean function `f | var = value`
    pub fn condition(&mut self, bdd: BddPtr, lbl: VarLabel, value: bool) -> BddPtr {
        let r = self.cond_helper(bdd, lbl, value, &mut Bump::new());
        bdd.clear_scratch();
        r
    }

    fn cond_model_h(&mut self, bdd: BddPtr, m: &PartialModel, alloc: &mut Bump) -> BddPtr {
        self.stats.num_recursive_calls += 1;
        if bdd.is_const() {
            return bdd;
        }
        match m.get(bdd.var()) {
            Some(value) => {
                let node = bdd.into_node();
                let r = if value {
                    self.cond_model_h(node.high, m, alloc)
                } else {
                    self.cond_model_h(node.low, m, alloc)
                };
                if bdd.is_neg() {
                    r.neg()
                } else {
                    r
                }
            }
            None => {
                // check cache
                match bdd.get_scratch::<BddPtr>() {
                    None => (),
                    Some(v) => return if bdd.is_neg() { v.neg() } else { *v },
                };

                // recurse on the children
                let n = bdd.into_node();
                let l = self.cond_model_h(n.low, m, alloc);
                let h = self.cond_model_h(n.high, m, alloc);
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

    /// Compute the Boolean function `f | var = value` for every set value in
    /// the partial model `m`
    ///
    /// Pre-condition: scratch cleared
    pub fn condition_model(&mut self, bdd: BddPtr, m: &PartialModel) -> BddPtr {
        let mut alloc = Bump::new();
        let r = self.cond_model_h(bdd, m, &mut alloc);
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

    /// Compile a BDD from a CNF
    pub fn from_cnf(&mut self, cnf: &Cnf) -> BddPtr {
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
            for lit in lit_vec {
                let (vlabel, val) = (lit.get_label(), lit.get_polarity());
                let var = self.var(vlabel, val);
                bdd = self.or(bdd, var);
            }
            cvec.push(bdd);
        }
        // now cvec has a list of all the clauses; collapse it down
        fn helper<T: LruTable<BddPtr>>(vec: &[BddPtr], man: &mut BddManager<T>) -> Option<BddPtr> {
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
        match r {
            None => BddPtr::true_ptr(),
            Some(x) => x,
        }
    }

    pub fn from_boolexpr(&mut self, expr: &LogicalExpr) -> BddPtr {
        match &expr {
            LogicalExpr::Literal(lbl, polarity) => self.var(VarLabel::new(*lbl as u64), *polarity),
            LogicalExpr::And(ref l, ref r) => {
                let r1 = self.from_boolexpr(l);
                let r2 = self.from_boolexpr(r);
                self.and(r1, r2)
            }
            LogicalExpr::Or(ref l, ref r) => {
                let r1 = self.from_boolexpr(l);
                let r2 = self.from_boolexpr(r);
                self.or(r1, r2)
            }
            LogicalExpr::Not(ref e) => self.from_boolexpr(e).neg(),
            LogicalExpr::Iff(ref l, ref r) => {
                let r1 = self.from_boolexpr(l);
                let r2 = self.from_boolexpr(r);
                self.iff(r1, r2)
            }
            LogicalExpr::Xor(ref l, ref r) => {
                let r1 = self.from_boolexpr(l);
                let r2 = self.from_boolexpr(r);
                self.xor(r1, r2)
            }
            LogicalExpr::Ite {
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
        match &expr {
            BddPlan::Literal(var, polarity) => self.var(*var, *polarity),
            BddPlan::And(ref l, ref r) => {
                let r1 = self.compile_plan(l);
                let r2 = self.compile_plan(r);
                self.and(r1, r2)
            }
            BddPlan::Or(ref l, ref r) => {
                let r1 = self.compile_plan(l);
                let r2 = self.compile_plan(r);
                self.or(r1, r2)
            }
            BddPlan::Iff(ref l, ref r) => {
                let r1 = self.compile_plan(l);
                let r2 = self.compile_plan(r);
                self.iff(r1, r2)
            }
            BddPlan::Ite(ref f, ref g, ref h) => {
                let f = self.compile_plan(f);
                let g = self.compile_plan(g);
                let h = self.compile_plan(h);
                self.ite(f, g, h)
            }
            BddPlan::Not(ref f) => {
                let f = self.compile_plan(f);
                f.neg()
            }
            BddPlan::ConstTrue => BddPtr::true_ptr(),
            BddPlan::ConstFalse => BddPtr::false_ptr(),
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

    use crate::repr::wmc::WmcParams;
    use crate::{builder::cache::all_app::AllTable, repr::ddnnf::DDNNFPtr};
    use maplit::*;
    use num::abs;

    use crate::{
        builder::bdd_builder::BddManager,
        repr::{
            bdd::BddPtr,
            cnf::Cnf,
            var_label::{Literal, VarLabel},
        },
    };

    // check that (a \/ b) /\ a === a
    #[test]
    fn simple_equality() {
        let mut man = BddManager::<AllTable<BddPtr>>::new_default_order(3);
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
        let mut man = BddManager::<AllTable<BddPtr>>::new_default_order(3);
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
        let mut man = BddManager::<AllTable<BddPtr>>::new_default_order(0);
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
        let mut man = BddManager::<AllTable<BddPtr>>::new_default_order(2);
        let v1 = man.var(VarLabel::new(0), true);
        let v2 = man.var(VarLabel::new(1), true);
        let r1 = man.or(v1, v2);
        let weights = hashmap! {VarLabel::new(0) => (0.2,0.8),
        VarLabel::new(1) => (0.1,0.9)};
        let params = WmcParams::new_with_default(0.0, 1.0, weights);
        let wmc = r1.wmc(man.get_order(), &params);
        assert!(abs(wmc - (1.0 - 0.2 * 0.1)) < 0.000001);
    }

    #[test]
    fn test_condition() {
        let mut man = BddManager::<AllTable<BddPtr>>::new_default_order(3);
        let v1 = man.var(VarLabel::new(0), true);
        let v2 = man.var(VarLabel::new(1), true);
        let r1 = man.or(v1, v2);
        let r3 = man.condition(r1, VarLabel::new(1), false);
        assert!(man.eq_bdd(r3, v1));
    }

    #[test]
    fn test_condition_compl() {
        let mut man = BddManager::<AllTable<BddPtr>>::new_default_order(3);
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
        let mut man = BddManager::<AllTable<BddPtr>>::new_default_order(3);
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
        let mut man = BddManager::<AllTable<BddPtr>>::new_default_order(3);
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
        let mut man = BddManager::<AllTable<BddPtr>>::new_default_order(3);
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
        let mut man = BddManager::<AllTable<BddPtr>>::new_default_order(4);
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
        let mut man = BddManager::<AllTable<BddPtr>>::new_default_order(4);
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
        let mut man = BddManager::<AllTable<BddPtr>>::new_default_order(20);
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
        let mut man = BddManager::<AllTable<BddPtr>>::new_default_order(0);
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
        let mut man = BddManager::<AllTable<BddPtr>>::new_default_order(3);
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
        let mut man = BddManager::<AllTable<BddPtr>>::new_default_order(3);
        let x = man.var(VarLabel::new(0), true);
        let y = man.var(VarLabel::new(1), false);
        let z = man.var(VarLabel::new(2), false);
        let r1 = man.and(x, y);
        let r2 = man.and(r1, z);
        // now r2 is x /\ !y /\ !z

        let res = man.condition(r2, VarLabel::new(1), true); // condition on y=T
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
        let mut man = BddManager::<AllTable<BddPtr>>::new_default_order(4);
        let x = man.var(VarLabel::new(0), true);
        let y = man.var(VarLabel::new(1), true);
        let f1 = man.var(VarLabel::new(2), true);
        let f2 = man.var(VarLabel::new(3), true);
        let map = hashmap! { VarLabel::new(0) => (1.0, 1.0),
        VarLabel::new(1) => (1.0, 1.0),
        VarLabel::new(2) => (0.8, 0.2),
        VarLabel::new(3) => (0.7, 0.3) };
        let wmc = WmcParams::new_with_default(0.0, 1.0, map);
        let iff1 = man.iff(x, f1);
        let iff2 = man.iff(y, f2);
        let obs = man.or(x, y);
        let and1 = man.and(iff1, iff2);
        let f = man.and(and1, obs);
        assert_eq!(
            f.wmc(man.get_order(), &wmc),
            0.2 * 0.3 + 0.2 * 0.7 + 0.8 * 0.3
        );
    }

    #[allow(clippy::assertions_on_constants)] // TODO: why does this test have assert!(true) ?
    #[test]
    fn iff_regression() {
        let mut man = BddManager::<AllTable<BddPtr>>::new_default_order(0);
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
        let mut mgr = BddManager::<AllTable<BddPtr>>::new_default_order(2);
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
        let mut mgr = BddManager::<AllTable<BddPtr>>::new_default_order(2);
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
        let mut mgr = BddManager::<AllTable<BddPtr>>::new_default_order(cnf.num_vars());
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
        let mut mgr = BddManager::<AllTable<BddPtr>>::new_default_order(cnf.num_vars());
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
    fn test_ite_1() {
        let mut mgr = BddManager::<AllTable<BddPtr>>::new_default_order(16);
        let c1 = Cnf::from_string(String::from("(1 || 2) && (0 || -2)"));
        let c2 = Cnf::from_string(String::from("(0 || 1) && (-4 || -7)"));
        let cnf1 = mgr.from_cnf(&c1);
        let cnf2 = mgr.from_cnf(&c2);
        let iff1 = mgr.iff(cnf1, cnf2);

        let clause1 = mgr.and(cnf1, cnf2);
        let clause2 = mgr.and(cnf1.neg(), cnf2.neg());
        let and = mgr.or(clause1, clause2);

        if and != iff1 {
            println!("cnf1: {}", c1);
            println!("cnf2: {}", c2);
            println!(
                "not equal:\nBdd1: {}\nBdd2: {}",
                and.to_string_debug(),
                iff1.to_string_debug()
            );
        }
        assert_eq!(and, iff1);
    }
}
