//! Primary interface for manipulating and constructing BDDs. Contains the BDD
//! manager, which manages the global state necessary for constructing canonical
//! binary decision diagrams.

use crate::repr::model::PartialModel;
use crate::repr::robdd::BddNode;
use crate::repr::var_order::VarOrder;
use crate::{
    backing_store::bump_table::BackedRobinhoodTable, repr::cnf::*, repr::logical_expr::LogicalExpr,
    repr::model,
};

use super::cache::all_app::AllTable;
use super::cache::ite::Ite;
use super::cache::lru_app::BddApplyTable;
use super::BottomUpBuilder;
use crate::backing_store::*;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::fmt::Debug;

use super::bdd_plan::BddPlan;

pub use crate::builder::cache::LruTable;
pub use crate::repr::ddnnf::DDNNFPtr;
pub use crate::repr::robdd::BddPtr;
pub use crate::repr::var_label::VarLabel;

#[derive(Eq, PartialEq, Debug)]
struct CompiledCNF<'a> {
    ptr: BddPtr<'a>,
    sz: usize,
}

// The priority queue depends on `Ord`.
// Explicitly implement the trait so the queue becomes a min-heap
// instead of a max-heap.
impl<'a> Ord for CompiledCNF<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        // Notice that the we flip the ordering on costs.
        // In case of a tie we compare positions - this step is necessary
        // to make implementations of `PartialEq` and `Ord` consistent.
        other.sz.cmp(&self.sz)
    }
}

// `PartialOrd` needs to be implemented as well.
impl<'a> PartialOrd for CompiledCNF<'a> {
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
struct BddBuilderStats {
    /// For now, always track the number of recursive calls. In the future,
    /// this should probably be gated behind a debug build (since I suspect
    /// it may have non-trivial performance overhead and synchronization cost)
    num_recursive_calls: usize,
}

impl BddBuilderStats {
    pub fn new() -> BddBuilderStats {
        BddBuilderStats {
            num_recursive_calls: 0,
        }
    }
}

pub trait BddBuilder<'a>: BottomUpBuilder<'a, BddPtr<'a>> {
    fn get_or_insert(&'a self, bdd: BddNode<'a>) -> BddPtr<'a>;

    // implementation-dependent helper functions

    fn ite_helper(&'a self, f: BddPtr<'a>, g: BddPtr<'a>, h: BddPtr<'a>) -> BddPtr<'a>;
    fn cond_helper(&'a self, bdd: BddPtr<'a>, lbl: VarLabel, value: bool) -> BddPtr<'a>;

    // convenience utilities
    /// disjoins a list of BDDs
    fn or_lst(&'a self, f: &[BddPtr<'a>]) -> BddPtr<'a> {
        let mut cur_bdd = BddPtr::false_ptr();
        for &itm in f {
            cur_bdd = self.or(cur_bdd, itm);
        }
        cur_bdd
    }

    /// disjoins a list of BDDs
    fn and_lst(&'a self, f: &[BddPtr<'a>]) -> BddPtr<'a> {
        let mut cur_bdd = BddPtr::true_ptr();
        for &itm in f {
            cur_bdd = self.and(cur_bdd, itm);
        }
        cur_bdd
    }
}

impl<'a, T> BottomUpBuilder<'a, BddPtr<'a>> for T
where
    T: BddBuilder<'a>,
{
    fn true_ptr(&self) -> BddPtr<'a> {
        BddPtr::true_ptr()
    }

    fn false_ptr(&self) -> BddPtr<'a> {
        BddPtr::false_ptr()
    }

    /// Get a pointer to the variable with label `lbl` and polarity `polarity`
    fn var(&'a self, label: VarLabel, polarity: bool) -> BddPtr<'a> {
        let bdd = BddNode::new(label, BddPtr::false_ptr(), BddPtr::true_ptr());
        let r = self.get_or_insert(bdd);
        if polarity {
            r
        } else {
            r.neg()
        }
    }

    fn eq(&'a self, a: BddPtr<'a>, b: BddPtr<'a>) -> bool {
        a == b
    }

    /// Produce a new BDD that is the result of conjoining `f` and `g`
    /// ```
    /// # use rsdd::builder::bdd_builder::StandardBddBuilder;
    /// # use rsdd::builder::BottomUpBuilder;
    /// # use rsdd::repr::var_label::VarLabel;
    /// # use rsdd::repr::ddnnf::DDNNFPtr;
    /// # use rsdd::builder::cache::all_app::AllTable;
    /// # use rsdd::repr::robdd::BddPtr;
    /// let mut man = StandardBddBuilder::<AllTable<BddPtr>>::new_default_order(10);
    /// let lbl_a = man.new_label();
    /// let a = man.var(lbl_a, true);
    /// let a_and_not_a = man.and(a, a.neg());
    /// assert!(a_and_not_a.is_false());
    /// ```
    fn and(&'a self, f: BddPtr<'a>, g: BddPtr<'a>) -> BddPtr<'a> {
        self.ite(f, g, BddPtr::false_ptr())
    }

    fn negate(&'a self, f: BddPtr<'a>) -> BddPtr<'a> {
        f.neg()
    }

    /// if f then g else h
    fn ite(&'a self, f: BddPtr<'a>, g: BddPtr<'a>, h: BddPtr<'a>) -> BddPtr<'a> {
        self.ite_helper(f, g, h)
    }

    /// Compute the Boolean function `f iff g`
    fn iff(&'a self, f: BddPtr<'a>, g: BddPtr<'a>) -> BddPtr<'a> {
        self.ite(f, g, g.neg())
    }

    fn xor(&'a self, f: BddPtr<'a>, g: BddPtr<'a>) -> BddPtr<'a> {
        self.ite(f, g.neg(), g)
    }

    /// Existentially quantifies out the variable `lbl` from `f`
    fn exists(&'a self, bdd: BddPtr<'a>, lbl: VarLabel) -> BddPtr<'a> {
        // TODO this can be optimized by specializing it
        let v1 = self.condition(bdd, lbl, true);
        let v2 = self.condition(bdd, lbl, false);
        self.or(v1, v2)
    }

    /// Compute the Boolean function `f | var = value`
    fn condition(&'a self, bdd: BddPtr<'a>, lbl: VarLabel, value: bool) -> BddPtr<'a> {
        let r = self.cond_helper(bdd, lbl, value);
        bdd.clear_scratch();
        r
    }

    /// Compose `g` into `f` by substituting for `lbl`
    fn compose(&'a self, f: BddPtr<'a>, lbl: VarLabel, g: BddPtr<'a>) -> BddPtr<'a> {
        // TODO this can be optimized with a specialized implementation to make
        // it a single traversal
        let var = self.var(lbl, true);
        let iff = self.iff(var, g);
        let a = self.and(iff, f);

        self.exists(a, lbl)
    }
}

pub struct StandardBddBuilder<'a, T: LruTable<'a, BddPtr<'a>>> {
    compute_table: RefCell<BackedRobinhoodTable<'a, BddNode<'a>>>,
    apply_table: RefCell<T>,
    stats: RefCell<BddBuilderStats>,
    order: RefCell<VarOrder>,
}

impl<'a, T: LruTable<'a, BddPtr<'a>>> BddBuilder<'a> for StandardBddBuilder<'a, T> {
    /// Normalizes and fetches a node from the store
    fn get_or_insert(&'a self, bdd: BddNode<'a>) -> BddPtr<'a> {
        unsafe {
            // TODO: Make this safe if possible
            let tbl = &mut *self.compute_table.as_ptr();
            if bdd.high.is_neg() || bdd.high.is_false() {
                let bdd: BddNode<'a> = BddNode::new(bdd.var, bdd.low.neg(), bdd.high.neg());
                let r: &'a BddNode<'a> = tbl.get_or_insert(bdd);
                BddPtr::new_compl(r)
            } else {
                let bdd = BddNode::new(bdd.var, bdd.low, bdd.high);
                BddPtr::new_reg(tbl.get_or_insert(bdd))
            }
        }
    }

    fn ite_helper(&'a self, f: BddPtr<'a>, g: BddPtr<'a>, h: BddPtr<'a>) -> BddPtr<'a> {
        self.stats.borrow_mut().num_recursive_calls += 1;
        let o = |a: BddPtr, b: BddPtr| {
            if a.is_const() {
                return true;
            }
            if b.is_const() {
                return false;
            }
            return self.order.borrow().lt(a.var(), b.var());
        };

        let ite = Ite::new(o, f, g, h);

        if let Ite::IteConst(f) = ite {
            return f;
        }

        let hash = self.apply_table.borrow().hash(&ite);
        if let Some(v) = self.apply_table.borrow().get(ite, hash) {
            return v;
        }

        // ok the work!
        // find the first essential variable for f, g, or h
        let lbl = self.order.borrow().first_essential(f, g, h);
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
        self.apply_table.borrow_mut().insert(ite, r, hash);
        r
    }

    fn cond_helper(&'a self, bdd: BddPtr<'a>, lbl: VarLabel, value: bool) -> BddPtr<'a> {
        self.cond_with_alloc(bdd, lbl, value, &mut Vec::new())
    }
}

impl<'a, T: LruTable<'a, BddPtr<'a>>> StandardBddBuilder<'a, T> {
    /// Make a BDD manager with a default variable ordering
    pub fn new_default_order(num_vars: usize) -> StandardBddBuilder<'a, AllTable<BddPtr<'a>>> {
        let default_order = VarOrder::linear_order(num_vars);
        StandardBddBuilder::new(default_order, AllTable::new())
    }

    pub fn new_default_order_lru(
        num_vars: usize,
    ) -> StandardBddBuilder<'a, BddApplyTable<BddPtr<'a>>> {
        let default_order = VarOrder::linear_order(num_vars);
        StandardBddBuilder::new(default_order, BddApplyTable::new(21))
    }

    /// Creates a new variable manager with the specified order
    pub fn new(order: VarOrder, table: T) -> StandardBddBuilder<'a, T> {
        StandardBddBuilder {
            compute_table: RefCell::new(BackedRobinhoodTable::new()),
            order: RefCell::new(order),
            apply_table: RefCell::new(table),
            stats: RefCell::new(BddBuilderStats::new()),
        }
    }

    /// Returns the number of variables in the manager
    #[inline]
    pub fn num_vars(&self) -> usize {
        self.order.borrow().num_vars()
    }

    /// Generate a new variable label which was not in the original order. Places the
    /// new variable label at the end of the current order. Returns the newly
    /// generated label.
    #[inline]
    pub fn new_label(&self) -> VarLabel {
        self.order.borrow_mut().new_last()
    }

    /// Generate a new pointer which was not in the original order. Uses
    /// `new_label` to produce a new label at the end of the current order, then
    /// uses `var` to create a pointer in the manager. Returns the output of both.
    #[inline]
    pub fn new_var(&'a self, polarity: bool) -> (VarLabel, BddPtr<'a>) {
        let label = self.new_label();
        let ptr = self.var(label, polarity);
        (label, ptr)
    }

    /// Use `new_var` to create a new positive pointer.
    #[inline]
    pub fn new_pos(&'a self) -> (VarLabel, BddPtr<'a>) {
        self.new_var(true)
    }

    /// Use `new_var` to create a new negative pointer.
    #[inline]
    pub fn new_neg(&'a self) -> (VarLabel, BddPtr<'a>) {
        self.new_var(false)
    }

    /// Get the current variable order
    #[inline]
    pub fn get_order(&self) -> &VarOrder {
        // TODO fix this, it doesn't need to be unsafe
        unsafe { &*self.order.as_ptr() }
    }

    // condition a BDD *only* if the top variable is `v`; used in `ite`
    fn condition_essential(&'a self, f: BddPtr<'a>, lbl: VarLabel, v: bool) -> BddPtr<'a> {
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

    fn cond_with_alloc(
        &'a self,
        bdd: BddPtr<'a>,
        lbl: VarLabel,
        value: bool,
        alloc: &mut Vec<BddPtr<'a>>,
    ) -> BddPtr<'a> {
        self.stats.borrow_mut().num_recursive_calls += 1;
        if bdd.is_const() || self.order.borrow().lt(lbl, bdd.var()) {
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
            match bdd.get_scratch::<usize>() {
                None => (),
                Some(v) => {
                    return if bdd.is_neg() {
                        alloc[v].neg()
                    } else {
                        alloc[v]
                    }
                }
            };

            // recurse on the children
            let l = self.cond_with_alloc(bdd.low_raw(), lbl, value, alloc);
            let h = self.cond_with_alloc(bdd.high_raw(), lbl, value, alloc);

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

            let idx = if bdd.is_neg() {
                alloc.push(res.neg());
                alloc.len() - 1
            } else {
                alloc.push(res);
                alloc.len() - 1
            };
            bdd.set_scratch(idx);
            res
        }
    }

    fn cond_model_h(&'a self, bdd: BddPtr<'a>, m: &PartialModel) -> BddPtr<'a> {
        // TODO: optimize this
        let mut bdd = bdd;
        for m in m.assignment_iter() {
            bdd = self.condition(bdd, m.get_label(), m.get_polarity());
        }
        bdd
    }

    /// Compute the Boolean function `f | var = value` for every set value in
    /// the partial model `m`
    ///
    /// Pre-condition: scratch cleared
    pub fn condition_model(&'a self, bdd: BddPtr<'a>, m: &PartialModel) -> BddPtr<'a> {
        debug_assert!(bdd.is_scratch_cleared());
        let r = self.cond_model_h(bdd, m);
        bdd.clear_scratch();
        r
    }

    pub fn from_cnf_with_assignments(
        &'a self,
        cnf: &Cnf,
        assgn: &model::PartialModel,
    ) -> BddPtr<'a> {
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
    pub fn from_cnf(&'a self, cnf: &Cnf) -> BddPtr<'a> {
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
        let order = self.order.borrow();
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
        fn helper<'a, T: LruTable<'a, BddPtr<'a>>>(
            vec: &[BddPtr<'a>],
            man: &'a StandardBddBuilder<'a, T>,
        ) -> Option<BddPtr<'a>> {
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

    pub fn from_boolexpr(&'a self, expr: &LogicalExpr) -> BddPtr<'a> {
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
    pub fn compile_plan(&'a self, expr: &BddPlan) -> BddPtr<'a> {
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

    /// Prints the total number of recursive calls executed so far by the StandardBddBuilder
    /// This is a stable way to track performance
    pub fn num_recursive_calls(&self) -> usize {
        self.stats.borrow().num_recursive_calls
    }
}

#[cfg(test)]
mod tests {

    use std::borrow::Borrow;

    use crate::builder::BottomUpBuilder;
    use crate::repr::wmc::WmcParams;
    use crate::util::semirings::realsemiring::RealSemiring;
    use crate::util::semirings::semiring_traits::Semiring;
    use crate::{builder::cache::all_app::AllTable, repr::ddnnf::DDNNFPtr};
    use maplit::*;

    use crate::{
        builder::bdd_builder::StandardBddBuilder,
        repr::{cnf::Cnf, robdd::BddPtr, var_label::VarLabel},
    };

    // check that (a \/ b) /\ a === a
    #[test]
    fn simple_equality() {
        let man = StandardBddBuilder::<AllTable<BddPtr>>::new_default_order(3);
        let v1 = man.var(VarLabel::new(0), true);
        let v2 = man.var(VarLabel::new(1), true);
        let r1 = man.or(v1, v2);
        let r2 = man.and(r1, v1);
        assert!(
            man.eq(v1, r2),
            "Not eq:\n {}\n{}",
            v1.to_string_debug(),
            r2.to_string_debug()
        );
    }

    #[test]
    fn simple_ite1() {
        let man = StandardBddBuilder::<AllTable<BddPtr>>::new_default_order(3);
        let v1 = man.var(VarLabel::new(0), true);
        let v2 = man.var(VarLabel::new(1), true);
        let r1 = man.or(v1, v2);
        let r2 = man.ite(r1, v1, BddPtr::false_ptr());
        assert!(
            man.eq(v1, r2),
            "Not eq:\n {}\n{}",
            v1.to_string_debug(),
            r2.to_string_debug()
        );
    }

    #[test]
    fn test_newvar() {
        let man = StandardBddBuilder::<AllTable<BddPtr>>::new_default_order(0);
        let l1 = man.new_label();
        let l2 = man.new_label();
        let v1 = man.var(l1, true);
        let v2 = man.var(l2, true);
        let r1 = man.or(v1, v2);
        let r2 = man.and(r1, v1);
        assert!(
            man.eq(v1, r2),
            "Not eq:\n {}\n{}",
            v1.to_string_debug(),
            r2.to_string_debug()
        );
    }

    #[test]
    fn test_wmc() {
        let man = StandardBddBuilder::<AllTable<BddPtr>>::new_default_order(2);
        let v1 = man.var(VarLabel::new(0), true);
        let v2 = man.var(VarLabel::new(1), true);
        let r1 = man.or(v1, v2);
        let weights = hashmap! {VarLabel::new(0) => (RealSemiring(0.2), RealSemiring(0.8)),
        VarLabel::new(1) => (RealSemiring(0.1), RealSemiring(0.9))};
        let params =
            WmcParams::new_with_default(RealSemiring::zero(), RealSemiring::one(), weights);
        let wmc = r1.wmc(man.get_order().borrow(), &params);
        assert!((wmc.0 - (1.0 - 0.2 * 0.1)).abs() < 0.000001);
    }

    #[test]
    fn test_condition() {
        let man = StandardBddBuilder::<AllTable<BddPtr>>::new_default_order(3);
        let v1 = man.var(VarLabel::new(0), true);
        let v2 = man.var(VarLabel::new(1), true);
        let r1 = man.or(v1, v2);
        let r3 = man.condition(r1, VarLabel::new(1), false);
        assert!(man.eq(r3, v1));
    }

    #[test]
    fn test_condition_compl() {
        let man = StandardBddBuilder::<AllTable<BddPtr>>::new_default_order(3);
        let v1 = man.var(VarLabel::new(0), false);
        let v2 = man.var(VarLabel::new(1), false);
        let r1 = man.and(v1, v2);
        let r3 = man.condition(r1, VarLabel::new(1), false);
        assert!(
            man.eq(r3, v1),
            "Not eq:\nOne: {}\nTwo: {}",
            r3.to_string_debug(),
            v1.to_string_debug()
        );
    }

    #[test]
    fn test_exist() {
        let man = StandardBddBuilder::<AllTable<BddPtr>>::new_default_order(3);
        // 1 /\ 2 /\ 3
        let v1 = man.var(VarLabel::new(0), true);
        let v2 = man.var(VarLabel::new(1), true);
        let v3 = man.var(VarLabel::new(2), true);
        let a1 = man.and(v1, v2);
        let r1 = man.and(a1, v3);
        let r_expected = man.and(v1, v3);
        let res = man.exists(r1, VarLabel::new(1));
        assert!(
            man.eq(r_expected, res),
            "Got:\nOne: {}\nExpected: {}",
            res.to_string_debug(),
            r_expected.to_string_debug()
        );
    }

    #[test]
    fn test_exist_compl() {
        let man = StandardBddBuilder::<AllTable<BddPtr>>::new_default_order(3);
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
            man.eq(r_expected, res),
            "Got:\n: {}\nExpected: {}",
            res.to_string_debug(),
            r_expected.to_string_debug()
        );
    }

    #[test]
    fn test_compose() {
        let man = StandardBddBuilder::<AllTable<BddPtr>>::new_default_order(3);
        let v0 = man.var(VarLabel::new(0), true);
        let v1 = man.var(VarLabel::new(1), true);
        let v2 = man.var(VarLabel::new(2), true);
        let v0_and_v1 = man.and(v0, v1);
        let v0_and_v2 = man.and(v0, v2);
        let res = man.compose(v0_and_v1, VarLabel::new(1), v2);
        assert!(
            man.eq(res, v0_and_v2),
            "\nGot: {}\nExpected: {}",
            res.to_string_debug(),
            v0_and_v2.to_string_debug()
        );
    }

    #[test]
    fn test_compose_2() {
        let man = StandardBddBuilder::<AllTable<BddPtr>>::new_default_order(4);
        let v0 = man.var(VarLabel::new(0), true);
        let v1 = man.var(VarLabel::new(1), true);
        let v2 = man.var(VarLabel::new(2), true);
        let v3 = man.var(VarLabel::new(3), true);
        let v0_and_v1 = man.and(v0, v1);
        let v2_and_v3 = man.and(v2, v3);
        let v0v2v3 = man.and(v0, v2_and_v3);
        let res = man.compose(v0_and_v1, VarLabel::new(1), v2_and_v3);
        assert!(
            man.eq(res, v0v2v3),
            "\nGot: {}\nExpected: {}",
            res.to_string_debug(),
            v0v2v3.to_string_debug()
        );
    }

    #[test]
    fn test_compose_3() {
        let man = StandardBddBuilder::<AllTable<BddPtr>>::new_default_order(4);
        let v0 = man.var(VarLabel::new(0), true);
        let v1 = man.var(VarLabel::new(1), true);
        let v2 = man.var(VarLabel::new(2), true);
        let f = man.ite(v0, BddPtr::false_ptr(), v1);
        let res = man.compose(f, VarLabel::new(1), v2);
        let expected = man.ite(v0, BddPtr::false_ptr(), v2);
        assert!(
            man.eq(res, expected),
            "\nGot: {}\nExpected: {}",
            res.to_string_debug(),
            expected.to_string_debug()
        );
    }

    #[test]
    fn test_compose_4() {
        let man = StandardBddBuilder::<AllTable<BddPtr>>::new_default_order(20);
        let v0 = man.var(VarLabel::new(4), true);
        let v1 = man.var(VarLabel::new(5), true);
        let v2 = man.var(VarLabel::new(6), true);
        let f = man.ite(v1, BddPtr::false_ptr(), v2);
        let res = man.compose(f, VarLabel::new(6), v0);
        let expected = man.ite(v1, BddPtr::false_ptr(), v0);
        assert!(
            man.eq(res, expected),
            "\nGot: {}\nExpected: {}",
            res.to_string_debug(),
            expected.to_string_debug()
        );
    }

    #[test]
    fn test_new_label() {
        let man = StandardBddBuilder::<AllTable<BddPtr>>::new_default_order(0);
        let vlbl1 = man.new_label();
        let vlbl2 = man.new_label();
        let v1 = man.var(vlbl1, false);
        let v2 = man.var(vlbl2, false);
        let r1 = man.and(v1, v2);
        let r3 = man.condition(r1, VarLabel::new(1), false);
        assert!(
            man.eq(r3, v1),
            "Not eq:\nOne: {}\nTwo: {}",
            r3.to_string_debug(),
            v1.to_string_debug()
        );
    }

    #[test]
    fn circuit1() {
        let man = StandardBddBuilder::<AllTable<BddPtr>>::new_default_order(3);
        let x = man.var(VarLabel::new(0), false);
        let y = man.var(VarLabel::new(1), true);
        let delta = man.and(x, y);
        let yp = man.var(VarLabel::new(2), true);
        let inner = man.iff(yp, y);
        let conj = man.and(inner, delta);
        let res = man.exists(conj, VarLabel::new(1));

        let expected = man.and(x, yp);
        assert!(
            man.eq(res, expected),
            "Not eq:\nGot: {}\nExpected: {}",
            res.to_string_debug(),
            expected.to_string_debug()
        );
    }

    #[test]
    fn simple_cond() {
        let man = StandardBddBuilder::<AllTable<BddPtr>>::new_default_order(3);
        let x = man.var(VarLabel::new(0), true);
        let y = man.var(VarLabel::new(1), false);
        let z = man.var(VarLabel::new(2), false);
        let r1 = man.and(x, y);
        let r2 = man.and(r1, z);
        // now r2 is x /\ !y /\ !z

        let res = man.condition(r2, VarLabel::new(1), true); // condition on y=T
        let expected = BddPtr::false_ptr();
        assert!(
            man.eq(res, expected),
            "\nOriginal BDD: {}\nNot eq:\nGot: {}\nExpected: {}",
            r2.to_string_debug(),
            res.to_string_debug(),
            expected.to_string_debug()
        );
    }

    #[test]
    fn wmc_test_2() {
        let man = StandardBddBuilder::<AllTable<BddPtr>>::new_default_order(4);
        let x = man.var(VarLabel::new(0), true);
        let y = man.var(VarLabel::new(1), true);
        let f1 = man.var(VarLabel::new(2), true);
        let f2 = man.var(VarLabel::new(3), true);
        let map = hashmap! { VarLabel::new(0) => (RealSemiring(1.0), RealSemiring(1.0)),
        VarLabel::new(1) => (RealSemiring(1.0), RealSemiring(1.0)),
        VarLabel::new(2) => (RealSemiring(0.8), RealSemiring(0.2)),
        VarLabel::new(3) => (RealSemiring(0.7), RealSemiring(0.3)) };
        let wmc = WmcParams::new_with_default(RealSemiring::zero(), RealSemiring::one(), map);
        let iff1 = man.iff(x, f1);
        let iff2 = man.iff(y, f2);
        let obs = man.or(x, y);
        let and1 = man.and(iff1, iff2);
        let f = man.and(and1, obs);
        assert_eq!(
            f.wmc(man.get_order().borrow(), &wmc).0,
            0.2 * 0.3 + 0.2 * 0.7 + 0.8 * 0.3
        );
    }

    #[allow(clippy::assertions_on_constants)] // TODO: why does this test have assert!(true) ?
    #[test]
    fn iff_regression() {
        let man = StandardBddBuilder::<AllTable<BddPtr>>::new_default_order(0);
        let mut ptrvec = Vec::new();
        for _ in 0..40 {
            let vlab = man.new_label();
            let flab = man.new_label();
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
    fn test_ite_1() {
        let man = StandardBddBuilder::<AllTable<BddPtr>>::new_default_order(16);
        let c1 = Cnf::from_string(String::from("(1 || 2) && (0 || -2)"));
        let c2 = Cnf::from_string(String::from("(0 || 1) && (-4 || -7)"));
        let cnf1 = man.from_cnf(&c1);
        let cnf2 = man.from_cnf(&c2);
        let iff1 = man.iff(cnf1, cnf2);

        let clause1 = man.and(cnf1, cnf2);
        let clause2 = man.and(cnf1.neg(), cnf2.neg());
        let and = man.or(clause1, clause2);

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
