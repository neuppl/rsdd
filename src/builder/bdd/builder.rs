use std::cmp::Ordering;
use std::collections::BinaryHeap;

use crate::plan::bdd_plan::BddPlan;
use crate::repr::bdd::BddNode;
use crate::repr::cnf::Cnf;
use crate::repr::logical_expr::LogicalExpr;
use crate::repr::model::PartialModel;

use crate::builder::BottomUpBuilder;

pub use crate::builder::cache::LruTable;
pub use crate::repr::bdd::BddPtr;
pub use crate::repr::ddnnf::DDNNFPtr;
pub use crate::repr::var_label::VarLabel;

use super::CompiledCNF;

pub trait BddBuilder<'a>: BottomUpBuilder<'a, BddPtr<'a>> {
    fn less_than(&self, a: VarLabel, b: VarLabel) -> bool;

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

    fn compile_cnf_with_assignments(&'a self, cnf: &Cnf, assgn: &PartialModel) -> BddPtr<'a> {
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
    fn compile_cnf(&'a self, cnf: &Cnf) -> BddPtr<'a> {
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
        cnf_sorted.sort_by(|c1, c2| {
            // order the clause with the first-most variable last
            let fst1 = c1
                .iter()
                .max_by(|l1, l2| {
                    if self.less_than(l1.get_label(), l2.get_label()) {
                        Ordering::Less
                    } else {
                        Ordering::Equal
                    }
                })
                .unwrap();
            let fst2 = c2
                .iter()
                .max_by(|l1, l2| {
                    if self.less_than(l1.get_label(), l2.get_label()) {
                        Ordering::Less
                    } else {
                        Ordering::Equal
                    }
                })
                .unwrap();
            if self.less_than(fst1.get_label(), fst2.get_label()) {
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
        let r = self.collapse_clauses(&cvec);
        match r {
            None => BddPtr::true_ptr(),
            Some(x) => x,
        }
    }

    fn collapse_clauses(&'a self, vec: &[BddPtr<'a>]) -> Option<BddPtr<'a>> {
        if vec.is_empty() {
            None
        } else if vec.len() == 1 {
            Some(vec[0])
        } else {
            let (l, r) = vec.split_at(vec.len() / 2);
            let sub_l = self.collapse_clauses(l);
            let sub_r = self.collapse_clauses(r);
            match (sub_l, sub_r) {
                (None, None) => None,
                (Some(v), None) | (None, Some(v)) => Some(v),
                (Some(l), Some(r)) => Some(self.and(l, r)),
            }
        }
    }
    fn compile_boolexpr(&'a self, expr: &LogicalExpr) -> BddPtr<'a> {
        match &expr {
            LogicalExpr::Literal(lbl, polarity) => self.var(VarLabel::new(*lbl as u64), *polarity),
            LogicalExpr::And(ref l, ref r) => {
                let r1 = self.compile_boolexpr(l);
                let r2 = self.compile_boolexpr(r);
                self.and(r1, r2)
            }
            LogicalExpr::Or(ref l, ref r) => {
                let r1 = self.compile_boolexpr(l);
                let r2 = self.compile_boolexpr(r);
                self.or(r1, r2)
            }
            LogicalExpr::Not(ref e) => self.compile_boolexpr(e).neg(),
            LogicalExpr::Iff(ref l, ref r) => {
                let r1 = self.compile_boolexpr(l);
                let r2 = self.compile_boolexpr(r);
                self.iff(r1, r2)
            }
            LogicalExpr::Xor(ref l, ref r) => {
                let r1 = self.compile_boolexpr(l);
                let r2 = self.compile_boolexpr(r);
                self.xor(r1, r2)
            }
            LogicalExpr::Ite {
                ref guard,
                ref thn,
                ref els,
            } => {
                let g = self.compile_boolexpr(guard);
                let t = self.compile_boolexpr(thn);
                let e = self.compile_boolexpr(els);
                self.ite(g, t, e)
            }
        }
    }

    /// Compiles a plan into a BDD
    fn compile_plan(&'a self, expr: &BddPlan) -> BddPtr<'a> {
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
    /// # use rsdd::builder::bdd::robdd::RobddBuilder;
    /// # use rsdd::builder::BottomUpBuilder;
    /// # use rsdd::repr::var_label::VarLabel;
    /// # use rsdd::repr::ddnnf::DDNNFPtr;
    /// # use rsdd::builder::cache::all_app::AllTable;
    /// # use rsdd::repr::bdd::BddPtr;
    /// let mut builder = RobddBuilder::<AllTable<BddPtr>>::new_default_order(10);
    /// let lbl_a = builder.new_label();
    /// let a = builder.var(lbl_a, true);
    /// let a_and_not_a = builder.and(a, a.neg());
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
