use crate::{
    builder::{bdd::CompiledCNF, BottomUpBuilder},
    repr::{BddNode, BddPtr, Cnf, DDNNFPtr, PartialModel, VarLabel},
};
use std::{cmp::Ordering, collections::BinaryHeap};

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
                match assgn.get(lit.label()) {
                    None => {
                        let new_v = self.var(lit.label(), lit.polarity());
                        cur_ptr = self.or(new_v, cur_ptr);
                    }
                    Some(v) if v == lit.polarity() => {
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
    /// # use rsdd::builder::bdd::RobddBuilder;
    /// # use rsdd::builder::BottomUpBuilder;
    /// # use rsdd::repr::VarLabel;
    /// # use rsdd::repr::DDNNFPtr;
    /// # use rsdd::builder::cache::AllIteTable;
    /// # use rsdd::repr::BddPtr;
    /// let mut builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(10);
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
                    if self.less_than(l1.label(), l2.label()) {
                        Ordering::Less
                    } else {
                        Ordering::Equal
                    }
                })
                .unwrap();
            let fst2 = c2
                .iter()
                .max_by(|l1, l2| {
                    if self.less_than(l1.label(), l2.label()) {
                        Ordering::Less
                    } else {
                        Ordering::Equal
                    }
                })
                .unwrap();
            if self.less_than(fst1.label(), fst2.label()) {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        });

        for lit_vec in cnf_sorted.iter() {
            let (vlabel, val) = (lit_vec[0].label(), lit_vec[0].polarity());
            let mut bdd = self.var(vlabel, val);
            for lit in lit_vec {
                let (vlabel, val) = (lit.label(), lit.polarity());
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
}
