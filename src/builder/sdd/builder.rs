//! The main trait of the SDD manager, the primary way of interacting
//! with SDDs.

use crate::{
    builder::{cache::Ite, BottomUpBuilder},
    repr::{
        BinarySDD, Cnf, DDNNFPtr, SddAnd, SddOr, SddPtr, VTree, VTreeIndex, VTreeManager, VarLabel,
    },
};
use std::cmp::Ordering;

#[derive(Default)]
pub struct SddBuilderStats {
    pub app_cache_hits: usize,
    pub app_cache_size: usize,
    pub num_logically_redundant: usize,
    pub num_recursive_calls: usize,
    pub num_compressions: usize,
    pub num_get_or_insert_bdd: usize,
    pub num_get_or_insert_sdd: usize,
}

pub trait SddBuilder<'a>: BottomUpBuilder<'a, SddPtr<'a>> {
    // internal data structures
    fn vtree_manager(&self) -> &VTreeManager;

    fn app_cache_get(&self, and: &SddAnd<'a>) -> Option<SddPtr<'a>>;
    fn app_cache_insert(&self, and: SddAnd<'a>, ptr: SddPtr<'a>);

    fn ite_cache_hash(&self, ite: &Ite<SddPtr>) -> u64;
    fn ite_cache_get(&self, ite: Ite<SddPtr<'a>>, hash: u64) -> Option<SddPtr>;
    fn ite_cache_insert(&self, ite: Ite<SddPtr<'a>>, res: SddPtr<'a>, hash: u64);

    fn get_or_insert_bdd(&'a self, bdd: BinarySDD<'a>) -> SddPtr<'a>;
    fn get_or_insert_sdd(&'a self, or: SddOr<'a>) -> SddPtr<'a>;
    fn node_iter(&self) -> Vec<SddPtr>;

    // equality
    fn sdd_eq(&'a self, a: SddPtr<'a>, b: SddPtr<'a>) -> bool;

    fn is_true(&'a self, a: SddPtr<'a>) -> bool {
        self.eq(a, SddPtr::PtrTrue)
    }

    fn is_false(&'a self, a: SddPtr<'a>) -> bool {
        self.eq(a, SddPtr::PtrFalse)
    }

    // compression & canonicalization
    fn set_compression(&mut self, b: bool);
    fn compress(&'a self, node: &mut Vec<SddAnd<'a>>);
    fn canonicalize(&'a self, node: Vec<SddAnd<'a>>, table: VTreeIndex) -> SddPtr<'a>;

    fn unique_or(&'a self, mut node: Vec<SddAnd<'a>>, table: VTreeIndex) -> SddPtr<'a> {
        // check if it is a BDD; if it is, return that
        if node.len() == 2 {
            if let SddPtr::Var(_, polarity) = node[0].prime() {
                if let SddPtr::Var(label, _) = node[1].prime() {
                    // this is a BDD
                    // this SDD may be unsorted, so extract the low and high value
                    // based on whether or not node[0]'s prime is negated
                    let low = if !polarity {
                        node[0].sub()
                    } else {
                        node[1].sub()
                    };
                    let high = if polarity {
                        node[0].sub()
                    } else {
                        node[1].sub()
                    };
                    return self.unique_bdd(BinarySDD::new(label, low, high, table));
                }
            }
        }

        node.sort_by_key(|a| a.prime());
        if node[0].sub().is_neg() || self.is_false(node[0].sub()) || node[0].sub().is_neg_var() {
            for x in node.iter_mut() {
                *x = SddAnd::new(x.prime(), x.sub().neg());
            }
            self.get_or_insert_sdd(SddOr::new(node, table)).neg()
        } else {
            self.get_or_insert_sdd(SddOr::new(node, table))
        }
    }

    fn unique_bdd(&'a self, bdd: BinarySDD<'a>) -> SddPtr<'a> {
        if self.eq(bdd.high(), bdd.low()) {
            return bdd.high();
        }
        if self.is_false(bdd.high()) && self.is_true(bdd.low()) {
            return SddPtr::Var(bdd.label(), false);
        }
        if self.is_true(bdd.high()) && self.is_false(bdd.low()) {
            return SddPtr::Var(bdd.label(), true);
        }

        if bdd.high().is_neg() || self.is_false(bdd.high()) || bdd.high().is_neg_var() {
            let low = bdd.low().neg();
            let high = bdd.high().neg();
            let neg_bdd = BinarySDD::new(bdd.label(), low, high, bdd.index());

            return self.get_or_insert_bdd(neg_bdd).neg();
        }

        self.get_or_insert_bdd(bdd)
    }

    // calc
    /// conjoin two SDDs that are in independent vtrees
    /// a is prime to b
    fn and_indep(&'a self, a: SddPtr<'a>, b: SddPtr<'a>, lca: VTreeIndex) -> SddPtr<'a> {
        // check if this is a right-linear fragment and construct the relevant SDD type
        if self.vtree_manager().vtree(lca).is_right_linear() {
            // a is a right-linear decision for b; construct a binary decision
            let bdd = match a {
                SddPtr::Var(label, true) => BinarySDD::new(label, SddPtr::false_ptr(), b, lca),
                SddPtr::Var(label, false) => BinarySDD::new(label, b, SddPtr::false_ptr(), lca),
                _ => panic!(
                    "Assumed that a is a right-linear decision for b, but a is not a variable"
                ),
            };
            self.unique_bdd(bdd)
        } else {
            let node = vec![SddAnd::new(a, b), SddAnd::new(a.neg(), SddPtr::false_ptr())];
            // no need to canonicalize, guaranteed canonical by construction
            self.unique_or(node, lca)
        }
    }

    /// conjoin SDDs where `d` is a descendent of `r`, and `d` is sub to `r`
    /// i.e. for vtree
    ///       3
    ///    1       5
    ///  0  2    4   6
    /// r might be wrt. node 3, and d wrt. node 5
    fn and_sub_desc(&'a self, r: SddPtr<'a>, d: SddPtr<'a>) -> SddPtr<'a> {
        // check if `r` is a bdd and handle that case
        match r {
            SddPtr::BDD(bdd) | SddPtr::ComplBDD(bdd) => {
                let l = self.and(r.low(), d);
                let h = self.and(r.high(), d);
                self.unique_bdd(BinarySDD::new(bdd.label(), l, h, bdd.index()))
            }
            SddPtr::Reg(or) | SddPtr::Compl(or) => {
                let mut v: Vec<SddAnd> = Vec::with_capacity(or.iter().len());
                for a in or.iter() {
                    let root_p = a.prime();
                    let root_s = a.sub();
                    let root_s = if r.is_neg() { root_s.neg() } else { root_s };
                    let new_s = self.and(root_s, d);
                    v.push(SddAnd::new(root_p, new_s));
                }
                self.canonicalize(v, or.index())
            }
            _ => panic!("Called and_sub_desc() on a constant"),
        }
    }

    /// conjoin SDDs where `d` is a descendent of `r`, and `r` is sub to `d`
    /// i.e. for vtree
    ///       3
    ///    1       5
    ///  0  2    4   6
    /// r might be wrt. node 3, and d wrt. node 1
    fn and_prime_desc(&'a self, r: SddPtr<'a>, d: SddPtr<'a>) -> SddPtr<'a> {
        // first handle the BDD case
        // if r.is_bdd() {
        //     // d is the topvar of r
        //     assert!(d.is_var());
        //     let n = if d.is_neg_var() {
        //         // d's low edge is True and high edge is False
        //         // conjoin these with r
        //         BinarySDD::new(r.topvar(), r.low(), SddPtr::false_ptr())
        //     } else {
        //         // d's high edge is True and low edge is False
        //         BinarySDD::new(r.topvar(), SddPtr::false_ptr(), r.high())
        //     };

        //     return self.unique_bdd(n);
        // }

        // normalized structure:
        //       o
        //   /      \
        // [d,T]  [!d, F]

        // first, trim by seeing if any primes in `r` are equal to `d` or its negation
        // for a in r.node_iter() {
        //     let s = if r.is_compl() { a.sub().neg() } else { a.sub() };
        //     if a.prime() == d {
        //         return s;
        //     }
        // }

        // no need to canonicalize -- cannot introduce new subs
        // return self.unique_or(v, r.vtree());

        // TODO optimize this for special cases
        let b = [
            SddAnd::new(d, SddPtr::true_ptr()),
            SddAnd::new(d.neg(), SddPtr::false_ptr()),
        ];

        let mut new_n: Vec<SddAnd> = Vec::new();
        for a1 in r.node_iter() {
            let p1 = a1.prime();
            let s1 = a1.sub();
            let s1 = if r.is_neg() { s1.neg() } else { s1 };
            // no special case
            for a2 in b.iter() {
                let p2 = a2.prime();
                let s2 = a2.sub();
                let p = self.and(p1, p2);
                if self.is_false(p) {
                    continue;
                }
                let s = self.and(s1, s2);
                // check if one of the nodes is true; if it is, we can
                // return a `true` SddPtr here, for trimming
                if self.is_true(p) && self.is_true(s) {
                    let new_v = SddPtr::true_ptr();
                    return new_v;
                }
                new_n.push(SddAnd::new(p, s));
            }
        }

        // canonicalize
        self.canonicalize(new_n, r.vtree())

        // for a in r.node_iter() {
        //     let p = self.and(a.prime(), d);
        //     let s = if r.is_compl() { a.sub().neg() } else { a.sub() };
        //     if self.is_false(p) {
        //         continue;
        //     }
        //     v.push(SddAnd::new(p, s));
        // }
        // return self.canonicalize(v, r.vtree());
    }

    /// conjoin SDDs where `a` and `b` are wrt. the same vtree node
    fn and_cartesian(&'a self, a: SddPtr<'a>, b: SddPtr<'a>, lca: VTreeIndex) -> SddPtr<'a> {
        // check if a and b are both binary SDDs; if so, we apply BDD conjunction here

        if let SddPtr::BDD(or) | SddPtr::ComplBDD(or) = a {
            if self.vtree_manager().vtree(lca).is_right_linear() {
                let l = self.and(a.low(), b.low());
                let h = self.and(a.high(), b.high());
                return self.unique_bdd(BinarySDD::new(or.label(), l, h, lca));
            }
        }
        // now the SDDs are normalized with respect to the same vtree, so we can
        // apply the prime/sub pairs

        // iterate over each prime/sub pair and do the relevant application
        // there are 2 simplifying cases:
        // (1) if p1i = p2j or if p1i => p2j, then p1k x p2j = false for all k<>i
        // (2) if p1i = !p2j, then p1k x p2j = p1k for all k<>i
        let mut r: Vec<SddAnd> = Vec::new();
        for a1 in a.node_iter() {
            let p1 = a1.prime();
            let s1 = a1.sub();
            // // check if there exists an equal prime
            let eq_itm = b.node_iter().find(|a| self.eq(a.prime(), p1));
            let s1 = if a.is_neg() { s1.neg() } else { s1 };

            match eq_itm {
                None => (),
                Some(andb) => {
                    let s2 = if b.is_neg() {
                        andb.sub().neg()
                    } else {
                        andb.sub()
                    };
                    // this sub is the only one with a non-false prime, so no need to iterate
                    r.push(SddAnd::new(p1, self.and(s1, s2)));
                    continue;
                }
            }

            // no special case
            for a2 in b.node_iter() {
                let p2 = a2.prime();
                let s2 = a2.sub();
                let s2 = if b.is_neg() { s2.neg() } else { s2 };
                let p = self.and(p1, p2);
                if self.is_false(p) {
                    continue;
                }
                let s = self.and(s1, s2);
                // check if one of the nodes is true; if it is, we can
                // return a `true` SddPtr here, for trimming
                if self.is_true(p) && self.is_true(s) {
                    let new_v = SddPtr::true_ptr();
                    return new_v;
                }
                r.push(SddAnd::new(p, s));
                // check if p1 => p2 (which is true iff (p1 && p2) == p1); if it
                // does we can stop early because the rest of the primes will be
                // false
                if self.eq(p1, p) {
                    break;
                }
            }
        }

        // canonicalize
        self.canonicalize(r, lca)
    }

    // helpers

    fn num_vars(&self) -> usize {
        self.vtree_manager().num_vars()
    }

    fn vtree(&self, ptr: SddPtr) -> &VTree {
        match ptr {
            SddPtr::Var(lbl, _) => {
                let idx = self.vtree_manager().var_index(lbl);
                self.vtree_manager().vtree(idx)
            }
            SddPtr::Compl(_) | SddPtr::Reg(_) => self.vtree_manager().vtree(ptr.vtree()),
            _ => panic!("called vtree on constant"),
        }
    }

    fn vtree_index(&self, ptr: SddPtr) -> VTreeIndex {
        match ptr {
            SddPtr::Var(lbl, _) => self.vtree_manager().var_index(lbl),
            SddPtr::BDD(_) | SddPtr::ComplBDD(_) | SddPtr::Compl(_) | SddPtr::Reg(_) => ptr.vtree(),
            _ => panic!("called vtree on constant"),
        }
    }

    fn compile_cnf_helper(&'a self, vec: &[SddPtr<'a>]) -> Option<SddPtr<'a>> {
        if vec.is_empty() {
            None
        } else if vec.len() == 1 {
            Some(vec[0])
        } else {
            let (l, r) = vec.split_at(vec.len() / 2);
            let sub_l = self.compile_cnf_helper(l);
            let sub_r = self.compile_cnf_helper(r);
            match (sub_l, sub_r) {
                (None, None) => None,
                (Some(v), None) | (None, Some(v)) => Some(v),
                (Some(l), Some(r)) => Some(self.and(l, r)),
            }
        }
    }

    fn print_sdd(&'a self, ptr: SddPtr<'a>) -> String {
        use pretty::*;
        fn helper(ptr: SddPtr) -> Doc<'static, BoxDoc<'static>> {
            match ptr {
                SddPtr::PtrTrue => Doc::from("T"),
                SddPtr::PtrFalse => Doc::from("F"),
                SddPtr::Var(label, polarity) => Doc::from(format!(
                    "{}{}",
                    if polarity { "" } else { "!" },
                    label.value()
                )),
                SddPtr::BDD(bdd) | SddPtr::ComplBDD(bdd) => {
                    let l = helper(bdd.low());
                    let h = helper(bdd.high());
                    let mut doc: Doc<BoxDoc> = Doc::from("");
                    doc = doc.append(Doc::newline()).append(
                        (Doc::from(format!("ITE {:?} {}", ptr, bdd.label().value()))
                            .append(Doc::newline())
                            .append(h.append(Doc::newline()).append(l)))
                        .nest(2),
                    );
                    doc
                }
                SddPtr::Reg(or) | SddPtr::Compl(or) => {
                    let mut doc: Doc<BoxDoc> = Doc::from("");
                    for a in or.iter() {
                        let sub = a.sub();
                        let prime = a.prime();
                        let s = if ptr.is_neg() { sub.neg() } else { sub };
                        let new_s1 = helper(prime);
                        let new_s2 = helper(s);
                        doc = doc.append(Doc::newline()).append(
                            (Doc::from("/\\")
                                .append(Doc::newline())
                                .append(new_s1.append(Doc::newline()).append(new_s2)))
                            .nest(2),
                        );
                    }
                    let d = Doc::from(format!("\\/ {:?}", ptr));
                    d.append(doc.nest(2))
                }
            }
        }
        let d = helper(ptr);
        let mut w = Vec::new();
        d.render(10, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }

    fn stats(&self) -> SddBuilderStats;
    fn log_recursive_call(&self);
}

impl<'a, T> BottomUpBuilder<'a, SddPtr<'a>> for T
where
    T: SddBuilder<'a>,
{
    #[inline]
    fn true_ptr(&self) -> SddPtr<'a> {
        SddPtr::PtrTrue
    }

    #[inline]
    fn false_ptr(&self) -> SddPtr<'a> {
        SddPtr::PtrFalse
    }

    #[inline]
    fn var(&'a self, label: VarLabel, polarity: bool) -> SddPtr<'a> {
        SddPtr::Var(label, polarity)
    }

    #[inline]
    fn negate(&'a self, f: SddPtr<'a>) -> SddPtr<'a> {
        f.neg()
    }

    fn eq(&'a self, a: SddPtr<'a>, b: SddPtr<'a>) -> bool {
        self.sdd_eq(a, b)
    }

    fn and(&'a self, a: SddPtr<'a>, b: SddPtr<'a>) -> SddPtr<'a> {
        self.log_recursive_call();

        // first, check for a base case
        match (a, b) {
            (a, b) if self.is_true(a) => return b,
            (a, b) if self.is_true(b) => return a,
            (a, _) if self.is_false(a) => return SddPtr::false_ptr(),
            (_, b) if self.is_false(b) => return SddPtr::false_ptr(),
            (a, b) if self.eq(a, b) => return a,
            (a, b) if self.eq(a, b.neg()) => return SddPtr::false_ptr(),
            _ => (),
        };

        // normalize so `a` is always prime if possible
        let (a, b) = if self.vtree_index(a) == self.vtree_index(b)
            || self
                .vtree_manager()
                .is_prime_index(self.vtree_index(a), self.vtree_index(b))
        {
            (a, b)
        } else {
            (b, a)
        };

        // check if we have this application cached
        if let Some(x) = self.app_cache_get(&SddAnd::new(a, b)) {
            return x;
        }

        let av = self.vtree_index(a);
        let bv = self.vtree_index(b);
        let lca = self.vtree_manager().lca(av, bv);

        // now we determine the current iterator for primes and subs
        // consider the following example vtree:
        //       3
        //    1       5
        //  0  2    4   6
        // 4 cases:
        //   1. `a` and `b` have the same vtree
        //   2. The lca is `a`
        //   3. The lca is `b`
        //   4. The lca is a shared parent equal to neither
        // we can only conjoin two SDD nodes if they are normalized with respect
        // to the same vtree; the following code does this for each of the
        // above cases.
        let r = if av == bv {
            self.and_cartesian(a, b, lca)
        } else if lca == av {
            self.and_sub_desc(a, b)
        } else if lca == bv {
            self.and_prime_desc(b, a)
        } else {
            self.and_indep(a, b, lca)
        };

        // cache and return
        self.app_cache_insert(SddAnd::new(a, b), r);
        r
    }

    /// Computes `f | var = value`
    /// TODO: This is highly inefficient, will re-traverse nodes, needs a cache
    /// TODO : this can bail out early by checking the vtree
    fn condition(&'a self, f: SddPtr<'a>, lbl: VarLabel, value: bool) -> SddPtr<'a> {
        self.log_recursive_call();
        match f {
            SddPtr::PtrTrue | SddPtr::PtrFalse => f,
            SddPtr::Var(label, polarity) => {
                if label == lbl {
                    if polarity == value {
                        SddPtr::PtrTrue
                    } else {
                        SddPtr::PtrFalse
                    }
                } else {
                    f
                }
            }
            // if f.is_bdd() {
            //     if f.topvar() == lbl {
            //         if !f.is_compl() == value {
            //             return f.high();
            //         } else {
            //             return f.low();
            //         }
            //     } else {
            //         let l = self.condition(f.low(), lbl, value);
            //         let h = self.condition(f.high(), lbl, value);
            //         return self.unique_bdd(BinarySDD::new(f.topvar(), l, h));
            //     }
            // }
            _ => {
                let mut v = Vec::new();
                // f is a node; recurse and compress the result
                for a in f.node_iter() {
                    let prime = a.prime();
                    let sub = a.sub();
                    let newp = self.condition(prime, lbl, value);
                    let sub = if f.is_neg() { sub.neg() } else { sub };
                    if self.is_false(newp) {
                        continue;
                    };
                    let news = self.condition(sub, lbl, value);
                    if self.is_true(newp) {
                        return news;
                    }
                    v.push(SddAnd::new(newp, news));
                }
                self.canonicalize(v, f.vtree())
            }
        }
    }

    /// Computes the SDD representing the logical function `if f then g else h`
    fn ite(&'a self, f: SddPtr<'a>, g: SddPtr<'a>, h: SddPtr<'a>) -> SddPtr<'a> {
        let ite = Ite::new(|a, b| self.vtree_manager().is_prime(a, b), f, g, h);
        if let Ite::IteConst(f) = ite {
            return f;
        }

        let hash = self.ite_cache_hash(&ite);
        if let Some(v) = self.ite_cache_get(ite, hash) {
            return v;
        }

        // TODO make this a primitive operation
        let fg = self.and(f, g);
        let negfh = self.and(f.neg(), h);
        let r = self.or(fg, negfh);
        self.ite_cache_insert(ite, r, hash);
        r
    }

    /// Computes the SDD representing the logical function `f <=> g`
    fn iff(&'a self, f: SddPtr<'a>, g: SddPtr<'a>) -> SddPtr<'a> {
        self.ite(f, g, g.neg())
    }

    /// Computes the SDD representing the logical function `f xor g`
    fn xor(&'a self, f: SddPtr<'a>, g: SddPtr<'a>) -> SddPtr<'a> {
        self.ite(f, g.neg(), g)
    }

    /// Existentially quantifies out the variable `lbl` from `f`
    fn exists(&'a self, sdd: SddPtr<'a>, lbl: VarLabel) -> SddPtr<'a> {
        // TODO this can be optimized by specializing it
        let v1 = self.condition(sdd, lbl, true);
        let v2 = self.condition(sdd, lbl, false);
        self.or(v1, v2)
    }

    /// compile an SDD from an input CNF
    fn compile_cnf(&'a self, cnf: &Cnf) -> SddPtr<'a> {
        let mut cvec: Vec<SddPtr> = Vec::with_capacity(cnf.clauses().len());
        if cnf.clauses().is_empty() {
            return SddPtr::true_ptr();
        }
        // check if there is an empty clause -- if so, UNSAT
        if cnf.clauses().iter().any(|x| x.is_empty()) {
            return SddPtr::false_ptr();
        }

        // sort the clauses based on a best-effort bottom-up ordering of clauses
        let mut cnf_sorted = cnf.clauses().to_vec();
        cnf_sorted.sort_by(|c1, c2| {
            // order the clause with the first-most variable last
            let fst1 = c1
                .iter()
                .max_by(|l1, l2| {
                    if self.vtree_manager().is_prime_var(l1.label(), l2.label()) {
                        Ordering::Less
                    } else {
                        Ordering::Equal
                    }
                })
                .unwrap();
            let fst2 = c2
                .iter()
                .max_by(|l1, l2| {
                    if self.vtree_manager().is_prime_var(l1.label(), l2.label()) {
                        Ordering::Less
                    } else {
                        Ordering::Equal
                    }
                })
                .unwrap();
            if self
                .vtree_manager()
                .is_prime_var(fst1.label(), fst2.label())
            {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        });

        for lit_vec in cnf_sorted.iter() {
            let (vlabel, val) = (lit_vec[0].label(), lit_vec[0].polarity());
            let mut bdd = SddPtr::Var(vlabel, val);
            for lit in lit_vec {
                let (vlabel, val) = (lit.label(), lit.polarity());
                let var = SddPtr::Var(vlabel, val);
                bdd = self.or(bdd, var);
            }
            cvec.push(bdd);
        }
        // now cvec has a list of all the clauses; collapse it down
        let r = self.compile_cnf_helper(&cvec);
        match r {
            None => SddPtr::true_ptr(),
            Some(x) => x,
        }
    }
}
