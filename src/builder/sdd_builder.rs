//! The main implementation of the SDD manager, the primary way of interacting
//! with SDDs.

use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};

use super::cache::all_app::AllTable;
use super::cache::ite::Ite;
use super::cache::LruTable;

use crate::backing_store::bump_table::BackedRobinhoodTable;
use crate::backing_store::UniqueTable;
use crate::repr::ddnnf::DDNNFPtr;
use crate::repr::robdd::create_semantic_hash_map;
use crate::repr::sdd::binary_sdd::BinarySDD;
use crate::repr::sdd::sdd_or::{SddAnd, SddOr};
use crate::repr::sdd::SddPtr;
use crate::repr::vtree::{VTree, VTreeIndex, VTreeManager};
use crate::{repr::cnf::Cnf, repr::logical_expr::LogicalExpr, repr::var_label::VarLabel};

#[derive(Debug, Clone)]
pub struct SddStats {
    /// total number of recursive calls
    pub num_rec: usize,
    /// total number of calls to compress
    pub num_compr: usize,
    /// total number of new SddAnds generated in compress
    pub num_compr_and: usize,
    /// total number of gets/inserts generated in compress
    pub num_get_or_insert: usize,
    /// app cache hits
    pub num_app_cache_hits: usize,
}

impl SddStats {
    pub fn new() -> SddStats {
        SddStats {
            num_rec: 0,
            num_compr: 0,
            num_compr_and: 0,
            num_get_or_insert: 0,
            num_app_cache_hits: 0,
        }
    }
}

impl Default for SddStats {
    fn default() -> Self {
        Self::new()
    }
}

pub struct SddManager<'a> {
    vtree: VTreeManager,
    stats: SddStats,
    should_compress: bool,
    // tables
    bdd_tbl: RefCell<BackedRobinhoodTable<'a, BinarySDD<'a>>>,
    sdd_tbl: RefCell<BackedRobinhoodTable<'a, SddOr<'a>>>,
    // caches
    ite_cache: RefCell<AllTable<SddPtr<'a>>>,
    app_cache: RefCell<HashMap<SddAnd<'a>, SddPtr<'a>>>,
}

impl<'a> SddManager<'a> {
    pub fn new(vtree: VTree) -> SddManager<'a> {
        let vtree_man = VTreeManager::new(vtree);
        SddManager {
            stats: SddStats::new(),
            ite_cache: RefCell::new(AllTable::new()),
            app_cache: RefCell::new(HashMap::new()),
            bdd_tbl: RefCell::new(BackedRobinhoodTable::new()),
            sdd_tbl: RefCell::new(BackedRobinhoodTable::new()),
            vtree: vtree_man,
            should_compress: true,
        }
    }

    pub fn set_compression(&mut self, b: bool) {
        self.should_compress = false
    }

    pub fn get_vtree_root(&self) -> &VTree {
        self.vtree.vtree_root()
    }

    pub fn get_vtree_manager(&self) -> &VTreeManager {
        &self.vtree
    }

    pub fn num_vars(&self) -> usize {
        self.vtree.num_vars()
    }

    /// Canonicalizes the list of (prime, sub) terms in-place
    /// `node`: a list of (prime, sub) pairs
    fn compress(&'a self, node: &mut Vec<SddAnd<'a>>) {
        // self.stats.num_compr += 1;
        for i in 0..node.len() {
            // see if we can compress i
            let mut j = i + 1;
            while j < node.len() {
                if self.sdd_eq(node[i].sub(), node[j].sub()) {
                    // self.stats.num_compr_and += 1;
                    // compress j into i and remove j from the node list
                    node[i] = SddAnd::new(self.or(node[i].prime(), node[j].prime()), node[i].sub());
                    node.swap_remove(j);
                } else {
                    j += 1;
                }
            }
        }
    }

    pub fn get_vtree(&self, ptr: SddPtr) -> &VTree {
        match ptr {
            SddPtr::Var(lbl, _) => {
                let idx = self.vtree.get_varlabel_idx(lbl);
                self.vtree.get_idx(idx)
            }
            SddPtr::Compl(_) | SddPtr::Reg(_) => self.vtree.get_idx(ptr.vtree()),
            _ => panic!("called vtree on constant"),
        }
    }

    pub fn get_vtree_idx(&self, ptr: SddPtr) -> VTreeIndex {
        match ptr {
            SddPtr::Var(lbl, _) => self.vtree.get_varlabel_idx(lbl),
            SddPtr::BDD(_) | SddPtr::ComplBDD(_) | SddPtr::Compl(_) | SddPtr::Reg(_) => ptr.vtree(),
            _ => panic!("called vtree on constant"),
        }
    }

    fn unique_or(&'a self, mut node: Vec<SddAnd<'a>>, table: VTreeIndex) -> SddPtr<'a> {
        // check if it is a BDD; if it is, return that
        if node.len() == 2 && node[0].prime().is_var() && node[1].prime().is_var() {
            // this is a BDD
            // this SDD may be unsorted, so extract the low and high value
            // based on whether or not node[0]'s prime is negated
            let v = node[1].prime().get_var().get_label();
            let low = if node[0].prime().is_neg_var() {
                node[0].sub()
            } else {
                node[1].sub()
            };
            let high = if node[0].prime().is_pos_var() {
                node[0].sub()
            } else {
                node[1].sub()
            };
            self.unique_bdd(BinarySDD::new(v, low, high, table))
        } else {
            node.sort_by_key(|a| a.prime());
            if node[0].sub().is_neg() || self.is_false(node[0].sub()) || node[0].sub().is_neg_var()
            {
                for x in node.iter_mut() {
                    *x = SddAnd::new(x.prime(), x.sub().neg());
                }
                SddPtr::Reg(
                    self.sdd_tbl
                        .borrow_mut()
                        .get_or_insert(SddOr::new(node, table)),
                )
                .neg()
            } else {
                SddPtr::Reg(
                    self.sdd_tbl
                        .borrow_mut()
                        .get_or_insert(SddOr::new(node, table)),
                )
            }
        }
    }

    fn unique_bdd(&'a self, bdd: BinarySDD<'a>) -> SddPtr<'a> {
        if bdd.high() == bdd.low() {
            return bdd.high();
        }
        if self.is_false(bdd.high()) && self.is_true(bdd.low()) {
            return SddPtr::var(bdd.label(), false);
        }
        if self.is_true(bdd.high()) && self.is_false(bdd.low()) {
            return SddPtr::var(bdd.label(), true);
        }

        if bdd.high().is_neg() || self.is_false(bdd.high()) || bdd.high().is_neg_var() {
            let low = bdd.low().neg();
            let high = bdd.high().neg();
            let neg_bdd = BinarySDD::new(bdd.label(), low, high, bdd.vtree());

            let unique = self.bdd_tbl.borrow_mut().get_or_insert(neg_bdd);

            return SddPtr::BDD(unique).neg();
        }

        SddPtr::BDD(self.bdd_tbl.borrow_mut().get_or_insert(bdd))
    }

    #[inline]
    fn canonicalize_base_case(&'a self, node: &Vec<SddAnd<'a>>) -> Option<SddPtr<'a>> {
        if node.is_empty() {
            return Some(SddPtr::true_ptr());
        }
        if node.len() == 1 {
            if self.is_true(node[0].prime()) {
                return Some(node[0].sub());
            }

            if self.is_false(node[0].sub()) {
                return Some(SddPtr::false_ptr());
            }
        }
        if node.len() == 2 {
            if self.is_true(node[0].sub()) && self.is_false(node[1].sub()) {
                return Some(node[0].prime());
            } else if self.is_false(node[0].sub()) && self.is_true(node[1].sub()) {
                return Some(node[1].prime());
            }
        }
        None
    }

    /// Returns a canonicalized SDD pointer from a list of (prime, sub) pairs
    fn canonicalize(&'a self, mut node: Vec<SddAnd<'a>>, table: VTreeIndex) -> SddPtr<'a> {
        // check for base cases before compression
        if let Some(sdd) = self.canonicalize_base_case(&node) {
            return sdd;
        }

        if self.should_compress {
            self.compress(&mut node);

            // check for a base case after compression (compression can sometimes
            // reduce node counts to a base case)
            if let Some(sdd) = self.canonicalize_base_case(&node) {
                return sdd;
            }
        }

        self.unique_or(node, table)
    }

    /// conjoin two SDDs that are in independent vtrees
    /// a is prime to b
    fn and_indep(&'a self, a: SddPtr<'a>, b: SddPtr<'a>, lca: VTreeIndex) -> SddPtr<'a> {
        // check if this is a right-linear fragment and construct the relevant SDD type
        if self.vtree.get_idx(lca).is_right_linear() {
            // a is a right-linear decision for b; construct a binary decision
            let bdd = if a.is_neg_var() {
                BinarySDD::new(a.get_var_label(), b, SddPtr::false_ptr(), lca)
            } else {
                BinarySDD::new(a.get_var_label(), SddPtr::false_ptr(), b, lca)
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
        if r.is_bdd() {
            let l = self.and(r.low(), d);
            let h = self.and(r.high(), d);
            return self.unique_bdd(BinarySDD::new(r.topvar(), l, h, r.vtree()));
        }

        let mut v: Vec<SddAnd> = Vec::with_capacity(r.num_nodes());
        for a in r.node_iter() {
            let root_p = a.prime();
            let root_s = a.sub();
            let root_s = if r.is_neg() { root_s.neg() } else { root_s };
            let new_s = self.and(root_s, d);
            v.push(SddAnd::new(root_p, new_s));
        }
        self.canonicalize(v, r.vtree())
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
        let b = vec![
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
        if a.is_bdd() && self.vtree.get_idx(lca).is_right_linear() {
            let l = self.and(a.low(), b.low());
            let h = self.and(a.high(), b.high());
            return self.unique_bdd(BinarySDD::new(a.topvar(), l, h, lca));
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
            let eq_itm = b.node_iter().find(|a| self.sdd_eq(a.prime(), p1));
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
                if self.sdd_eq(p1, p) {
                    break;
                }
            }
        }

        // canonicalize
        self.canonicalize(r, lca)
    }

    pub fn and(&'a self, a: SddPtr<'a>, b: SddPtr<'a>) -> SddPtr<'a> {
        // println!("and a: {}\nb: {}", self.print_sdd(a), self.print_sdd(b));
        // first, check for a base case
        match (a, b) {
            (a, b) if self.is_true(a) => return b,
            (a, b) if self.is_true(b) => return a,
            (a, _) if self.is_false(a) => return SddPtr::false_ptr(),
            (_, b) if self.is_false(b) => return SddPtr::false_ptr(),
            (a, b) if self.sdd_eq(a, b) => return a,
            (a, b) if self.sdd_eq(a, b.neg()) => return SddPtr::false_ptr(),
            _ => (),
        };

        // normalize so `a` is always prime if possible
        let (a, b) = if self.get_vtree_idx(a) == self.get_vtree_idx(b)
            || self
                .vtree
                .is_prime_index(self.get_vtree_idx(a), self.get_vtree_idx(b))
        {
            (a, b)
        } else {
            (b, a)
        };

        let mut app_cache = self.app_cache.borrow_mut();

        // check if we have this application cached
        if let Some(x) = app_cache.get(&SddAnd::new(a, b)) {
            // self.stats.num_app_cache_hits += 1;
            return *x;
        }

        let av = self.get_vtree_idx(a);
        let bv = self.get_vtree_idx(b);
        let lca = self.vtree.lca(av, bv);

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
        app_cache.insert(SddAnd::new(a, b), r);
        r
    }

    pub fn or(&'a self, a: SddPtr<'a>, b: SddPtr<'a>) -> SddPtr<'a> {
        self.and(a.neg(), b.neg()).neg()
    }

    /// Computes `f | var = value`
    /// TODO: This is highly inefficient, will re-traverse nodes, needs a cache
    pub fn condition(&'a self, f: SddPtr<'a>, lbl: VarLabel, value: bool) -> SddPtr<'a> {
        // self.stats.num_rec += 1;
        // TODO : this can bail out early by checking the vtree
        // check base case
        if f.is_const() {
            return f;
        };
        if f.is_var() {
            let l = f.get_var();
            if l.get_label() == lbl {
                // condition this var
                if l.get_polarity() == value {
                    return SddPtr::true_ptr();
                } else {
                    return SddPtr::false_ptr();
                }
            } else {
                return f;
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

    /// Computes the SDD representing the logical function `if f then g else h`
    pub fn ite(&'a self, f: SddPtr<'a>, g: SddPtr<'a>, h: SddPtr<'a>) -> SddPtr<'a> {
        let ite = Ite::new(|a, b| self.vtree.is_prime(a, b), f, g, h);
        if let Ite::IteConst(f) = ite {
            return f;
        }

        let hash = self.ite_cache.borrow().hash(&ite);
        if let Some(v) = self.ite_cache.borrow().get(ite, hash) {
            return v;
        }

        // TODO make this a primitive operation
        let fg = self.and(f, g);
        let negfh = self.and(f.neg(), h);
        let r = self.or(fg, negfh);
        self.ite_cache.borrow_mut().insert(ite, r, hash);
        r
    }

    /// Computes the SDD representing the logical function `f <=> g`
    pub fn iff(&'a self, f: SddPtr<'a>, g: SddPtr<'a>) -> SddPtr<'a> {
        self.ite(f, g, g.neg())
    }

    /// Computes the SDD representing the logical function `f xor g`
    pub fn xor(&'a self, f: SddPtr<'a>, g: SddPtr<'a>) -> SddPtr<'a> {
        self.ite(f, g.neg(), g)
    }

    /// Existentially quantifies out the variable `lbl` from `f`
    pub fn exists(&'a self, sdd: SddPtr<'a>, lbl: VarLabel) -> SddPtr<'a> {
        // TODO this can be optimized by specializing it
        let v1 = self.condition(sdd, lbl, true);
        let v2 = self.condition(sdd, lbl, false);
        self.or(v1, v2)
    }

    /// Compose `g` into `f` by substituting for `lbl`
    pub fn compose(&'a self, _f: SddPtr<'a>, _lbl: VarLabel, _g: SddPtr<'a>) -> SddPtr<'a> {
        panic!("not impl")
        // TODO this can be optimized with a specialized implementation to make
        // it a single traversal
        // let var = self.var(lbl, true);
        // let iff = self.iff(var, g);
        // let a = self.and(iff, f);
        // self.exists(a, lbl)
    }

    fn print_sdd_internal(&'a self, ptr: SddPtr<'a>) -> String {
        // self.canonicalizer.borrow().on_sdd_print_dump_state(ptr);
        use pretty::*;
        fn helper(ptr: SddPtr) -> Doc<'static, BoxDoc<'static>> {
            if ptr.is_true() {
                return Doc::from("T");
            } else if ptr.is_false() {
                return Doc::from("F");
            } else if ptr.is_var() {
                let l = ptr.get_var();
                return Doc::from(format!(
                    "{}{}",
                    if l.get_polarity() { "" } else { "!" },
                    l.get_label().value()
                ));
            } else if ptr.is_bdd() {
                let l = helper(ptr.low_raw());
                let h = helper(ptr.high_raw());
                let mut doc: Doc<BoxDoc> = Doc::from("");
                doc = doc.append(Doc::newline()).append(
                    (Doc::from(format!("ITE {:?} {}", ptr, ptr.topvar().value()))
                        .append(Doc::newline())
                        .append(h.append(Doc::newline()).append(l)))
                    .nest(2),
                );
                return doc;
            }

            let mut doc: Doc<BoxDoc> = Doc::from("");
            for a in ptr.node_iter() {
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
        let d = helper(ptr);
        let mut w = Vec::new();
        d.render(10, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }

    pub fn print_sdd(&'a self, ptr: SddPtr<'a>) -> String {
        self.print_sdd_internal(ptr)
    }

    pub fn sdd_eq(&'a self, a: SddPtr<'a>, b: SddPtr<'a>) -> bool {
        a == b
    }

    pub fn is_true(&'a self, a: SddPtr<'a>) -> bool {
        self.sdd_eq(a, SddPtr::PtrTrue)
    }

    pub fn is_false(&'a self, a: SddPtr<'a>) -> bool {
        self.sdd_eq(a, SddPtr::PtrFalse)
    }

    /// compile an SDD from an input CNF
    pub fn from_cnf(&'a self, cnf: &Cnf) -> SddPtr<'a> {
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
                    if self.vtree.is_prime_var(l1.get_label(), l2.get_label()) {
                        Ordering::Less
                    } else {
                        Ordering::Equal
                    }
                })
                .unwrap();
            let fst2 = c2
                .iter()
                .max_by(|l1, l2| {
                    if self.vtree.is_prime_var(l1.get_label(), l2.get_label()) {
                        Ordering::Less
                    } else {
                        Ordering::Equal
                    }
                })
                .unwrap();
            if self.vtree.is_prime_var(fst1.get_label(), fst2.get_label()) {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        });

        for lit_vec in cnf_sorted.iter() {
            let (vlabel, val) = (lit_vec[0].get_label(), lit_vec[0].get_polarity());
            let mut bdd = SddPtr::var(vlabel, val);
            for lit in lit_vec {
                let (vlabel, val) = (lit.get_label(), lit.get_polarity());
                let var = SddPtr::var(vlabel, val);
                bdd = self.or(bdd, var);
            }
            cvec.push(bdd);
        }
        // now cvec has a list of all the clauses; collapse it down
        let r = self.from_cnf_helper(&cvec);
        match r {
            None => SddPtr::true_ptr(),
            Some(x) => x,
        }
    }

    fn from_cnf_helper(&'a self, vec: &[SddPtr<'a>]) -> Option<SddPtr<'a>> {
        if vec.is_empty() {
            None
        } else if vec.len() == 1 {
            Some(vec[0])
        } else {
            let (l, r) = vec.split_at(vec.len() / 2);
            let sub_l = self.from_cnf_helper(l);
            let sub_r = self.from_cnf_helper(r);
            match (sub_l, sub_r) {
                (None, None) => None,
                (Some(v), None) | (None, Some(v)) => Some(v),
                (Some(l), Some(r)) => Some(self.and(l, r)),
            }
        }
    }

    pub fn from_logical_expr(&mut self, _expr: &LogicalExpr) -> SddPtr {
        panic!("not impl")
        // match expr {
        //     &LogicalExpr::Literal(lbl, polarity) => self.var(VarLabel::new(lbl as u64), polarity),
        //     &LogicalExpr::Not(ref e) => {
        //         let e = self.from_logical_expr(e);
        //         e.neg()
        //     }
        //     &LogicalExpr::And(ref l, ref r) => {
        //         let r1 = self.from_logical_expr(l);
        //         let r2 = self.from_logical_expr(r);
        //         self.and(r1, r2)
        //     }
        //     &LogicalExpr::Or(ref l, ref r) => {
        //         let r1 = self.from_logical_expr(l);
        //         let r2 = self.from_logical_expr(r);
        //         self.or(r1, r2)
        //     }
        //     &LogicalExpr::Xor(ref l, ref r) => {
        //         let r1 = self.from_logical_expr(l);
        //         let r2 = self.from_logical_expr(r);
        //         self.xor(r1, r2)
        //     }
        //     &LogicalExpr::Iff(ref l, ref r) => {
        //         let r1 = self.from_logical_expr(l);
        //         let r2 = self.from_logical_expr(r);
        //         self.iff(r1, r2)
        //     }
        //     &LogicalExpr::Ite {
        //         ref guard,
        //         ref thn,
        //         ref els,
        //     } => {
        //         let g = self.from_logical_expr(guard);
        //         let thn = self.from_logical_expr(thn);
        //         let els = self.from_logical_expr(els);
        //         self.ite(g, thn, els)
        //     }
        // }
    }

    pub fn stats(&self) -> &SddStats {
        &self.stats
    }

    fn node_iter(&self) -> Vec<SddPtr> {
        let binding = self.bdd_tbl.borrow_mut();
        let bdds = binding.iter().map(|x| SddPtr::bdd(x));
        let binding = self.sdd_tbl.borrow_mut();
        let sdds = binding.iter().map(|x| SddPtr::Reg(x));
        bdds.chain(sdds).collect()
    }

    /// computes the number of logically redundant nodes allocated by the
    /// manager (nodes that have the same semantic hash)
    pub fn num_logically_redundant(&self) -> usize {
        let mut s: HashSet<u128> = HashSet::new();
        let hasher = create_semantic_hash_map::<100000000063>(self.num_vars() + 1000); // TODO FIX THIS BADNESS
        let mut num_collisions = 0;
        for n in self.node_iter() {
            let h = n.cached_semantic_hash(self.get_vtree_manager(), &hasher);
            if s.contains(&h.value()) {
                num_collisions += 1;
            }
            s.insert(h.value());
        }
        num_collisions
    }
}

// check that (a \/ b) /\ a === a
#[test]
fn simple_equality() {
    let man = SddManager::new(VTree::even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        2,
    ));
    let a = SddPtr::var(VarLabel::new(0), true);
    let d = SddPtr::var(VarLabel::new(3), true);
    let inner = man.or(a, d);
    println!("0 || 3:\n{}", man.print_sdd(inner));
    let term = man.and(inner, a);
    assert_eq!(a, term);
}

// check that (a \/ b) | !b === a
#[test]
fn sdd_simple_cond() {
    let man = SddManager::new(VTree::even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        2,
    ));
    let a = SddPtr::var(VarLabel::new(0), true);
    let d = SddPtr::var(VarLabel::new(3), true);
    let inner = man.or(a, d);
    println!("0 || 3: {}", man.print_sdd(inner));
    let term = man.condition(inner, VarLabel::new(3), false);
    assert_eq!(
        a,
        term,
        "Got:\n{}\nexpected:\n{}\n",
        man.print_sdd(term),
        man.print_sdd(a)
    );
}

#[test]
fn sdd_test_exist() {
    let man = SddManager::new(VTree::even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        2,
    ));
    // 1 /\ 2 /\ 3
    let v1 = SddPtr::var(VarLabel::new(0), true);
    let v2 = SddPtr::var(VarLabel::new(1), true);
    let v3 = SddPtr::var(VarLabel::new(2), true);
    let a1 = man.and(v1, v2);
    let r1 = man.and(a1, v3);
    let r_expected = man.and(v1, v3);
    let res = man.exists(r1, VarLabel::new(1));
    assert!(
        man.sdd_eq(r_expected, res),
        "Got:\n{}\nExpected:\n{}",
        man.print_sdd(res),
        man.print_sdd(r_expected)
    );
}

#[test]
fn sdd_bigand() {
    let man = SddManager::new(VTree::right_linear(&[
        VarLabel::new(0),
        VarLabel::new(1),
        VarLabel::new(2),
        VarLabel::new(3),
        VarLabel::new(4),
    ]));

    // let man = SddManager::new(VTree::even_split(
    //     &[
    //         VarLabel::new(0),
    //         VarLabel::new(1),
    //         VarLabel::new(2),
    //         VarLabel::new(3),
    //         VarLabel::new(4),
    //     ],
    //     2,
    // ));
    // 1 /\ 2 /\ 3
    let v1 = SddPtr::var(VarLabel::new(0), true);
    let v2 = SddPtr::var(VarLabel::new(1), true);
    let v3 = SddPtr::var(VarLabel::new(2), true);
    let a1 = man.and(v1, v2);
    let r1 = man.and(a1, v3);
    let f = man.and(r1, SddPtr::var(VarLabel::new(0), false));
    println!("{}", man.print_sdd(r1));
    assert_eq!(
        f,
        SddPtr::false_ptr(),
        "Expected False, got {}",
        man.print_sdd(f)
    );
}

#[test]
fn sdd_ite1() {
    let man = SddManager::new(VTree::even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        2,
    ));
    let v1 = SddPtr::var(VarLabel::new(0), true);
    let v2 = SddPtr::var(VarLabel::new(1), true);
    println!("v1: {}", man.print_sdd(v1));
    println!("v2: {}", man.print_sdd(v2));
    let r1 = man.or(v1, v2);
    // r1: 0 \/ 1
    println!("0 || 1: {}", man.print_sdd(r1));
    let r2 = man.and(r1, v1);
    // r2: (0 \/ 1) && 1
    assert!(
        man.sdd_eq(v1, r2),
        "Not eq:\n {}\n{}",
        man.print_sdd(v1),
        man.print_sdd(r2)
    );
}

#[test]
fn sdd_demorgan() {
    let man = SddManager::new(VTree::even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        1,
    ));
    let x = SddPtr::var(VarLabel::new(0), true);
    let y = SddPtr::var(VarLabel::new(3), true);
    let res = man.or(x, y).neg();
    let expected = man.and(x.neg(), y.neg());
    assert!(
        man.sdd_eq(res, expected),
        "Not eq:\nGot: {}\nExpected: {}",
        man.print_sdd(res),
        man.print_sdd(expected)
    );
}

#[test]
fn sdd_circuit1() {
    let man = SddManager::new(VTree::even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        2,
    ));
    let x = SddPtr::var(VarLabel::new(0), false);
    let y = SddPtr::var(VarLabel::new(1), true);
    let delta = man.and(x, y);
    let yp = SddPtr::var(VarLabel::new(2), true);
    let inner = man.iff(yp, y);
    println!("(2 <=> 1): \n{}", man.print_sdd(inner));
    println!("(0 && 1): \n{}", man.print_sdd(delta));
    let conj = man.and(inner, delta);
    println!("((!0 && 1) && (2 <=> 1)): \n{}", man.print_sdd(conj));
    let res = man.exists(conj, VarLabel::new(1));

    let expected = man.and(x, yp);
    assert!(
        man.sdd_eq(res, expected),
        "Not eq:\nGot: {}\nExpected: {}",
        man.print_sdd(res),
        man.print_sdd(expected)
    );
}

#[test]
fn sdd_circuit2() {
    // same as circuit1, but with a different variable order
    let man = SddManager::new(VTree::even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        2,
    ));
    let x = SddPtr::var(VarLabel::new(3), false);
    let y = SddPtr::var(VarLabel::new(1), true);
    let delta = man.and(x, y);
    let yp = SddPtr::var(VarLabel::new(4), true);
    let inner = man.iff(yp, y);
    let conj = man.and(inner, delta);
    let res = man.exists(conj, VarLabel::new(1));

    let expected = man.and(x, yp);
    assert!(
        man.sdd_eq(res, expected),
        "Not eq:\nGot: {}\nExpected: {}",
        man.print_sdd(res),
        man.print_sdd(expected)
    );
}

#[test]
fn sdd_wmc1() {
    use crate::util::semiring::RealSemiring;
    // modeling the formula (x<=>fx) && (y<=>fy), with f weight of 0.5

    // let vtree = VTree::right_linear(
    //     &[
    //         VarLabel::new(0),
    //         VarLabel::new(1),
    //         VarLabel::new(2),
    //         VarLabel::new(3),
    //     ],
    // );

    let vtree = VTree::even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
        ],
        1,
    );
    let man = SddManager::new(vtree);
    let mut wmc_map = crate::repr::wmc::WmcParams::new(RealSemiring(0.0), RealSemiring(1.0));
    let x = SddPtr::var(VarLabel::new(0), true);
    wmc_map.set_weight(VarLabel::new(0), RealSemiring(1.0), RealSemiring(1.0));
    let y = SddPtr::var(VarLabel::new(1), true);
    wmc_map.set_weight(VarLabel::new(1), RealSemiring(1.0), RealSemiring(1.0));
    let fx = SddPtr::var(VarLabel::new(2), true);
    wmc_map.set_weight(VarLabel::new(2), RealSemiring(0.5), RealSemiring(0.5));
    let fy = SddPtr::var(VarLabel::new(3), true);
    wmc_map.set_weight(VarLabel::new(3), RealSemiring(0.5), RealSemiring(0.5));
    let x_fx = man.iff(x, fx);
    let y_fy = man.iff(y, fy);
    let ptr = man.and(x_fx, y_fy);
    let wmc_res: RealSemiring = ptr.wmc(man.get_vtree_manager(), &wmc_map);
    let expected = RealSemiring(1.0);
    let diff = (wmc_res - expected).0.abs();
    println!("sdd: {}", man.print_sdd(ptr));
    assert!(
        (diff < 0.0001),
        "Not eq: \n Expected: {:?} \n WMC: {:?}",
        expected,
        wmc_res
    );
}

// #[test]
// fn prob_equiv_sdd_demorgan() {
//     use crate::repr::robdd::create_semantic_hash_map;
//     use crate::repr::robdd::WmcParams;
//     use crate::util::semiring::FiniteField;

//     let mut man = SddManager::<crate::builder::canonicalize::SemanticCanonicalizer<100000049>>::new(
//         VTree::even_split(
//             &[
//                 VarLabel::new(0),
//                 VarLabel::new(1),
//                 VarLabel::new(2),
//                 VarLabel::new(3),
//                 VarLabel::new(4),
//             ],
//             1,
//         ),
//     );
//     man.set_compression(false);
//     let x = SddPtr::var(VarLabel::new(0), true);
//     let y = SddPtr::var(VarLabel::new(3), true);
//     let res = man.or(x, y).neg();
//     let expected = man.and(x.neg(), y.neg());

//     let map: WmcParams<FiniteField<100000049>> = create_semantic_hash_map(man.num_vars());

//     let sh1 = res.cached_semantic_hash(man.get_vtree_manager(), &map);
//     let sh2 = expected.cached_semantic_hash(man.get_vtree_manager(), &map);

//     assert!(sh1 == sh2, "Not eq:\nGot: {:?}\nExpected: {:?}", sh1, sh2);
// }
