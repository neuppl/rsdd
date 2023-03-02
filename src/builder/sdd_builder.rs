//! The main implementation of the SDD manager, the primary way of interacting
//! with SDDs.

use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::Binary;

use rand::seq::SliceRandom;
use rand::thread_rng;

use super::cache::all_app::AllTable;
use super::cache::ite::Ite;
use super::cache::sdd_apply_cache::SddApply;
use super::cache::LruTable;
use crate::backing_store::bump_table::BackedRobinhoodTable;
use crate::backing_store::UniqueTable;
use crate::repr::ddnnf::DDNNFPtr;
use crate::repr::sdd::{BinarySDD, SddAnd, SddOr, SddPtr};
use crate::repr::vtree::{VTree, VTreeIndex, VTreeManager};
use crate::{repr::cnf::Cnf, repr::logical_expr::LogicalExpr, repr::var_label::VarLabel};

#[derive(Debug, Clone)]
pub struct SddStats {
    /// total number of recursive calls
    pub num_rec: usize,
}

impl SddStats {
    pub fn new() -> SddStats {
        SddStats { num_rec: 0 }
    }
}

impl Default for SddStats {
    fn default() -> Self {
        Self::new()
    }
}

pub struct SddManager {
    sdd_tbl: BackedRobinhoodTable<SddOr>,
    bdd_tbl: BackedRobinhoodTable<BinarySDD>,
    vtree: VTreeManager,
    stats: SddStats,
    /// the apply cache
    app_cache: SddApply,
    ite_cache: AllTable<SddPtr>,
    // disabling compression for testing purposes, eventual experimentation
    use_compression: bool,
}

impl SddManager {
    pub fn new(vtree: VTree) -> SddManager {
        SddManager {
            sdd_tbl: BackedRobinhoodTable::new(),
            bdd_tbl: BackedRobinhoodTable::new(),
            stats: SddStats::new(),
            ite_cache: AllTable::new(),
            vtree: VTreeManager::new(vtree),
            app_cache: SddApply::new(),
            use_compression: true,
        }
    }

    pub fn set_compression(&mut self, b: bool) {
        self.use_compression = b
    }

    pub fn get_vtree_root(&self) -> &VTree {
        self.vtree.vtree_root()
    }

    pub fn get_vtree_manager(&self) -> &VTreeManager {
        &self.vtree
    }

    /// Canonicalizes the list of (prime, sub) terms in-place
    /// `node`: a list of (prime, sub) pairs
    fn compress(&mut self, node: &mut Vec<SddAnd>) {
        if !self.use_compression {
            panic!("compress called when disabled")
        }
        for i in 0..node.len() {
            // see if we can compress i
            let mut j = i + 1;
            while j < node.len() {
                if self.sdd_eq(node[i].sub(), node[j].sub()) {
                    // compress j into i and remove j from the node list
                    node[i] = SddAnd::new(self.or(node[i].prime(), node[j].prime()), node[i].sub());
                    node.swap_remove(j);
                } else {
                    j += 1;
                }
            }
        }
    }

    /// Normalizes and fetches a node from the store
    pub fn get_or_insert(&mut self, sdd: SddOr) -> SddPtr {
        let p = self.sdd_tbl.get_or_insert(sdd);
        SddPtr::or(p)
    }

    pub fn get_vtree(&self, ptr: SddPtr) -> &VTree {
        if ptr.is_var() {
            let idx = self.vtree.get_varlabel_idx(ptr.get_var().get_label());
            self.vtree.get_idx(idx)
        } else if ptr.is_or() {
            self.vtree.get_idx(ptr.vtree())
        } else {
            panic!("called vtree on constant")
        }
    }

    pub fn get_vtree_idx(&self, ptr: SddPtr) -> VTreeIndex {
        if ptr.is_var() {
            self.vtree.get_varlabel_idx(ptr.get_var().get_label())
        } else if ptr.is_or() || ptr.is_bdd() {
            ptr.vtree()
        } else {
            panic!("called vtree on constant")
        }
    }

    fn unique_or(&mut self, mut node: Vec<SddAnd>, table: VTreeIndex) -> SddPtr {
        // check if it is a BDD; if it is, return that
        if node.len() == 2 && node[0].prime().is_var() && node[1].prime().is_var() {
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
            if node[0].sub().is_neg() || node[0].sub().is_false() || node[0].sub().is_neg_var() {
                for x in node.iter_mut() {
                    *x = SddAnd::new(x.prime(), x.sub().neg());
                }
                self.get_or_insert(SddOr::new(node, table)).neg()
            } else {
                self.get_or_insert(SddOr::new(node, table))
            }
        }
    }

    fn unique_bdd(&mut self, bdd: BinarySDD) -> SddPtr {
        if bdd.high() == bdd.low() {
            return bdd.high();
        }
        if bdd.high().is_false() && bdd.low().is_true() {
            return SddPtr::var(bdd.label(), false);
        }
        if bdd.high().is_true() && bdd.low().is_false() {
            return SddPtr::var(bdd.label(), true);
        }

        // uniqify BDD
        if bdd.high().is_neg() || bdd.high().is_false() {
            let neg_bdd =
                BinarySDD::new(bdd.label(), bdd.low().neg(), bdd.high().neg(), bdd.vtree());
            SddPtr::bdd(self.bdd_tbl.get_or_insert(neg_bdd)).neg()
        } else {
            SddPtr::bdd(self.bdd_tbl.get_or_insert(bdd))
        }
    }

    /// Returns a canonicalized SDD pointer from a list of (prime, sub) pairs
    fn canonicalize(&mut self, mut node: Vec<SddAnd>, table: VTreeIndex) -> SddPtr {
        // check for base cases before compression
        if node.is_empty() {
            return SddPtr::true_ptr();
        }
        if node.len() == 1 {
            if node[0].prime().is_true() {
                return node[0].sub();
            }
        } else if node.len() == 2 {
            if node[0].sub().is_true() && node[1].sub().is_false() {
                return node[0].prime();
            } else if node[0].sub().is_false() && node[1].sub().is_true() {
                return node[1].prime();
            }
        }

        if self.use_compression {
            // first compress
            self.compress(&mut node);
        }

        // check for a base case after compression (compression can sometimes
        // reduce node counts to a base case)
        if node.is_empty() {
            return SddPtr::true_ptr();
        }
        if node.len() == 1 {
            if node[0].prime().is_true() {
                return node[0].sub();
            } else if node[0].sub().is_false() {
                return SddPtr::false_ptr();
            } else {
                panic!("internal error: encountered untrimmed SDD")
            }
        } else if node.len() == 2 {
            if node[0].sub().is_true() && node[1].sub().is_false() {
                return node[0].prime();
            } else if node[0].sub().is_false() && node[1].sub().is_true() {
                return node[1].prime();
            }
        }
        self.unique_or(node, table)
    }

    /// conjoin two SDDs that are in independent vtrees
    /// a is prime to b
    fn and_indep(&mut self, a: SddPtr, b: SddPtr, lca: VTreeIndex) -> SddPtr {
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
    fn and_sub_desc(&mut self, r: SddPtr, d: SddPtr) -> SddPtr {
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
    fn and_prime_desc(&mut self, r: SddPtr, d: SddPtr) -> SddPtr {
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
            // println!("b: {:?}", b);
            for a2 in b.iter() {
                let p2 = a2.prime();
                let s2 = a2.sub();
                let p = self.and(p1, p2);
                // println!("(PRIME) result of {} && {}: {}", self.print_sdd(p1), self.print_sdd(p2), self.print_sdd(p));
                if p.is_false() {
                    continue;
                }
                let s = self.and(s1, s2);
                // println!("(SUB) result of {} && {}: {}", self.print_sdd(s1), self.print_sdd(s2), self.print_sdd(s));
                // check if one of the nodes is true; if it is, we can
                // return a `true` SddPtr here, for trimming
                if p.is_true() && s.is_true() {
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
        //     if p.is_false() {
        //         continue;
        //     }
        //     v.push(SddAnd::new(p, s));
        // }
        // return self.canonicalize(v, r.vtree());
    }

    /// conjoin SDDs where `a` and `b` are wrt. the same vtree node
    fn and_cartesian(&mut self, a: SddPtr, b: SddPtr, lca: VTreeIndex) -> SddPtr {
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
                if p.is_false() {
                    continue;
                }
                let s = self.and(s1, s2);
                // check if one of the nodes is true; if it is, we can
                // return a `true` SddPtr here, for trimming
                if p.is_true() && s.is_true() {
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

    pub fn and(&mut self, a: SddPtr, b: SddPtr) -> SddPtr {
        // println!("and a: {}\nb: {}", self.print_sdd(a), self.print_sdd(b));
        self.stats.num_rec += 1;
        // first, check for a base case
        match (a, b) {
            (a, b) if a.is_true() => return b,
            (a, b) if b.is_true() => return a,
            (a, _) if a.is_false() => return SddPtr::false_ptr(),
            (_, b) if b.is_false() => return SddPtr::false_ptr(),
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

        let av = self.get_vtree_idx(a);
        let bv = self.get_vtree_idx(b);
        let lca = self.vtree.lca(av, bv);

        // check if we have this application cached
        let c = self.app_cache.get(SddAnd::new(a, b));

        if let Some(x) = c {
            return x;
        }

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
        self.app_cache.insert(SddAnd::new(a, b), r);
        r
    }

    pub fn or(&mut self, a: SddPtr, b: SddPtr) -> SddPtr {
        self.and(a.neg(), b.neg()).neg()
    }

    /// Computes `f | var = value`
    /// TODO: This is highly inefficient, will re-traverse nodes, needs a cache
    pub fn condition(&mut self, f: SddPtr, lbl: VarLabel, value: bool) -> SddPtr {
        self.stats.num_rec += 1;
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
            if newp.is_false() {
                continue;
            };
            let news = self.condition(sub, lbl, value);
            if newp.is_true() {
                return news;
            }
            v.push(SddAnd::new(newp, news));
        }
        self.canonicalize(v, f.vtree())
    }

    /// Computes the SDD representing the logical function `if f then g else h`
    pub fn ite(&mut self, f: SddPtr, g: SddPtr, h: SddPtr) -> SddPtr {
        let ite = Ite::new(|a, b| self.vtree.is_prime(a, b), f, g, h);
        if let Ite::IteConst(f) = ite {
            return f;
        }

        let hash = self.ite_cache.hash(&ite);
        if let Some(v) = self.ite_cache.get(ite, hash) {
            return v;
        }

        // TODO make this a primitive operation
        let fg = self.and(f, g);
        let negfh = self.and(f.neg(), h);
        let r = self.or(fg, negfh);
        self.ite_cache.insert(ite, r, hash);
        r
    }

    /// Computes the SDD representing the logical function `f <=> g`
    pub fn iff(&mut self, f: SddPtr, g: SddPtr) -> SddPtr {
        self.ite(f, g, g.neg())
    }

    /// Computes the SDD representing the logical function `f xor g`
    pub fn xor(&mut self, f: SddPtr, g: SddPtr) -> SddPtr {
        self.ite(f, g.neg(), g)
    }

    /// Existentially quantifies out the variable `lbl` from `f`
    pub fn exists(&mut self, sdd: SddPtr, lbl: VarLabel) -> SddPtr {
        // TODO this can be optimized by specializing it
        let v1 = self.condition(sdd, lbl, true);
        let v2 = self.condition(sdd, lbl, false);
        self.or(v1, v2)
    }

    /// Compose `g` into `f` by substituting for `lbl`
    pub fn compose(&mut self, _f: SddPtr, _lbl: VarLabel, _g: SddPtr) -> SddPtr {
        panic!("not impl")
        // TODO this can be optimized with a specialized implementation to make
        // it a single traversal
        // let var = self.var(lbl, true);
        // let iff = self.iff(var, g);
        // let a = self.and(iff, f);
        // self.exists(a, lbl)
    }

    fn print_sdd_internal(&self, ptr: SddPtr) -> String {
        use pretty::*;
        fn helper(_man: &SddManager, ptr: SddPtr) -> Doc<BoxDoc> {
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
                let l = helper(_man, ptr.low());
                let h = helper(_man, ptr.high());
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
                let new_s1 = helper(_man, prime);
                let new_s2 = helper(_man, s);
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
        let d = helper(self, ptr);
        let mut w = Vec::new();
        d.render(10, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }

    pub fn print_sdd(&self, ptr: SddPtr) -> String {
        self.print_sdd_internal(ptr)
    }

    pub fn sdd_eq(&self, a: SddPtr, b: SddPtr) -> bool {
        a == b
    }

    pub fn is_true(&self, a: SddPtr) -> bool {
        a.is_true()
    }

    pub fn is_false(&self, a: SddPtr) -> bool {
        a.is_false()
    }

    /// compile an SDD from an input CNF
    pub fn from_cnf(&mut self, cnf: &Cnf) -> SddPtr {
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
        fn helper(vec: &[SddPtr], man: &mut SddManager) -> Option<SddPtr> {
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
            None => SddPtr::true_ptr(),
            Some(x) => x,
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

    // Generates a mapping from variables to numbers in [2, PRIME)
    pub fn create_prob_map(&self, prime: u128) -> HashMap<usize, u128> {
        let all_vars = self.get_vtree_root().get_all_vars();
        let vars = all_vars.into_iter().collect::<Vec<usize>>();

        // theoretical guarantee from paper; need to verify more!
        assert!((2 * vars.len() as u128) < prime);

        let mut random_order = vars;
        random_order.shuffle(&mut thread_rng());

        let mut value_range: Vec<u128> = (2..prime / 2).collect();
        value_range.shuffle(&mut thread_rng());

        let mut map = HashMap::<usize, u128>::new();

        for (&var, &value) in random_order.iter().zip(value_range.iter()) {
            map.insert(var, value);
        }

        map
    }

    /// get an iterator over all allocated or-nodes
    fn or_iter(&self) -> impl Iterator<Item = *mut SddOr> + '_ {
        self.sdd_tbl.iter()
    }

    /// get an iterator over all allocated bdd nodes
    fn bdd_iter(&self) -> impl Iterator<Item = *mut BinarySDD> + '_ {
        self.bdd_tbl.iter()
    }

    /// get an iterator over all unique allocated nodes by the manager
    pub fn node_iter(&self) -> impl Iterator<Item = SddPtr> + '_ { 
        let bdditer = self.bdd_iter().map(|x| SddPtr::bdd(x));
        self.or_iter().map(|x| SddPtr::Reg(x)).chain(bdditer)
    }

    pub fn get_stats(&self) -> &SddStats {
        &self.stats
    }

    pub fn print_stats(&self) {
        println!("***************[ SDD Stats ]***************");
        println!("\tNumber of recursive calls: {}", self.stats.num_rec);
    }
}

// check that (a \/ b) /\ a === a
#[test]
fn simple_equality() {
    let mut mgr = SddManager::new(VTree::even_split(
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
    let inner = mgr.or(a, d);
    println!("0 || 3:\n{}", mgr.print_sdd(inner));
    let term = mgr.and(inner, a);
    assert_eq!(a, term);
}

// check that (a \/ b) | !b === a
#[test]
fn sdd_simple_cond() {
    let mut mgr = SddManager::new(VTree::even_split(
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
    let inner = mgr.or(a, d);
    println!("0 || 3: {}", mgr.print_sdd(inner));
    let term = mgr.condition(inner, VarLabel::new(3), false);
    assert_eq!(
        a,
        term,
        "Got:\n{}\nexpected:\n{}\n",
        mgr.print_sdd(term),
        mgr.print_sdd(a)
    );
}

#[test]
fn sdd_test_exist() {
    let mut man = SddManager::new(VTree::even_split(
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
    let mut man = SddManager::new(VTree::right_linear(&[
        VarLabel::new(0),
        VarLabel::new(1),
        VarLabel::new(2),
        VarLabel::new(3),
        VarLabel::new(4),
    ]));

    // let mut man = SddManager::new(VTree::even_split(
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
    let mut man = SddManager::new(VTree::even_split(
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
    let mut man = SddManager::new(VTree::even_split(
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
    let mut man = SddManager::new(VTree::even_split(
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
    let mut man = SddManager::new(VTree::even_split(
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
    let mut man = SddManager::new(vtree);
    let mut wmc_map = crate::repr::wmc::WmcParams::new(0.0, 1.0);
    let x = SddPtr::var(VarLabel::new(0), true);
    wmc_map.set_weight(VarLabel::new(0), 1.0, 1.0);
    let y = SddPtr::var(VarLabel::new(1), true);
    wmc_map.set_weight(VarLabel::new(1), 1.0, 1.0);
    let fx = SddPtr::var(VarLabel::new(2), true);
    wmc_map.set_weight(VarLabel::new(2), 0.5, 0.5);
    let fy = SddPtr::var(VarLabel::new(3), true);
    wmc_map.set_weight(VarLabel::new(3), 0.5, 0.5);
    let x_fx = man.iff(x, fx);
    let y_fy = man.iff(y, fy);
    let ptr = man.and(x_fx, y_fy);
    let wmc_res: f64 = ptr.wmc(man.get_vtree_manager(), &wmc_map);
    let expected: f64 = 1.0;
    let diff = (wmc_res - expected).abs();
    println!("sdd: {}", man.print_sdd(ptr));
    assert!(
        (diff < 0.0001),
        "Not eq: \n Expected: {:?} \n WMC: {:?}",
        expected,
        wmc_res
    );
}

#[test]
fn prob_equiv_sdd_demorgan() {
    let mut man = SddManager::new(VTree::even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        1,
    ));
    man.set_compression(false);
    let x = SddPtr::var(VarLabel::new(0), true);
    let y = SddPtr::var(VarLabel::new(3), true);
    let res = man.or(x, y).neg();
    let expected = man.and(x.neg(), y.neg());

    let prime = 1223; // small enough for this circuit
    let map = man.create_prob_map(prime);

    let sh1 = res.get_semantic_hash(&map, prime);
    let sh2 = expected.get_semantic_hash(&map, prime);

    // TODO: need to express this as pointer eq, not semantic eq
    // assert!(res != expected);
    assert!(sh1 == sh2, "Not eq:\nGot: {}\nExpected: {}", sh1, sh2);
}
