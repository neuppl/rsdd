//! The main implementation of the SDD manager, the primary way of interacting
//! with SDDs.

use backing_store::sdd_table::*;
use manager::cache::lru::*;
use quickersort;
use repr::boolexpr::BoolExpr;
use repr::cnf::Cnf;
use repr::sdd::*;
use repr::var_label::VarLabel;
use std::collections::{HashMap, HashSet};
use util::btree::*;
use std::fmt::Debug;
use num::traits::Num;

use super::rsbdd_manager::{BddManager, BddWmc};


 

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
enum WmcStruct<T: Num + Clone + Debug + Copy> { 
    Bdd(BddWmc<T>),
    Dummy(usize),
} 
pub struct SddWmc<T: Num + Clone + Debug + Copy> { 
   pub zero: T,
   pub one: T,
   // A vector which keeps track of the BddWmc Structs for the component Bdds
   // of an SDD 
   wmc_structs: Vec<WmcStruct<T>>,
}

impl<T: Num + Clone + Debug + Copy> SddWmc<T> { 
    // Set up the store of BddWmc structs given the vtree
    pub fn new(zero: T, one: T, vtree: VTree) -> SddWmc<T> { 
       let mut wmc = SddWmc { 
           zero: zero,
           one: one,
           wmc_structs: Vec::new(),
       }; 

       for v in vtree.in_order_iter() { 
           match v { 
               &BTree::Leaf(..) => wmc.wmc_structs.push(WmcStruct::Bdd(BddWmc::new(zero, one))),
               &BTree::Node(..) => wmc.wmc_structs.push(WmcStruct::Dummy(0)),
           }
       }
       wmc
    }

    // Given an SDD VarLabel, set the weight in the appropriate BddWmc struct
    pub fn set_weight(&mut self, man: &mut SddManager, lbl: VarLabel, low: T, high: T) -> () { 
       let vlbl = man.tbl.sdd_to_bdd.get(&lbl).unwrap().clone(); 
       let idx = man.get_vtree_idx(lbl);
       match &mut self.wmc_structs[idx] { 
           &mut WmcStruct::Bdd(ref mut s) => s.set_weight(vlbl, low, high),
           _ => panic!("Attempted to set weight for non-bdd node"),
       }
    }

}

/// generate an even vtree by splitting a variable ordering in half `num_splits`
/// times
pub fn even_split(order: &[VarLabel], num_splits: usize) -> VTree {
    if num_splits <= 0 {
        BTree::Leaf(order.to_vec())
    } else {
        let (l_s, r_s) = order.split_at(order.len() / 2);
        let l_tree = even_split(l_s, num_splits - 1);
        let r_tree = even_split(r_s, num_splits - 1);
        BTree::Node((), Box::new(l_tree), Box::new(r_tree))
    }
}

pub struct SddManager {
    /// Managers ordered by their order in a depth-first left-first traversal of
    /// the vtree
    tbl: SddTable,
    vtree: VTree,
    stats: SddStats,
    /// a helper structure which matches the vtree and is used for efficient LCA
    /// computation
    parent_ptr: Vec<(Option<usize>, usize)>,
    /// the apply cache
    app_cache: Vec<Lru<(SddPtr, SddPtr), SddPtr>>,
}

/// produces a vector of pointers to vtrees such that (i) the order is given by
/// a depth-first traversal of the vtree; (ii) each element of the vector is a
/// tuple where the first element is the index of parent to the vtree node at
/// that location in the order, and the second is the height of the node. This
/// is used for an efficient implementation of least-common ancestor.
fn into_parent_ptr_vec(vtree: &VTree) -> Vec<(Option<usize>, usize)> {
    fn helper<'a>(
        cur: &'a BTree<usize, usize>,
        level: usize,
        parent: Option<usize>,
    ) -> Vec<(Option<usize>, usize)> {
        match cur {
            &BTree::Leaf(_) => vec![(parent, level)],
            &BTree::Node(ref v, ref l, ref r) => {
                let l = helper(l, level + 1, Some(*v));
                let mut r = helper(r, level + 1, Some(*v));
                let mut v = l;
                v.append(&mut vec![(parent, level)]);
                v.append(&mut r);
                v
            }
        }
    }
    helper(&vtree.into_order_tree(), 0, None)
}

/// find the index of the least common ancestor between `a` and `b`
fn least_common_ancestor(
    parent_vec: &Vec<(Option<usize>, usize)>,
    idx_a: usize,
    idx_b: usize,
) -> usize {
    // base cases
    if idx_a == idx_b {
        return idx_a;
    } else {
    }
    let (a_par, a_h) = parent_vec[idx_a];
    let (b_par, b_h) = parent_vec[idx_b];
    if a_h == 0 {
        return idx_a;
    } else {
    }
    if b_h == 0 {
        return idx_b;
    } else {
    }
    if a_h == b_h {
        least_common_ancestor(parent_vec, a_par.unwrap(), b_par.unwrap())
    } else {
        if a_h > b_h {
            least_common_ancestor(parent_vec, a_par.unwrap(), idx_b)
        } else {
            least_common_ancestor(parent_vec, idx_a, b_par.unwrap())
        }
    }
}

/// true if `idx_a` is prime to `idx_b`
fn is_prime(_: &VTree, idx_a: usize, idx_b: usize) -> bool {
    idx_a < idx_b
}

impl<'a> SddManager {
    pub fn new(vtree: VTree) -> SddManager {
        let mut app_cache = Vec::new();
        for _ in vtree.in_order_iter() {
            app_cache.push(Lru::new(17));
        }

        let m = SddManager {
            tbl: SddTable::new(&vtree),
            stats: SddStats::new(),
            parent_ptr: into_parent_ptr_vec(&vtree),
            vtree: vtree,
            app_cache: app_cache,
        };

        return m;
    }

    // Walks the Sdd, caching results of previously computed values 
    fn unsmoothed_wmc_h<T: Num + Clone + Debug + Copy>(
        &self, 
        ptr: SddPtr, 
        weights: &SddWmc<T>,
        tbl: &mut HashMap<SddPtr, T>
    ) -> T { 
        match tbl.get(&ptr.regular()) { 
            Some(v) => *v,
            None => { 
                if ptr.is_false() { 
                    return weights.zero;
                }
                if ptr.is_true() { 
                    return weights.one;
                }
                if ptr.is_bdd() { 
                    let mgr = self.get_bdd_mgr(ptr);
                    let bdd_ptr = ptr.as_bdd_ptr();
                    let pot_wmc = &weights.wmc_structs[ptr.vtree()]; 
                    let bdd_wmc = match pot_wmc { 
                        WmcStruct::Bdd(wmc) => wmc,
                        WmcStruct::Dummy(_) => panic!("Oh the humanity!"),
                    }; 
                    let wmc_val = mgr.wmc(bdd_ptr, bdd_wmc);
                    tbl.insert(ptr.regular(), wmc_val);
                    return wmc_val;
                }
                self.tbl
                    .sdd_get_or(ptr)
                    .iter()
                    .fold(weights.zero, |acc, (ref p, ref s)| {
                        let p = if ptr.is_compl() { p.neg() } else { *p };
                        let s = if ptr.is_compl() { s.neg() } else { *s }; 
                        acc + 
                            (self.unsmoothed_wmc_h(p, weights, tbl) *
                             self.unsmoothed_wmc_h(s, weights, tbl))})
            }
        }
    }

    pub fn unsmoothed_wmc<T: Num + Clone + Debug + Copy>(
        &mut self, 
        ptr: SddPtr, 
        weights: &SddWmc<T> 
    ) -> T { 
       self.unsmoothed_wmc_h(ptr, weights, &mut HashMap::new()) 
    }


    /// Find the index into self.vtree that contains the label `lbl`
    /// panics if this does not exist.
    fn get_vtree_idx(&self, lbl: VarLabel) -> usize {
        match self.vtree.find_leaf_idx(&|ref l| l.contains(&lbl)) {
            None => panic!("var {:?} not found", lbl),
            Some(a) => a,
        }
    }

    /// Generate a new variable of label `lbl` with truth value `is_true`
    pub fn var(&mut self, lbl: VarLabel, is_true: bool) -> SddPtr {
        // convert the var label into the correct BDD var label
        let vlbl = self.tbl.sdd_to_bdd.get(&lbl).unwrap().clone();
        let idx = self.get_vtree_idx(lbl);
        SddPtr::new_bdd(self.tbl.bdd_man_mut(idx).var(vlbl, is_true), idx as u16)
    }

    /// Logically negate an SddPtr
    pub fn negate(&self, a: SddPtr) -> SddPtr {
        a.neg()
    }

    /// Get a pointer to the vtree for `f`
    /// Equivalent to finding the variable order of a variable in a BDD
    fn get_vtree(&self, f: SddPtr) -> &VTree {
        return self.vtree.in_order_iter().nth(f.vtree()).unwrap();
    }

    /// get the BDD manager for `f`, where `f` is a BDD pointer
    /// panics if `f` is not a BDD pointer
    fn get_bdd_mgr(&self, f: SddPtr) -> &BddManager {
        assert!(f.is_bdd());
        &self.tbl.bdd_man(f.vtree())
    }

    /// Compresses, trims, and canonicalizes the list of (prime, sub) terms and
    /// creates a new canonicalized term
    /// `node`: a list of (prime, sub) pairs
    /// `table`: an index into `self.tbl` to store the result
    fn compress(&mut self, mut node: Vec<(SddPtr, SddPtr)>, table: usize) -> SddPtr {
        // base case
        if node.len() == 0 {
            return SddPtr::new_const(false);
        } else if node.len() == 1 {
            let (p, s) = node[0];
            if p.is_true() {
                // the prime is true, so we can return the sub
                return s;
            } else if s.is_true() {
                // the sub is true, so return the prime
                return p;
            } else {
                assert!(false, "invalid SDD node")
            }
        } else if node.len() == 2 {
            // check for terms of the form [(a, T), (!a, F)] => a
            let (p1, s1) = node[0];
            let (p2, s2) = node[1];
            if s1.is_true() && s2.is_false() {
                return p1;
            } else if s1.is_false() && s2.is_true() {
                return p2;
            }
        } else if node.iter().all(|(ref _p, ref s)| s.is_true()) {
            return SddPtr::new_const(true);
        }

        // this sort guarantees several things:
        // (1) if there is a constant sub (i.e., false), it will be at the *end*
        //     of the returned vector (because we reverse the order by pushing)
        // (2) two equal subs will be adjacent in the ordering, which allows for a
        //     single pass for doing compression
        quickersort::sort_by(&mut node[..], &|a, b| a.1.cmp(&b.1));
        node.dedup();
        // first, check if all the subs are equal; if they are, then return true
        let s0 = node[0].1;
        if node.iter().all(|&(ref _p, ref s)| *s == s0) {
            return s0;
        }

        // to compress, we must disjoin all primes which share a sub
        let mut r = Vec::new();
        let (mut p, mut s) = node[0];
        for i in 1..node.len() {
            let (cur_p, cur_s) = node[i];
            if s == cur_s {
                // disjoin the prime
                p = self.or_internal(p, cur_p);
            } else {
                // found a unique sub, start a new chain
                r.push((p, s));
                p = cur_p;
                s = cur_s;
            }
        }
        r.push((p, s));
        // now all the subs are unique, sort by primes now to guarantee
        // canonicity
        // TODO: combine these into a single initial sort
        quickersort::sort_by(&mut r[..], &|a, b| a.0.cmp(&b.0));

        if r.len() == 1 {
            let (p, s) = r[0];
            if p.is_true() {
                // the prime is true, so we can return the sub
                return s;
            } else if s.is_true() {
                // the sub is true, so return the prime
                return p;
            }
        }
        if r.len() == 2 {
            // again check for terms of the form [(a, T), (!a, F)] => a
            let (p1, s1) = r[0];
            let (p2, s2) = r[1];
            if s1.is_true() && s2.is_false() {
                return p1;
            } else if s1.is_false() && s2.is_true() {
                return p2;
            }
        }
        if r.iter().all(|(ref _p, ref s)| s.is_true()) {
            return SddPtr::new_const(true);
        }
        if r.iter().all(|(ref _p, ref s)| s.is_false()) {
            return SddPtr::new_const(false);
        }
        let res = if r[0].1.is_compl() {
            // guarantee first sub in the first node is not complemented
            // (regular form)
            // TODO looks like an unnecessary allocation here
            let compl_r = r.iter().map(|&(ref p, ref s)| (*p, s.neg())).collect();
            self.tbl
                .get_or_insert_sdd(&SddOr { nodes: compl_r }, table)
                .neg()
        } else {
            self.tbl.get_or_insert_sdd(&SddOr { nodes: r }, table)
        };
        // println!("result: {}\n", self.print_sdd(res));
        res
    }

    pub fn or_internal(&mut self, a: SddPtr, b: SddPtr) -> SddPtr {
        self.and_rec(a.neg(), b.neg()).neg()
    }

    fn and_rec(&mut self, a: SddPtr, b: SddPtr) -> SddPtr {
        self.stats.num_rec += 1;
        // first, check for a base case
        match (a, b) {
            (a, b) if a.is_true() => return b,
            (a, b) if b.is_true() => return a,
            (a, _) if a.is_false() => return SddPtr::new_const(false),
            (_, b) if b.is_false() => return SddPtr::new_const(false),
            (a, b) if self.sdd_eq(a, b) => return a,
            (a, b) if self.sdd_eq(a, b.neg()) => return SddPtr::new_const(false),
            _ => (),
        };

        // normalize the pointers to increase cache hit rate
        let (a, b) = if a < b { (a, b) } else { (b, a) };
        // check if both are BDDs; if they are, just invoke their apply
        // functions
        if a.is_bdd() && b.is_bdd() && a.vtree() == b.vtree() {
            // both nodes are BDDs, so simply apply them together
            // and return the result
            let a_bdd = a.as_bdd_ptr();
            let b_bdd = b.as_bdd_ptr();
            let r = self.tbl.bdd_man_mut(a.vtree()).and(a_bdd, b_bdd);
            return if r.is_false() {
                SddPtr::new_const(false)
            } else if r.is_true() {
                SddPtr::new_const(true)
            } else {
                SddPtr::new_bdd(r, a.vtree() as u16)
            };
        }

        // normalize so `a` is always prime if possible
        let (a, b) = if a.vtree() == b.vtree() || is_prime(&self.vtree, a.vtree(), b.vtree()) {
            (a, b)
        } else {
            (b, a)
        };

        let av = a.vtree();
        let bv = b.vtree();
        let lca = least_common_ancestor(&self.parent_ptr, av, bv);

        // check if we have this application cached
        let c = self.app_cache[lca].get((a, b));
        if c.is_some() {
            return c.unwrap();
        }

        // now we determine the current iterator for primes and subs
        // 4 cases:
        //   1. `a` and `b` have the same vtree
        //   2. The lca is the prime `a`
        //   3. The lca is the sub `b`
        //   4. The lca is a shared parent equal to neither
        // these pointers are necessary to ensure that the vectors live
        // long enough
        let (outer_v, inner_v) = if av == bv {
            let outer = self.tbl.sdd_get_or(a).to_vec();
            let inner = self.tbl.sdd_get_or(b).to_vec();
            (outer, inner)
        } else if lca == av {
            let outer = self.tbl.sdd_get_or(a).to_vec();
            let inner_v = vec![(SddPtr::new_const(true), b.regular())];
            (outer, inner_v)
        } else if lca == bv {
            // the sub must always be true
            let v = if a.is_compl() {
                SddPtr::new_const(false)
            } else {
                SddPtr::new_const(true)
            };
            let outer_v = vec![(a, v), (a.neg(), v.neg())];
            let inner = self.tbl.sdd_get_or(b).to_vec();
            (outer_v, inner)
        } else {
            // avoid double-negations by checking to see if `a` is already
            // negated
            let v = if a.is_compl() {
                SddPtr::new_const(false)
            } else {
                SddPtr::new_const(true)
            };
            let outer_v = vec![(a, v), (a.neg(), v.neg())];
            let inner_v = vec![(SddPtr::new_const(true), b.regular())];
            (outer_v, inner_v)
        };

        // iterate over each prime/sum pair and do the relevant application
        // there are 2 simplifying cases:
        // (1) if p1i = p2j or if p1i => p2j, then p1k x p2j = false for all k<>i
        // (2) if p1i = !p2j, then p1k x p2j = p1k for all k<>i
        let mut r: Vec<(SddPtr, SddPtr)> = Vec::new();
        for &(ref p1, ref s1) in outer_v.iter() {
            // check if there exists an equal prime
            let eq_itm = inner_v
                .iter()
                .find(|(ref p2, ref _s2)| self.sdd_eq(*p1, *p2));
            let s1 = if a.is_compl() { s1.neg() } else { *s1 };
            if eq_itm.is_some() {
                let (_, s2) = eq_itm.unwrap();
                let s2 = if b.is_compl() { s2.neg() } else { *s2 };
                // this sub is the only one with a non-false prime, so no need to iterate
                r.push((*p1, self.and_rec(s1, s2)));
                continue;
            }

            // check for a negated equal prime
            // let eq_neg = inner_v.iter().find(|(ref p2, ref _s2)| self.sdd_eq(*p1, p2.neg()));
            // if eq_neg.is_some() {
            //     // iterate but skip prime conjunction
            //     for &(ref p2, ref s2) in inner_v.iter() {
            //         if self.sdd_eq(*p1, p2.neg()) {
            //             // skip this one; we know these primes are false when conjoined
            //             continue;
            //         }
            //         let s2 = if b.is_compl() { s2.neg() } else { *s2 };
            //         let s = self.and_rec(s1, s2);
            //         r.push((*p1, s));
            //     }
            //     continue;
            // }

            // no special case
            for &(ref p2, ref s2) in inner_v.iter() {
                let s2 = if b.is_compl() { s2.neg() } else { *s2 };
                let p = self.and_rec(*p1, *p2);
                if p.is_false() {
                    continue;
                }
                let s = self.and_rec(s1, s2);
                // check if one of the nodes is true; if it is, we can
                // return a `true` SddPtr here, for trimming
                if p.is_true() && s.is_true() {
                    let new_v = SddPtr::new_const(true);
                    self.app_cache[lca].insert((a, b), new_v.clone());
                    return new_v;
                }
                r.push((p, s));
                // check if p1 => p2 (which is true iff (p1 && p2) == p1); if it
                // does we can stop early because the rest of the primes will be
                // false
                if self.sdd_eq(*p1, p) {
                    break;
                }
            }
        }

        // canonicalize
        let ptr = self.compress(r, lca);
        self.app_cache[lca].insert((a, b), ptr);
        ptr
    }

    pub fn and(&mut self, a: SddPtr, b: SddPtr) -> SddPtr {
        self.and_rec(a, b)
    }

    pub fn or(&mut self, a: SddPtr, b: SddPtr) -> SddPtr {
        self.or_internal(a, b)
    }

    /// Computes `f | var = value`
    pub fn condition(&mut self, f: SddPtr, lbl: VarLabel, value: bool) -> SddPtr {
        self.stats.num_rec += 1;
        // TODO : this can bail out early by checking the vtree
        // check base case
        if f.is_const() {
            return f;
        } else if f.is_bdd() {
            // check if this BDD contains the label
            if self.get_vtree(f).extract_leaf().contains(&lbl) {
                // it does; condition and return
                let mapped = self.tbl.sdd_to_bdd_label(&lbl).clone();
                let bdd = self
                    .tbl
                    .bdd_man_mut(f.vtree())
                    .condition(f.as_bdd_ptr(), mapped, value);
                return SddPtr::new_bdd(bdd, f.vtree() as u16);
            } else {
                // do nothing
                return f;
            }
        };

        let mut v = Vec::new();
        // f is a node; recurse and compress the result
        for (prime, sub) in self.tbl.sdd_get_or(f).to_vec().iter() {
            let newp = self.condition(*prime, lbl, value);
            let news = self.condition(*sub, lbl, value);
            v.push((newp, news));
        }
        let r = self.compress(v, f.vtree());
        if f.is_compl() {
            return r.neg();
        } else {
            return r;
        }
    }

    /// Computes the SDD representing the logical function `if f then g else h`
    pub fn ite(&mut self, f: SddPtr, g: SddPtr, h: SddPtr) -> SddPtr {
        // TODO make this a primitive operation
        let fg = self.and(f, g);
        let negfh = self.and(f.neg(), h);
        self.or(fg, negfh)
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
    pub fn compose(&mut self, f: SddPtr, lbl: VarLabel, g: SddPtr) -> SddPtr {
        // TODO this can be optimized with a specialized implementation to make
        // it a single traversal
        let var = self.var(lbl, true);
        let iff = self.iff(var, g);
        let a = self.and(iff, f);
        self.exists(a, lbl)
    }

    fn count_nodes_h(&self, f: SddPtr, sddcache: &mut HashSet<SddPtr>) -> u64 {
        if sddcache.contains(&f.regular()) {
            return 0;
        }
        sddcache.insert(f.regular());
        if f.is_const() {
            return 1;
        }
        if f.is_bdd() {
            // traverse down the BDD
            let mgr = self.get_bdd_mgr(f);
            let fbdd = f.as_bdd_ptr();
            let l = SddPtr::new_bdd(mgr.low(fbdd), f.vtree() as u16);
            let h = SddPtr::new_bdd(mgr.high(fbdd), f.vtree() as u16);
            return 1 + self.count_nodes_h(l, sddcache) + self.count_nodes_h(h, sddcache);
        };
        // it's an SDD node; iterate for each node
        self.tbl
            .sdd_get_or(f)
            .iter()
            .fold(0, |acc, (ref p, ref s)| {
                acc + 1 + self.count_nodes_h(*p, sddcache) + self.count_nodes_h(*s, sddcache)
            })
    }

    /// Counts the number of unique nodes in `f`
    pub fn count_nodes(&self, f: SddPtr) -> u64 {
        self.count_nodes_h(f, &mut HashSet::new())
    }

    fn print_sdd_internal(&self, ptr: SddPtr) -> String {
        use pretty::*;
        fn helper(man: &SddManager, ptr: SddPtr) -> Doc<BoxDoc> {
            if ptr.is_bdd() {
                let bdd_ptr = ptr.as_bdd_ptr();
                let m = man.tbl.bdd_conv(ptr.vtree());
                let s = man.tbl.bdd_man(ptr.vtree()).print_bdd_lbl(bdd_ptr, m);
                Doc::from(s)
            } else {
                if ptr.is_true() {
                    return Doc::from("T");
                } else if ptr.is_false() {
                    return Doc::from("F");
                }

                let mut doc: Doc<BoxDoc> = Doc::from("");
                let sl = man.tbl.sdd_get_or(ptr);
                for &(ref prime, ref sub) in sl.iter() {
                    let new_s1 = helper(man, prime.clone());
                    let new_s2 = helper(man, sub.clone());
                    doc = doc.append(Doc::newline()).append(
                        (Doc::from("/\\")
                            .append(Doc::newline())
                            .append(new_s1.append(Doc::newline()).append(new_s2)))
                        .nest(2),
                    );
                }
                let d = Doc::from(String::from(format!(
                    "{}\\/",
                    if ptr.is_compl() { "!" } else { "" }
                )));
                d.append(doc.nest(2))
            }
        }
        let d = helper(self, ptr);
        let mut w = Vec::new();
        d.render(10, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }

    pub fn print_sdd(&self, ptr: SddPtr) -> String {
        self.print_sdd_internal(ptr)
    }

    /// Evaluate an SDD on a set of input Boolean variable values
    /// TODO: This is *highly* inefficient, fix it
    pub fn eval_sdd(&self, ptr: SddPtr, assgn: &HashMap<VarLabel, bool>) -> bool {
        fn helper(man: &SddManager, sdd: SddPtr, assgn: &HashMap<VarLabel, bool>) -> bool {
            if sdd.is_false() {
                false
            } else if sdd.is_true() {
                true
            } else if sdd.is_bdd() {
                let mut labels: HashSet<VarLabel> = HashSet::new();
                for lbl in man.tbl.bdd_conv(sdd.vtree()).values() {
                    labels.insert(lbl.clone());
                }
                let mut new_m: HashMap<VarLabel, bool> = HashMap::new();
                for (key, value) in assgn.iter() {
                    if labels.contains(key) {
                        let translated = man.tbl.sdd_to_bdd.get(key).unwrap();
                        new_m.insert(*translated, *value);
                    }
                }
                let bdd_ptr = sdd.as_bdd_ptr();
                man.tbl.bdd_man(sdd.vtree()).eval_bdd(bdd_ptr, &new_m)
            } else {
                let mut res = false;
                let sl = man.tbl.sdd_get_or(sdd);
                for &(ref p, ref s) in sl.iter() {
                    let v1 = helper(man, *p, assgn);
                    let v2 = helper(man, *s, assgn);
                    res = res || (v1 && v2)
                }
                if sdd.is_compl() {
                    !res
                } else {
                    res
                }
            }
        }
        helper(self, ptr, assgn)
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

    pub fn from_cnf(&mut self, cnf: &Cnf) -> SddPtr {
        let mut cvec: Vec<SddPtr> = Vec::with_capacity(cnf.clauses().len());
        for lit_vec in cnf.clauses().iter() {
            assert!(lit_vec.len() > 0, "empty cnf");
            let (vlabel, val) = (lit_vec[0].get_label(), lit_vec[0].get_polarity());
            let mut sdd = self.var(vlabel, val);
            for i in 1..lit_vec.len() {
                let (vlabel, val) = (lit_vec[i].get_label(), lit_vec[i].get_polarity());
                let var = self.var(vlabel, val);
                sdd = self.or(sdd, var);
            }
            cvec.push(sdd);
        }
        // now cvec has a list of all the clauses; collapse it down
        fn helper(vec: &[SddPtr], man: &mut SddManager) -> Option<SddPtr> {
            if vec.len() == 0 {
                None
            } else if vec.len() == 1 {
                return Some(vec[0]);
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
        helper(&cvec, self).unwrap()
    }

    pub fn from_boolexpr(&mut self, expr: &BoolExpr) -> SddPtr {
        match expr {
            &BoolExpr::Var(lbl, polarity) => self.var(VarLabel::new(lbl as u64), polarity),
            &BoolExpr::And(ref l, ref r) => {
                let r1 = self.from_boolexpr(l);
                let r2 = self.from_boolexpr(r);
                self.and(r1, r2)
            }
            &BoolExpr::Or(ref l, ref r) => {
                let r1 = self.from_boolexpr(l);
                let r2 = self.from_boolexpr(r);
                self.or(r1, r2)
            }
        }
    }

    pub fn get_stats(&self) -> &SddStats {
        &self.stats
    }

    pub fn print_stats(&self) -> () {
        println!("***************[ SDD Stats ]***************");
        println!("\tNumber of recursive calls: {}", self.stats.num_rec);
    }
}

#[test]
fn test_lca() {
    let simple_vtree = BTree::Node(
        (),
        Box::new(BTree::Leaf(vec![VarLabel::new(0), VarLabel::new(1)])),
        Box::new(BTree::Leaf(vec![VarLabel::new(2), VarLabel::new(3)])),
    );
    //    3
    // 1     5
    //0 2   4 6
    let simple_vtree2 = BTree::Node(
        (),
        Box::new(simple_vtree.clone()),
        Box::new(simple_vtree.clone()),
    );
    let par_vec = into_parent_ptr_vec(&simple_vtree2);
    assert_eq!(least_common_ancestor(&par_vec, 2, 3), 3);
    assert_eq!(least_common_ancestor(&par_vec, 2, 5), 3);
    assert_eq!(least_common_ancestor(&par_vec, 1, 4), 3);
    assert_eq!(least_common_ancestor(&par_vec, 2, 1), 1);
    assert_eq!(least_common_ancestor(&par_vec, 4, 6), 5);
}

// check that (a \/ b) /\ a === a
#[test]
fn simple_equality() {
    let mut mgr = SddManager::new(even_split(
        &vec![
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        2,
    ));
    let a = mgr.var(VarLabel::new(0), true);
    let d = mgr.var(VarLabel::new(3), true);
    let inner = mgr.or(a, d);
    let term = mgr.and(inner, a);
    assert_eq!(a, term);
}

// check that (a \/ b) | !b === a
#[test]
fn sdd_simple_cond() {
    let mut mgr = SddManager::new(even_split(
        &vec![
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        2,
    ));
    let a = mgr.var(VarLabel::new(0), true);
    let d = mgr.var(VarLabel::new(3), true);
    let inner = mgr.or(a, d);
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
    let mut man = SddManager::new(even_split(
        &vec![
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        2,
    ));
    // 1 /\ 2 /\ 3
    let v1 = man.var(VarLabel::new(0), true);
    let v2 = man.var(VarLabel::new(1), true);
    let v3 = man.var(VarLabel::new(2), true);
    let a1 = man.and(v1, v2);
    let r1 = man.and(a1, v3);
    let r_expected = man.and(v1, v3);
    let res = man.exists(r1, VarLabel::new(1));
    assert!(
        man.sdd_eq(r_expected, res),
        "Got:\nOne: {}\nExpected: {}",
        man.print_sdd(res),
        man.print_sdd(r_expected)
    );
}

#[test]
fn sdd_ite1() {
    let mut man = SddManager::new(even_split(
        &vec![
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        2,
    ));
    let v1 = man.var(VarLabel::new(0), true);
    let v2 = man.var(VarLabel::new(1), true);
    let r1 = man.or(v1, v2);
    let r2 = man.ite(r1, v1, SddPtr::new_const(false));
    assert!(
        man.sdd_eq(v1, r2),
        "Not eq:\n {}\n{}",
        man.print_sdd(v1),
        man.print_sdd(r2)
    );
}

#[test]
fn sdd_circuit1() {
    let mut man = SddManager::new(even_split(
        &vec![
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        2,
    ));
    let x = man.var(VarLabel::new(0), false);
    let y = man.var(VarLabel::new(1), true);
    let delta = man.and(x, y);
    let yp = man.var(VarLabel::new(2), true);
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
fn sdd_circuit2() {
    // same as circuit1, but with a different variable order
    let mut man = SddManager::new(even_split(
        &vec![
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        2,
    ));
    let x = man.var(VarLabel::new(3), false);
    let y = man.var(VarLabel::new(1), true);
    let delta = man.and(x, y);
    let yp = man.var(VarLabel::new(4), true);
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

    let vtree = even_split(&vec![
                    VarLabel::new(0),
                    VarLabel::new(1),
                    VarLabel::new(2),
                    VarLabel::new(3),
                ],
                2,);    
    let mut man = SddManager::new(vtree.clone());
    let mut wmc_map = SddWmc::new(0.0, 1.0, vtree.clone());
    let x = man.var(VarLabel::new(0), true);
    wmc_map.set_weight(&mut man, VarLabel::new(0), 1.0, 1.0);
    let y = man.var(VarLabel::new(1), true);
    wmc_map.set_weight(&mut man, VarLabel::new(1), 1.0, 1.0);
    let fx = man.var(VarLabel::new(2), true);
    wmc_map.set_weight(&mut man, VarLabel::new(2), 0.5, 0.5);
    let fy = man.var(VarLabel::new(3), true);
    wmc_map.set_weight(&mut man, VarLabel::new(3), 0.5, 0.5);
    let x_fx = man.iff(x, fx);
    let y_fy = man.iff(y, fy);
    let ptr = man.and(x_fx, y_fy);
    let wmc_res: f64 = man.unsmoothed_wmc(ptr, &wmc_map);
    let expected: f64 = 1.0;
    let diff = (wmc_res - expected).abs(); 
    assert!( 
        (diff < 0.0001), 
        "Not eq: \n Diff: {:?} \n WMC: {:?}",
        diff, 
        wmc_res
    );
}
