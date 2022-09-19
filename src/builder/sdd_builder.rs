//! The main implementation of the SDD manager, the primary way of interacting
//! with SDDs.

use crate::{
    backing_store::sdd_table::*, builder::cache::lru::*, builder::repr::builder_sdd::*,
    repr::cnf::Cnf, repr::logical_expr::LogicalExpr, repr::var_label::VarLabel, util::btree::*,
};

use num::traits::Num;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;

use super::bdd_builder::{BddManager, BddWmc};

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
            zero,
            one,
            wmc_structs: Vec::new(),
        };

        for v in vtree.dfs_iter() {
            match v {
                &BTree::Leaf(..) => wmc.wmc_structs.push(WmcStruct::Bdd(BddWmc::new(zero, one))),
                &BTree::Node(..) => wmc.wmc_structs.push(WmcStruct::Dummy(0)),
            }
        }
        wmc
    }

    // Given an SDD VarLabel, set the weight in the appropriate BddWmc struct
    pub fn set_weight(&mut self, man: &mut SddManager, lbl: VarLabel, low: T, high: T) {
        let vlbl = man.tbl.sdd_to_bdd_label(&lbl);
        let idx = man.get_vtree_idx(lbl);
        match &mut self.wmc_structs[idx.value()] {
            &mut WmcStruct::Bdd(ref mut s) => s.set_weight(*vlbl, low, high),
            _ => panic!("Attempted to set weight for non-bdd node"),
        }
    }

    pub fn new_with_default(
        zero: T,
        one: T,
        mgr: &mut SddManager,
        tbl: &HashMap<VarLabel, (T, T)>,
    ) -> SddWmc<T> {
        // TODO add manager num_vars so this assertion can be implemented
        // assert!(tbl.len() == mgr.tbl.num_vars);
        let mut n = SddWmc::new(zero, one, mgr.get_vtree_root().clone());
        for (k, (low, high)) in tbl.iter() {
            n.set_weight(mgr, *k, *low, *high);
        }
        n
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
    vtree: VTreeManager,
    stats: SddStats,
    /// the apply cache
    app_cache: Vec<Lru<(SddPtr, SddPtr), SddPtr>>,
    // disabling compression for testing purposes, eventual experimentation
    use_compression: bool,
}

impl<'a> SddManager {
    pub fn new(vtree: VTree) -> SddManager {
        let mut app_cache = Vec::new();
        for _ in vtree.dfs_iter() {
            app_cache.push(Lru::new(8));
        }

        SddManager {
            tbl: SddTable::new(&vtree),
            stats: SddStats::new(),
            vtree: VTreeManager::new(vtree),
            app_cache,
            use_compression: true,
        }
    }

    pub fn set_compression(&mut self, b: bool) {
        self.use_compression = b
    }

    // Walks the Sdd, caching results of previously computed values
    fn unsmoothed_wmc_h<T: Num + Clone + Debug + Copy>(
        &mut self,
        ptr: SddPtr,
        weights: &SddWmc<T>,
        tbl: &mut HashMap<SddPtr, T>,
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
                    let mgr = self.get_bdd_mgr_mut(ptr);
                    let bdd_ptr = ptr.as_bdd_ptr();
                    let pot_wmc = &weights.wmc_structs[ptr.vtree().value()];
                    let bdd_wmc = match pot_wmc {
                        WmcStruct::Bdd(wmc) => wmc,
                        WmcStruct::Dummy(_) => panic!("Oh the humanity!"),
                    };
                    let wmc_val = mgr.wmc(bdd_ptr, bdd_wmc);
                    tbl.insert(ptr.regular(), wmc_val);
                    return wmc_val;
                }
                // TODO remove this allocation
                let or = Vec::from(self.tbl.sdd_get_or(ptr));
                or.iter().fold(weights.zero, |acc, (p, s)| {
                    let s = if ptr.is_compl() { s.neg() } else { *s };
                    acc + (self.unsmoothed_wmc_h(*p, weights, tbl)
                        * self.unsmoothed_wmc_h(s, weights, tbl))
                })
            }
        }
    }

    pub fn unsmoothed_wmc<T: Num + Clone + Debug + Copy>(
        &mut self,
        ptr: SddPtr,
        weights: &SddWmc<T>,
    ) -> T {
        self.unsmoothed_wmc_h(ptr, weights, &mut HashMap::new())
    }

    /// Find the index into self.vtree that contains the label `lbl`
    /// panics if this does not exist.
    fn get_vtree_idx(&self, lbl: VarLabel) -> VTreeIndex {
        self.vtree.get_varlabel_idx(lbl)
    }

    /// Generate a new variable of label `lbl` with truth value `is_true`
    pub fn var(&mut self, lbl: VarLabel, is_true: bool) -> SddPtr {
        // convert the var label into the correct BDD var label
        let vlbl = *self.tbl.sdd_to_bdd.get(&lbl).unwrap();
        let idx = self.get_vtree_idx(lbl).value();
        SddPtr::new_bdd(self.tbl.bdd_man_mut(idx).var(vlbl, is_true), idx as u16)
    }

    /// Logically negate an SddPtr
    pub fn negate(&self, a: SddPtr) -> SddPtr {
        a.neg()
    }

    /// Get a pointer to the vtree for `f`
    /// Equivalent to finding the variable order of a variable in a BDD
    fn get_vtree(&self, f: SddPtr) -> &VTree {
        self.vtree.get_idx(f.vtree())
    }

    pub fn get_vtree_root(&self) -> &VTree {
        self.vtree.vtree_root()
    }

    /// get the BDD manager for `f`, where `f` is a BDD pointer
    /// panics if `f` is not a BDD pointer
    fn get_bdd_mgr(&self, f: SddPtr) -> &BddManager {
        assert!(f.is_bdd());
        self.tbl.bdd_man(f.vtree().value())
    }

    /// get the BDD manager for `f`, where `f` is a BDD pointer
    /// panics if `f` is not a BDD pointer
    fn get_bdd_mgr_mut(&mut self, f: SddPtr) -> &mut BddManager {
        assert!(f.is_bdd());
        self.tbl.bdd_man_mut(f.vtree().value())
    }

    /// Canonicalizes the list of (prime, sub) terms in-place
    /// `node`: a list of (prime, sub) pairs
    fn compress(&mut self, node: &mut Vec<(SddPtr, SddPtr)>) {
        if !self.use_compression {
            panic!("compress called when disabled")
        }
        for i in 0..node.len() {
            // see if we can compress i
            let mut j = i + 1;
            while j < node.len() {
                if self.sdd_eq(node[i].1, node[j].1) {
                    // compress j into i and remove j from the node list
                    node[i].0 = self.or(node[i].0, node[j].0);
                    node.swap_remove(j);
                } else {
                    j += 1;
                }
            }
        }
    }

    /// Returns a canonicalized SDD pointer from a list of (prime, sub) pairs
    fn canonicalize(&mut self, mut node: Vec<(SddPtr, SddPtr)>, table: VTreeIndex) -> SddPtr {
        if self.use_compression {
            // first compress
            self.compress(&mut node);
        }
        // check for a base case
        if node.is_empty() {
            return SddPtr::new_const(true);
        }
        if node.len() == 1 {
            if node[0].0.is_true() {
                return node[0].1;
            }
        } else if node.len() == 2 {
            if node[0].1.is_true() && node[1].1.is_false() {
                return node[0].0;
            } else if node[0].1.is_false() && node[1].1.is_true() {
                return node[1].0;
            }
        }

        // we have a fresh SDD pointer, uniqify it
        node.sort_by(|a, b| a.0.cmp(&b.0));

        if node[0].1.is_compl() {
            for i in 0..node.len() {
                node[i].1 = node[i].1.neg();
            }
            self.tbl
                .get_or_insert_sdd(&SddOr { nodes: node }, table.value())
                .neg()
        } else {
            self.tbl
                .get_or_insert_sdd(&SddOr { nodes: node }, table.value())
        }
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

        // check if both are BDDs; if they are, just invoke their apply
        // functions
        if a.is_bdd() && b.is_bdd() && a.vtree() == b.vtree() {
            // both nodes are BDDs, so simply apply them together
            // and return the result
            let a_bdd = a.as_bdd_ptr();
            let b_bdd = b.as_bdd_ptr();
            let r = self.tbl.bdd_man_mut(a.vtree().value()).and(a_bdd, b_bdd);
            return if r.is_false() {
                SddPtr::new_const(false)
            } else if r.is_true() {
                SddPtr::new_const(true)
            } else {
                SddPtr::new_bdd(r, a.vtree().value() as u16)
            };
        }

        // normalize so `a` is always prime if possible
        let (a, b) = if a.vtree() == b.vtree() || self.vtree.is_prime(a.vtree(), b.vtree()) {
            (a, b)
        } else {
            (b, a)
        };

        let av = a.vtree();
        let bv = b.vtree();
        let lca = self.vtree.lca(av, bv);

        // check if we have this application cached
        let c = self.app_cache[lca.value()].get((a, b));
        if c.is_some() {
            return c.unwrap();
        }

        // now we determine the current iterator for primes and subs
        // 4 cases:
        //   1. `a` and `b` have the same vtree
        //   2. The lca is the prime `a`
        //   3. The lca is the sub `b`
        //   4. The lca is a shared parent equal to neither
        // TODO this to_vec is probably very bad for perf
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

        // iterate over each prime/sub pair and do the relevant application
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
                    self.app_cache[lca.value()].insert((a, b), new_v);
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
        let ptr = self.canonicalize(r, lca);
        self.app_cache[lca.value()].insert((a, b), ptr);
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
                let mapped = *self.tbl.sdd_to_bdd_label(&lbl);
                let bdd = self.tbl.bdd_man_mut(f.vtree().value()).condition(
                    f.as_bdd_ptr(),
                    mapped,
                    value,
                );
                return SddPtr::new_bdd(bdd, f.vtree().value() as u16);
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
        let r = self.canonicalize(v, f.vtree());
        if f.is_compl() {
            r.neg()
        } else {
            r
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
            let l = SddPtr::new_bdd(mgr.low(fbdd), f.vtree().value() as u16);
            let h = SddPtr::new_bdd(mgr.high(fbdd), f.vtree().value() as u16);
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

    fn is_canonical_h(&self, f: SddPtr) -> bool {
        self.is_compressed(f) && self.is_trimmed(f)
    }

    pub fn is_canonical(&self, f: SddPtr) -> bool {
        self.is_canonical_h(f)
    }

    // predicate that returns if an SDD is compressed;
    // see https://www.ijcai.org/Proceedings/11/Papers/143.pdf
    // definition 8
    pub fn is_compressed(&self, f: SddPtr) -> bool {
        self.is_compressed_h(f)
    }

    fn is_compressed_h(&self, f: SddPtr) -> bool {
        if f.is_const() {
            return true;
        }

        // TODO: is this assumption correct?
        if f.is_bdd() {
            return true;
        }

        // question for matt: should we be recursively passing this down?
        let mut visited_sdds: HashSet<SddPtr> = HashSet::new();

        let or = self.tbl.sdd_get_or(f);

        for (_, s) in or.iter() {
            if visited_sdds.contains(&s.regular()) {
                return false;
            }
            visited_sdds.insert(s.regular());
        }

        return or.iter().all(|(p, _)| self.is_compressed(*p));
    }

    // predicate that returns if an SDD is trimmed;
    // see https://www.ijcai.org/Proceedings/11/Papers/143.pdf
    // definition 8
    pub fn is_trimmed(&self, f: SddPtr) -> bool {
        self.is_trimmed_h(f)
    }

    fn is_trimmed_h(&self, f: SddPtr) -> bool {
        if f.is_const() {
            return true;
        }

        // assumption: bdd is trimmed!
        if f.is_bdd() {
            return true;
        }

        let or = self.tbl.sdd_get_or(f);

        // this is a linear search for decompositions of the form (T, a)
        let trivial_p_exists = or.iter().all(|(p, _)| p.is_true());

        if trivial_p_exists {
            return false;
        }

        // TODO(mattxwang): significantly optimize this
        // this next part is an O(n^2) (i.e., pairwise) comparison of each SDD
        // and an arbitrary prime. we are looking for untrimmed decomposition pairs of the form (a, T) and (~a, F)
        let mut visited_sdds: Vec<SddPtr> = Vec::new();

        for (p, s) in or.iter() {
            if !s.is_const() {
                continue;
            }
            let p_neg = p.neg();
            let neg_exists = visited_sdds
                .iter()
                .any(|visited_p| self.sdd_eq(*visited_p, p_neg));

            if neg_exists {
                return false;
            }
            visited_sdds.push(*p);
        }

        // neither trimmed form exists at the top-level, so we recurse down once
        return or.iter().all(|(p, _)| self.is_trimmed(*p));
    }

    fn print_sdd_internal(&self, ptr: SddPtr) -> String {
        use pretty::*;
        fn helper(man: &SddManager, ptr: SddPtr) -> Doc<BoxDoc> {
            if ptr.is_bdd() {
                let bdd_ptr = ptr.as_bdd_ptr();
                let m = man.tbl.bdd_conv(ptr.vtree().value());
                let s = man
                    .tbl
                    .bdd_man(ptr.vtree().value())
                    .print_bdd_lbl(bdd_ptr, m);
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
                    let new_s1 = helper(man, *prime);
                    let new_s2 = helper(man, *sub);
                    doc = doc.append(Doc::newline()).append(
                        (Doc::from("/\\")
                            .append(Doc::newline())
                            .append(new_s1.append(Doc::newline()).append(new_s2)))
                        .nest(2),
                    );
                }
                let d = Doc::from(format!("{}\\/", if ptr.is_compl() { "!" } else { "" }));
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
                for lbl in man.tbl.bdd_conv(sdd.vtree().value()).values() {
                    labels.insert(*lbl);
                }
                let mut new_m: HashMap<VarLabel, bool> = HashMap::new();
                for (key, value) in assgn.iter() {
                    if labels.contains(key) {
                        let translated = man.tbl.sdd_to_bdd.get(key).unwrap();
                        new_m.insert(*translated, *value);
                    }
                }
                let bdd_ptr = sdd.as_bdd_ptr();
                man.tbl
                    .bdd_man(sdd.vtree().value())
                    .eval_bdd(bdd_ptr, &new_m)
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
            assert!(!lit_vec.is_empty(), "empty cnf");
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
        helper(&cvec, self).unwrap()
    }

    pub fn from_logical_expr(&mut self, expr: &LogicalExpr) -> SddPtr {
        match expr {
            &LogicalExpr::Literal(lbl, polarity) => self.var(VarLabel::new(lbl as u64), polarity),
            &LogicalExpr::Not(ref e) => {
                let e = self.from_logical_expr(e);
                e.neg()
            }
            &LogicalExpr::And(ref l, ref r) => {
                let r1 = self.from_logical_expr(l);
                let r2 = self.from_logical_expr(r);
                self.and(r1, r2)
            }
            &LogicalExpr::Or(ref l, ref r) => {
                let r1 = self.from_logical_expr(l);
                let r2 = self.from_logical_expr(r);
                self.or(r1, r2)
            }
            &LogicalExpr::Xor(ref l, ref r) => {
                let r1 = self.from_logical_expr(l);
                let r2 = self.from_logical_expr(r);
                self.xor(r1, r2)
            }
            &LogicalExpr::Iff(ref l, ref r) => {
                let r1 = self.from_logical_expr(l);
                let r2 = self.from_logical_expr(r);
                self.iff(r1, r2)
            }
            &LogicalExpr::Ite {
                ref guard,
                ref thn,
                ref els,
            } => {
                let g = self.from_logical_expr(guard);
                let thn = self.from_logical_expr(thn);
                let els = self.from_logical_expr(els);
                self.ite(g, thn, els)
            }
        }
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
    let mut mgr = SddManager::new(even_split(
        &[
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
        &[
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
        &[
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
        &[
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
        &[
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

    let vtree = even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
        ],
        2,
    );
    let mut man = SddManager::new(vtree.clone());
    let mut wmc_map = SddWmc::new(0.0, 1.0, vtree);
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

#[test]
fn sdd_wmc2() {
    let vtree = even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
        ],
        2,
    );
    let mut man = SddManager::new(vtree.clone());
    let mut wmc_map = SddWmc::new(0.0, 1.0, vtree);
    let x = man.var(VarLabel::new(0), true);
    wmc_map.set_weight(&mut man, VarLabel::new(0), 1.0, 1.0);
    let y = man.var(VarLabel::new(1), true);
    wmc_map.set_weight(&mut man, VarLabel::new(1), 1.0, 1.0);
    let f1 = man.var(VarLabel::new(2), true);
    wmc_map.set_weight(&mut man, VarLabel::new(2), 0.8, 0.2);
    let f2 = man.var(VarLabel::new(3), true);
    wmc_map.set_weight(&mut man, VarLabel::new(3), 0.7, 0.3);
    let iff1 = man.iff(x, f1);
    let iff2 = man.iff(y, f2);
    let obs = man.or(x, y);
    let and1 = man.and(iff1, iff2);
    let f = man.and(and1, obs);
    assert_eq!(
        man.unsmoothed_wmc(f, &wmc_map),
        0.2 * 0.3 + 0.2 * 0.7 + 0.8 * 0.3
    );
}

#[test]
fn is_canonical_trivial() {
    let mut mgr = SddManager::new(even_split(&[VarLabel::new(0)], 2));
    let a = mgr.var(VarLabel::new(0), true);

    assert_eq!(mgr.is_trimmed(a), true);
    assert_eq!(mgr.is_compressed(a), true);
    assert_eq!(mgr.is_canonical(a), true);
}

#[test]
fn not_compressed_or_trimmed_trivial() {
    let mut man = SddManager::new(even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
        ],
        2,
    ));

    man.set_compression(false); // necessary so we can observe duplication

    let x = man.var(VarLabel::new(0), true);
    let y = man.var(VarLabel::new(1), true);
    let f1 = man.var(VarLabel::new(2), true);

    let iff1 = man.iff(x, f1);
    let iff2 = man.iff(y, f1); // note: same g's here!
    let obs = man.or(x, x); // note: same x's here!
    let and1 = man.and(iff1, iff2);
    let f = man.and(and1, obs);

    assert_eq!(man.is_trimmed(f), false);
    assert_eq!(man.is_compressed(f), false);
    assert_eq!(man.is_canonical(f), false);
}

// note: this test is identical to the one above.
// However, we keep compression on, and flip the asserts on the predicates
// the idea is that we test that the SDD Manager does indeed compress / trim under-the-hood!
#[test]
fn test_compression() {
    let mut man = SddManager::new(even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
        ],
        2,
    ));

    let x = man.var(VarLabel::new(0), true);
    let y = man.var(VarLabel::new(1), true);
    let f1 = man.var(VarLabel::new(2), true);

    let iff1 = man.iff(x, f1);
    let iff2 = man.iff(y, f1); // note: same g's here!
    let obs = man.or(x, x);
    let and1 = man.and(iff1, iff2);
    let f = man.and(and1, obs);

    assert_eq!(man.is_trimmed(f), true);
    assert_eq!(man.is_compressed(f), true);
    assert_eq!(man.is_canonical(f), true);
}
