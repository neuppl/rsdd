use sdd::*;
use bdd::{VarLabel, Op};
use sdd_table::*;
use std::rc::Rc;
use std::collections::{HashMap, HashSet};
use ref_table::*;
use apply_cache::*;
use quickersort;
use btree::*;

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
    /// a helper structure which matches the vtree and is used for efficient LCA
    /// computation
    parent_ptr: Vec<(Option<usize>, usize)>,
    external_table: ExternalRefTable<SddPtr>,
    app_cache: Vec<SubTable<(SddPtr, SddPtr), SddPtr>>,
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
            &BTree::Leaf(ref v) => vec![(parent, level)],
            &BTree::Node(ref v, ref l, ref r) => {
                let mut l = helper(l, level + 1, Some(*v));
                let mut r = helper(r, level + 1, Some(*v));
                let mut v = vec![(parent, level)];
                v.append(&mut l);
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
fn is_prime(vtree: &VTree, idx_a: usize, idx_b: usize) -> bool {
    idx_a < idx_b
}

impl SddManager {
    pub fn new(vtree: VTree) -> SddManager {
        let mut app_cache = Vec::new();
        for _ in vtree.in_order_iter() {
            app_cache.push(SubTable::new(17));
        }
        SddManager {
            tbl: SddTable::new(&vtree),
            parent_ptr: into_parent_ptr_vec(&vtree),
            vtree: vtree,
            external_table: ExternalRefTable::new(),
            app_cache: app_cache,
        }
    }

    pub fn var(&mut self, lbl: VarLabel, is_true: bool) -> ExternalRef {
        let idx = match self.vtree.find_leaf_idx(&|ref l| l.contains(&lbl)) {
            None => panic!("var {:?} not found", lbl),
            Some(a) => a,
        };
        // convert the var label into the correct BDD var label
        let vlbl = self.tbl.sdd_to_bdd.get(&lbl).unwrap().clone();
        let r = SddPtr::new_bdd(self.tbl.bdd_man_mut(idx).var(vlbl, is_true), idx as u16);
        self.external_table.gen_or_inc(r)
    }


    pub fn negate(&self, a: SddPtr) -> SddPtr {
        a.neg()
    }

    /// Compresses the list of (prime, sub) terms
    #[inline(never)]
    fn compress(&mut self, mut r: Vec<(SddPtr, SddPtr)>) -> Vec<(SddPtr, SddPtr)> {
        // TODO: is this really canonical? check it thoroughly... I don't think it is
        quickersort::sort_by(&mut r[..], &|a, b| a.1.cmp(&b.1));
        r.dedup();
        if r.len() <= 2 {
            return r;
        }
        // to compress, we must disjoin all primes which share a sub
        // first, sort by `sub`
        let mut n = Vec::with_capacity(20);
        let (mut p, mut s) = r[0];
        for i in 1..r.len() {
            let (cur_p, cur_s) = r[i];
            if s == cur_s {
                // disjoin the prime
                p = self.or_internal(p, cur_p);
            } else {
                // found a unique sub, start a new chain
                n.push((p, s));
                p = cur_p;
                s = cur_s;
            }
        }
        n.push((p, s));
        n
    }

    pub fn or_internal(&mut self, a: SddPtr, b: SddPtr) -> SddPtr {
        self.and_rec(a.neg(), b.neg()).neg()
    }

    fn and_rec(&mut self, a: SddPtr, b: SddPtr) -> SddPtr {
        // first, check for a base case
        match (a, b) {
            (a, b) if a.is_true() => return b,
            (a, b) if b.is_true() => return a,
            (a, _) if a.is_false() => return SddPtr::new_const(false),
            (_, b) if b.is_false() => return SddPtr::new_const(false),
            (a, b) if self.eq(a, b) => return a,
            (a, b) if self.eq(a, b.neg()) => return SddPtr::new_const(false),
            _ => (),
        };

        // normalize the pointers to increase cache hit rate
        let (a, b) = if a < b { (a, b) } else { (b, a) };
        let r = if a.vtree() == b.vtree() {
            if a.is_bdd() {
                // both nodes are BDDs, so simply apply them together
                // and return the result
                let a_bdd = a.as_bdd_ptr();
                let b_bdd = b.as_bdd_ptr();
                let r = self.tbl.bdd_man_mut(a.vtree()).and(a_bdd, b_bdd);
                if r.is_false() {
                    SddPtr::new_const(false)
                } else if r.is_true() {
                    SddPtr::new_const(true)
                } else {
                    SddPtr::new_bdd(r, a.vtree() as u16)
                }
            } else {
                // now, we know that both `a` and `b` are non-const SDD nodes.
                // First, check if we have this application cached
                let c = self.app_cache[a.vtree()].get((a, b));
                if c.is_some() {
                    return c.unwrap();
                }

                // iterate over each prime/sum pair and do the relevant application
                let mut r: Vec<(SddPtr, SddPtr)> = Vec::with_capacity(30);
                let sl_outer = self.tbl.sdd_slice_or_panic(a);
                for &(ref p1, ref s1) in sl_outer.iter() {
                    let s1 = if a.is_compl() { s1.neg() } else { *s1 };
                    let sl_inner = self.tbl.sdd_slice_or_panic(b);
                    for &(ref p2, ref s2) in sl_inner.iter() {
                        let s2 = if b.is_compl() { s2.neg() } else { *s2 };
                        let p = self.and_rec(*p1, *p2);
                        if p.is_false() {
                            continue;
                        }

                        let s = self.and_rec(s1, s2);
                        // check if one of the nodes is true; if it is, we can
                        // return a `true` SddPtr here
                        if p.is_true() && s.is_true() {
                            let new_v = SddPtr::new_const(true);
                            self.app_cache[a.vtree()].insert((a, b), new_v.clone());
                            return new_v;
                        }
                        r.push((p, s));
                    }
                }
                if r.len() == 0 {
                    let new_v = SddPtr::new_const(false);
                    self.app_cache[a.vtree()].insert((a, b), new_v.clone());
                    return new_v;
                }

                // canonicalize
                r = self.compress(r);
                // guarantee first sub in the first node is not complemented
                // (regular form)
                let new_v = if r[0].1.is_compl() {
                    let compl_r = r.iter().map(|&(ref p, ref s)| (*p, s.neg())).collect();
                    self.tbl
                        .get_or_insert_sdd(&SddOr { nodes: compl_r }, a.vtree())
                        .neg()
                } else {
                    self.tbl.get_or_insert_sdd(&SddOr { nodes: r }, a.vtree())
                };
                self.app_cache[a.vtree()].insert((a, b), new_v.clone());
                new_v
            }
        } else {
            // first, normalize so `a` is always prime if possible
            let (a, b) = if is_prime(&self.vtree, a.vtree(), b.vtree()) {
                (a, b)
            } else {
                (b, a)
            };
            // normalize and re-invoke the helper
            let av = a.vtree();
            let bv = b.vtree();
            let lca = least_common_ancestor(&self.parent_ptr, av, bv);
            // 3 cases:
            //   1. The lca is the prime `a`
            //   2. The lca is the sub `b`
            //   3. The lca is a shared parent equal to neither
            // TODO: remove the unnecessary allocations
            if lca == av {
                let v = vec![(SddPtr::new_const(true), b)];
                let new = self.tbl.get_or_insert_sdd(&SddOr { nodes: v }, av);
                self.and_rec(a, new)
            } else if lca == bv {
                let v = vec![(a, SddPtr::new_const(true))];
                let new = self.tbl.get_or_insert_sdd(&SddOr { nodes: v }, bv);
                self.and_rec(new, b)
            } else {
                let v1 = vec![
                    (a, SddPtr::new_const(true)),
                    (a.neg(), SddPtr::new_const(false)),
                ];
                let v2 = vec![(SddPtr::new_const(true), b)];
                let new_1 = self.tbl.get_or_insert_sdd(&SddOr { nodes: v1 }, lca);
                let new_2 = self.tbl.get_or_insert_sdd(&SddOr { nodes: v2 }, lca);
                self.and_rec(new_1, new_2)
            }
        };
        // println!("applying  {}\n  {}\n  result: {}",
        //          self.print_sdd_internal(a), self.print_sdd_internal(b),
        //          self.print_sdd_internal(r));
        r
    }


    pub fn apply(&mut self, op: Op, a: ExternalRef, b: ExternalRef) -> ExternalRef {
        // println!("applying {} to {} with op {:?}", self.print_sdd(a), self.print_sdd(b), op);
        let i_a = self.external_table.into_internal(a);
        let i_b = self.external_table.into_internal(b);
        let r = match op {
            Op::BddAnd => self.and_rec(i_a, i_b),
            Op::BddOr => self.or_internal(i_a, i_b),
        };
        let r = self.external_table.gen_or_inc(r);
        r
    }

    fn print_sdd_internal(&self, ptr: SddPtr) -> String {
        if ptr.is_bdd() {
            let bdd_ptr = ptr.as_bdd_ptr();
            let m = self.tbl.bdd_conv(ptr.vtree());
            let s = self.tbl.bdd_man(ptr.vtree()).print_bdd_lbl(bdd_ptr, m);
            s
        } else {
            let mut s = String::from(format!("{}\\/", if ptr.is_compl() { "!" } else { "" }));
            if ptr.is_true() {
                return String::from("T");
            } else if ptr.is_false() {
                return String::from("F");
            }
            let sl = self.tbl.sdd_slice_or_panic(ptr);
            for &(ref prime, ref sub) in sl.iter() {
                let new_s1 = self.print_sdd_internal(prime.clone());
                let new_s2 = self.print_sdd_internal(sub.clone());
                s.push_str(&format!("(/\\ {} {})", new_s1, new_s2));
            }
            s
        }
    }

    pub fn print_sdd(&self, ptr: ExternalRef) -> String {
        let int_ptr = self.external_table.into_internal(ptr);
        self.print_sdd_internal(int_ptr)
    }

    pub fn eval_sdd(&self, ptr: ExternalRef, assgn: &HashMap<VarLabel, bool>) -> bool {
        fn helper(man: &SddManager, sdd: SddPtr, assgn: &HashMap<VarLabel, bool>) -> bool {
            if sdd.is_false() {
                false
            } else if sdd.is_true() {
                true
            } else if sdd.is_bdd() {
                assert!(!sdd.is_compl());
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
                let sl = man.tbl.sdd_slice_or_panic(sdd);
                for &(ref p, ref s) in sl.iter() {
                    let v1 = helper(man, *p, assgn);
                    let v2 = helper(man, *s, assgn);
                    res = res || (v1 && v2)
                }
                if sdd.is_compl() { !res } else { res }
            }
        }
        let i = self.external_table.into_internal(ptr);
        helper(self, i, assgn)
    }


    fn eq(&self, a: SddPtr, b: SddPtr) -> bool {
        a == b
    }

    pub fn sdd_eq(&self, a: ExternalRef, b: ExternalRef) -> bool {
        let a_int = self.external_table.into_internal(a);
        let b_int = self.external_table.into_internal(b);
        self.eq(a_int, b_int)
    }
}


#[test]
fn make_sdd() {
    let simple_vtree = BTree::Node(
        (),
        Box::new(BTree::Leaf(vec![VarLabel::new(0), VarLabel::new(1)])),
        Box::new(BTree::Leaf(vec![VarLabel::new(2), VarLabel::new(3)])),
    );
    let mut man = SddManager::new(simple_vtree);
    let v = man.var(VarLabel::new(2), true);
    println!("sdd: {}", man.print_sdd(v));
}


#[test]
fn sdd_simple_apply() {
    let simple_vtree = BTree::Node(
        (),
        Box::new(BTree::Leaf(vec![VarLabel::new(0), VarLabel::new(1)])),
        Box::new(BTree::Leaf(vec![VarLabel::new(2), VarLabel::new(3)])),
    );
    let mut man = SddManager::new(simple_vtree);
    let v1 = man.var(VarLabel::new(3), true);
    let v2 = man.var(VarLabel::new(1), false);
    let v3 = man.apply(Op::BddOr, v1, v2);
    println!("sdd1: {}", man.print_sdd(v1));
    println!("sdd2: {}", man.print_sdd(v2));
    println!("sdd: {}", man.print_sdd(v3));
}

#[test]
fn test_lca() {
    let simple_vtree = BTree::Node(
        (),
        Box::new(BTree::Leaf(vec![VarLabel::new(0), VarLabel::new(1)])),
        Box::new(BTree::Leaf(vec![VarLabel::new(2), VarLabel::new(3)])),
    );
    //    0
    // 1      4
    //2 3   5  6
    let simple_vtree2 = BTree::Node(
        (),
        Box::new(simple_vtree.clone()),
        Box::new(simple_vtree.clone()),
    );
    let par_vec = into_parent_ptr_vec(&simple_vtree2);
    assert_eq!(least_common_ancestor(&par_vec, 2, 3), 1);
    assert_eq!(least_common_ancestor(&par_vec, 2, 5), 0);
    assert_eq!(least_common_ancestor(&par_vec, 2, 1), 1);
    assert_eq!(least_common_ancestor(&par_vec, 4, 2), 0);
}
