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
    external_table: ExternalRefTable<SddPtr>,
    app_cache: Vec<SubTable<(Op, SddPtr, SddPtr), SddPtr>>,
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


/// evaluate two sdd pointers which are valid wrt. the same vtree; returns
/// `None` if both of the pointers are nodes
fn eval_op(op: Op, a: SddPtr, b: SddPtr) -> Option<SddPtr> {
    let v = a.vtree() as u16;
    match op {
        Op::BddAnd => {
            if a.is_true() {
                Some(b)
            } else if b.is_true() {
                Some(a)
            } else if a.is_false() || b.is_false() {
                Some(SddPtr::new_const(false, v))
            } else {
                None
            }
        }
        Op::BddOr => {
            if a.is_false() {
                Some(b)
            } else if b.is_false() {
                Some(a)
            } else if a.is_true() || b.is_true() {
                Some(SddPtr::new_const(true, v))
            } else {
                None
            }
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
            app_cache.push(SubTable::new(10000));
        }
        SddManager {
            tbl: SddTable::new(&vtree),
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

    fn negate(&mut self, a: SddPtr) -> SddPtr {
        match a.ptr_type() {
            SddPtrType::False => SddPtr::new_const(true, a.vtree() as u16),
            SddPtrType::True => SddPtr::new_const(false, a.vtree() as u16),
            SddPtrType::Node => {
                if self.tbl.is_bdd(a) {
                    let bdd_ptr = a.as_bdd_ptr();
                    let neg_bdd = self.tbl.bdd_man_mut(a.vtree()).negate(bdd_ptr);
                    SddPtr::new_bdd(neg_bdd, a.vtree() as u16)
                } else {
                    let mut v = Vec::new();
                    for (p, s) in self.tbl.sdd_or_panic(a) {
                        v.push((p, self.negate(s)))
                    }
                    quickersort::sort(&mut v[..]);
                    v.dedup();
                    self.tbl.get_or_insert_sdd(SddOr { nodes: v }, a.vtree())
                }
            }
        }
    }

    pub fn apply(&mut self, op: Op, a: ExternalRef, b: ExternalRef) -> ExternalRef {
        fn helper(
            man: &mut SddManager,
            op: Op,
            a: SddPtr,
            b: SddPtr,
            parent_ptr: &Vec<(Option<usize>, usize)>,
        ) -> SddPtr {
            // normalize the pointers to increase cache hit rate
            let (a, b) = if a < b { (a, b) } else { (b, a) };
            // println!("applying({:?}) , {} to {}",
            //          op, man.print_sdd_internal(a), man.print_sdd_internal(b));
            let r = if a.vtree() == b.vtree() {
                if man.tbl.is_bdd(a) {
                    // both nodes are BDDs, so simply apply them together
                    // and return the result
                    let a_bdd = a.as_bdd_ptr();
                    let b_bdd = b.as_bdd_ptr();
                    let r = man.tbl.bdd_man_mut(a.vtree()).apply(op, a_bdd, b_bdd);
                    if r.is_false() {
                        SddPtr::new_const(false, a.vtree() as u16)
                    } else if r.is_true() {
                        SddPtr::new_const(true, a.vtree() as u16)
                    } else {
                        SddPtr::new_bdd(r, a.vtree() as u16)
                    }
                } else {
                    // check if either node is a constant
                    let v = eval_op(op, a, b);
                    if v.is_some() {
                        return v.unwrap();
                    }
                    // now, we know that both `a` and `b` are SDD nodes. First, check if
                    // we have this application cached
                    let c = man.app_cache[a.vtree()].get((op, a, b));
                    if c.is_some() {
                        return c.unwrap();
                    }
                    let a_sdd = man.tbl.sdd_or_panic(a);
                    let mut r: Vec<(SddPtr, SddPtr)> = Vec::with_capacity(30);
                    for (p1, s1) in a_sdd.into_iter() {
                        for (p2, s2) in man.tbl.sdd_or_panic(b) {
                            let p = helper(man, Op::BddAnd, p1, p2, parent_ptr);
                            if p.is_false() {
                                continue;
                            }
                            let s = helper(man, op, s1, s2, parent_ptr);
                            if s.is_false() {
                                continue;
                            }
                            // check if one of the nodes is true; if it is, we can
                            // return a `true` SddPtr here
                            if p.is_true() && s.is_true() {
                                let new_v = SddPtr::new_const(true, a.vtree() as u16);
                                man.app_cache[a.vtree()].insert((op, a, b), new_v.clone());
                                return new_v;
                            }
                            r.push((p, s));
                        }
                    }
                    if r.len() == 0 {
                        let new_v = SddPtr::new_const(false, a.vtree() as u16);
                        man.app_cache[a.vtree()].insert((op, a, b), new_v.clone());
                        return new_v;
                    }
                    quickersort::sort(&mut r[..]);
                    r.dedup();
                    let new_v = man.tbl.get_or_insert_sdd(SddOr { nodes: r }, a.vtree());
                    man.app_cache[a.vtree()].insert((op, a, b), new_v.clone());
                    new_v
                }
            } else {
                // normalize and re-invoke the helper
                let av = a.vtree();
                let bv = b.vtree();
                let lca = least_common_ancestor(parent_ptr, av, bv);
                if lca == av {
                    let v = if is_prime(&man.vtree, av, bv) {
                        vec![(SddPtr::new_const(true, av as u16), b)]
                    } else {
                        vec![(b, SddPtr::new_const(true, av as u16))]
                    };
                    let new = man.tbl.get_or_insert_sdd(SddOr { nodes: v }, av);
                    helper(man, op, a, new, parent_ptr)
                } else if lca == bv {
                    let v = if is_prime(&man.vtree, bv, av) {
                        vec![(SddPtr::new_const(true, bv as u16), a)]
                    } else {
                        vec![(a, SddPtr::new_const(true, bv as u16))]
                    };
                    let new = man.tbl.get_or_insert_sdd(SddOr { nodes: v }, bv);
                    helper(man, op, new, b, parent_ptr)
                } else {
                    let (fst, snd) = if is_prime(&man.vtree, av, bv) {
                        (a, b)
                    } else {
                        (b, a)
                    };
                    let v1 = vec![(fst, SddPtr::new_const(true, lca as u16)),
                                  (man.negate(fst), SddPtr::new_const(false, lca as u16))
                    ];
                    let v2 = vec![(SddPtr::new_const(true, lca as u16), snd)];
                    let new_1 = man.tbl.get_or_insert_sdd(SddOr { nodes: v1 }, lca);
                    let new_2 = man.tbl.get_or_insert_sdd(SddOr { nodes: v2 }, lca);
                    helper(man, op, new_1, new_2, parent_ptr)
                }
            };
            r
        }
        // println!("applying {} to {} with op {:?}", self.print_sdd(a), self.print_sdd(b), op);
        let i_a = self.external_table.into_internal(a);
        let i_b = self.external_table.into_internal(b);
        let pvec = into_parent_ptr_vec(&self.vtree);
        let r = helper(self, op, i_a, i_b, &pvec);
        let r = self.external_table.gen_or_inc(r);
        // println!("result: {}", self.print_sdd(r));
        r
    }

    fn print_sdd_internal(&self, ptr: SddPtr) -> String {
        if self.tbl.is_bdd(ptr) {
            let bdd_ptr = ptr.as_bdd_ptr();
            let m = self.tbl.bdd_conv(ptr.vtree());
            let s = self.tbl.bdd_man(ptr.vtree()).print_bdd_lbl(bdd_ptr, m);
            s
        } else {
            let mut s = String::from("\\/");
            if ptr.is_true() {
                return String::from("T");
            } else if ptr.is_false() {
                return String::from("F");
            }
            for &(ref prime, ref sub) in self.tbl.sdd_iter_or_panic(ptr) {
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
            } else if man.tbl.is_bdd(sdd) {
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
                for (p, s) in man.tbl.sdd_or_panic(sdd) {
                    let v1 = helper(man, p, assgn);
                    let v2 = helper(man, s, assgn);
                    res = res || (v1 && v2)
                }
                res
            }
        }
        let i = self.external_table.into_internal(ptr);
        helper(self, i, assgn)
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
