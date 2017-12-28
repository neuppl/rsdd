use sdd::*;
use bdd::{VarLabel, Op};
use sdd_table::*;
use std::rc::Rc;
use std::collections::HashMap;
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

impl SddManager {
    pub fn new(vtree: VTree) -> SddManager {
        let mut app_cache = Vec::new();
        for _ in vtree.in_order_iter() {
            app_cache.push(SubTable::new(10000));
        }
        let mut c = SddManager {
            tbl: SddTable::new(&vtree),
            vtree: vtree,
            external_table: ExternalRefTable::new(),
            app_cache: app_cache,
        };
        return c;
    }



    pub fn var(&mut self, lbl: VarLabel, is_true: bool) -> ExternalRef {
        let idx = match self.vtree.find_leaf_idx(&|ref l| l.contains(&lbl)) {
            None => panic!("var {:?} not found", lbl),
            Some(a) => a,
        };
        let r = SddPtr::new_bdd(self.tbl.bdd_man_mut(idx).var(lbl, is_true), idx as u16);
        self.external_table.gen_or_inc(r)
    }

    pub fn apply(&mut self, op: Op, a: ExternalRef, b: ExternalRef) -> ExternalRef {
        fn helper(
            man: &mut SddManager,
            order: &BTree<usize, usize>,
            is_prime: bool,
            op: Op,
            a: SddPtr,
            b: SddPtr,
        ) -> Option<SddPtr> {
            match order {
                &BTree::Leaf(c) => {
                    let a_bdd = a.as_bdd_ptr();
                    let b_bdd = b.as_bdd_ptr();
                    let r = man.tbl.bdd_man_mut(c).apply(op, a_bdd, b_bdd);
                    if r.is_false() && is_prime {
                        None
                    } else {
                        Some(SddPtr::new_bdd(r, c as u16))
                    }
                }
                &BTree::Node(c, ref l_n, ref r_n) => {
                    let v = man.app_cache[c].get((a, b));
                    match v {
                        None => {
                            let mut r: Vec<(SddPtr, SddPtr)> = Vec::with_capacity(30);
                            for (p1, s1) in man.tbl.sdd_or_panic(a) {
                                for (p2, s2) in man.tbl.sdd_or_panic(b) {
                                    let p = helper(man, l_n, true, Op::BddAnd, p1, p2);
                                    match p {
                                        None => (),
                                        Some(v) => {
                                            // unwrap, since it will never be `none`
                                            let s = helper(man, r_n, false, op, s1, s2).unwrap();
                                            r.push((v, s));
                                        }
                                    }
                                }
                            }
                            quickersort::sort(&mut r[..]);
                            r.dedup();
                            let l = r.len();
                            let new_v = man.tbl.get_or_insert_sdd(SddOr { nodes: r }, c);
                            man.app_cache[c].insert((a, b), new_v.clone());
                            if is_prime && l == 0 {
                                None
                            } else {
                                Some(new_v)
                            }
                        }
                        Some(v) => Some(v),
                    }
                }
            }
        }
        let i_a = self.external_table.into_internal(a);
        let i_b = self.external_table.into_internal(b);
        let t = self.vtree.into_order_tree();
        let r = helper(self, &t, false, op, i_a, i_b);
        match r {

            Some(r) => self.external_table.gen_or_inc(r),
            None => panic!("unsat base sdd"),
        }
    }

    pub fn print_sdd(&self, ptr: ExternalRef) -> String {
        fn helper(man: &SddManager, ptr: SddPtr, cnt: u16) -> (String, u16) {
            if man.tbl.is_bdd(ptr) {
                let bdd_ptr = ptr.as_bdd_ptr();
                let s = man.tbl.bdd_man(cnt as usize).print_bdd(bdd_ptr);
                (s, cnt + 1)
            } else {
                let mut s = String::from("\\/");
                let mut new_cnt = 0;
                for &(ref prime, ref sub) in man.tbl.sdd_iter_or_panic(ptr) {
                    let (new_s1, cnt_l) = helper(man, prime.clone(), cnt + 1);
                    let (new_s2, cnt_r) = helper(man, sub.clone(), cnt_l);
                    new_cnt = cnt_r;
                    s.push_str(&format!("(/\\ {} {})", new_s1, new_s2));
                }
                (s, new_cnt)
            }
        }
        let int_ptr = self.external_table.into_internal(ptr);
        let (r, _) = helper(self, int_ptr, 0);
        r
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
    let v1 = man.var(VarLabel::new(1), true);
    let v2 = man.var(VarLabel::new(1), false);
    let v3 = man.apply(Op::BddAnd, v1, v2);
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
        Box::new(simple_vtree.clone())
    );
    let par_vec = into_parent_ptr_vec(&simple_vtree2);
    println!("parent vec: {:?}", par_vec);
    assert_eq!(least_common_ancestor(&par_vec, 2, 3), 1);
    assert_eq!(least_common_ancestor(&par_vec, 2, 5), 0);
    assert_eq!(least_common_ancestor(&par_vec, 2, 1), 1);
    assert_eq!(least_common_ancestor(&par_vec, 4, 2), 0);
}
