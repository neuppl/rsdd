//! implements an SDD as a collection of BDDs

use std::collections::{HashSet, HashMap};
use std::rc::Rc;
use bdd::*;
use var_order::VarOrder;
use manager::*;
use fnv::FnvHashMap;
use ref_table::*;
#[macro_use]
use util::*;


/// The internal vtree represents a partition of variables among BDDs
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VTree {
    VNode(Rc<VTree>, Rc<VTree>),
    VLeaf(Vec<VarLabel>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
struct SddPtr {
    /// the index into the table
    idx: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
struct ExternalSdd {
    idx: usize,
}

impl SddPtr {
    fn new(idx: usize) -> SddPtr {
        SddPtr { idx: idx }
    }
}

/// Represent an SDD as a list of BDD pointers. The ordering of the BDDs
/// corresponds with the depth-first left-first traversal of the vtree
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Sdd {
    Or(Vec<SddPtr>),
    And(SddPtr, SddPtr),
    Bdd(BddPtr),
}

impl Sdd {
    fn normalized_or(mut nodes: Vec<SddPtr>) -> Sdd {
        nodes.sort();
        Sdd::Or(nodes)
    }
}

/// Handles memory management for the SDD manager
struct SddAllocator {
    elem: Vec<Sdd>,
    unique_tbl: FnvHashMap<Sdd, SddPtr>,
}

impl SddAllocator {
    fn new() -> SddAllocator {
        SddAllocator {
            elem: Vec::with_capacity(5000),
            unique_tbl: FnvHashMap::default(),
        }
    }

    fn get_or_insert(&mut self, sdd: Sdd) -> SddPtr {
        let r = match self.unique_tbl.get(&sdd) {
            None => None,
            Some(a) => Some(a.clone()),
        };
        match r {
            None => {
                let idx = self.elem.len();
                self.elem.push(sdd.clone());
                let r = SddPtr::new(idx);
                self.unique_tbl.insert(sdd, r.clone());
                return r;
            }
            Some(v) => return v,
        }
    }

    fn deref(&self, ptr: SddPtr) -> &Sdd {
        &self.elem[ptr.idx]
    }
}

/// Iterate over the leaves of the VTree in depth-first order
struct VTreeDFSIter<'a> {
    stack: Vec<&'a VTree>,
}

impl<'a> Iterator for VTreeDFSIter<'a> {
    type Item = &'a Vec<VarLabel>;

    fn next(&mut self) -> Option<Self::Item> {
        use self::VTree::*;
        let top_unwrap = match self.stack.pop() {
            None => return None,
            Some(v) => v,
        };
        match top_unwrap {
            &VLeaf(ref res) => Some(&res),
            &VNode(ref l, ref r) => {
                self.stack.push(&r);
                self.stack.push(l);
                self.next()
            }
        }
    }
}

impl VTree {
    fn dfs_iter(&self) -> VTreeDFSIter {
        VTreeDFSIter { stack: vec![self] }
    }

    fn contains(&self, lbl: VarLabel) -> bool {
        match self {
            &VTree::VLeaf(ref v) => v.iter().find(|&x| *x == lbl).is_some(),
            &VTree::VNode(ref l, ref r) => l.contains(lbl) || r.contains(lbl),
        }
    }

    /// generate an even vtree by splitting a variable ordering in half `num_splits`
    /// times
    pub fn even_split(order: &[VarLabel], num_splits: usize) -> VTree {
        if num_splits <= 0 {
            VTree::VLeaf(order.to_vec())
        } else {
            let (l_s, r_s) = order.split_at(order.len() / 2);
            let l_tree = VTree::even_split(l_s, num_splits-1);
            let r_tree = VTree::even_split(r_s, num_splits-1);
            VTree::VNode(Rc::new(l_tree), Rc::new(r_tree))
        }
    }
}

pub struct SddManager {
    /// Managers ordered by their order in a depth-first left-first traversal of
    /// the vtree
    bdd_managers: Vec<BddManager>,
    alloc: SddAllocator,
    /// mapping between sdd and bdd variable labels
    sdd_to_bdd: HashMap<VarLabel, VarLabel>,
    /// mapping between bdd and sdd variable labels
    bdd_to_sdd: Vec<HashMap<VarLabel, VarLabel>>,
    vtree: VTree,
    external_table: ExternalRefTable<SddPtr>,
}

impl SddManager {
    pub fn new(vtree: VTree) -> SddManager {
        let mut c = SddManager {
            bdd_managers: Vec::new(),
            sdd_to_bdd: HashMap::new(),
            bdd_to_sdd: Vec::new(),
            vtree: vtree,
            alloc: SddAllocator::new(),
            external_table: ExternalRefTable::new(),
        };
        for _ in c.vtree.dfs_iter() {
            c.bdd_to_sdd.push(HashMap::new())
        }
        for (tbl_idx, vars) in c.vtree.dfs_iter().enumerate() {
            let order = VarOrder::new(vars.clone());
            c.bdd_managers.push(BddManager::new(order));
            for (var_idx, v) in vars.iter().enumerate() {
                c.sdd_to_bdd.insert(
                    v.clone(),
                    VarLabel::new(var_idx as u64),
                );
                c.bdd_to_sdd[tbl_idx].insert(VarLabel::new(var_idx as u64), v.clone());
            }
        }
        return c;
    }

    pub fn var(&mut self, lbl: VarLabel, is_true: bool) -> ExternalRef {
        fn var_helper(
            man: &mut SddManager,
            count: usize,
            lbl: VarLabel,
            is_true: bool,
            value: bool,
            vtree: &VTree,
        ) -> (ExternalRef, usize) {
            match vtree {
                &VTree::VLeaf(ref lbl_vec) => {
                    if lbl_vec.iter().find(|&x| *x == lbl).is_some() {
                        let vlbl = man.sdd_to_bdd.get(&lbl).unwrap();
                        let bdd_r = man.bdd_managers[count].var(vlbl.clone(), is_true);
                        let new_sdd = man.alloc.get_or_insert(Sdd::Bdd(bdd_r));
                        (man.external_table.gen_or_inc(new_sdd), count + 1)
                    } else {
                        let bdd_r = if value {
                            man.bdd_managers[count].true_ptr()
                        } else {
                            man.bdd_managers[count].false_ptr()
                        };
                        let new_sdd = man.alloc.get_or_insert(Sdd::Bdd(bdd_r));
                        (man.external_table.gen_or_inc(new_sdd), count + 1)
                    }
                }
                &VTree::VNode(ref l, ref r) if l.contains(lbl) => {
                    let (l_sdd_1, cnt_l) = var_helper(man, count, lbl, is_true, true, &l);
                    let (l_sdd_2, _) = var_helper(man, count, lbl, !is_true, true, &l);
                    let (r_sdd_1, cnt_r) = var_helper(man, cnt_l, lbl, is_true, true, &r);
                    let (r_sdd_2, _) = var_helper(man, cnt_l, lbl, is_true, false, &r);
                    let int_lsdd_1 = man.external_table.into_internal(l_sdd_1);
                    let int_lsdd_2 = man.external_table.into_internal(l_sdd_2);
                    let int_rsdd_1 = man.external_table.into_internal(r_sdd_1);
                    let int_rsdd_2 = man.external_table.into_internal(r_sdd_2);
                    let and_1 = man.alloc.get_or_insert(Sdd::And(int_lsdd_1, int_rsdd_1));
                    let and_2 = man.alloc.get_or_insert(Sdd::And(int_lsdd_2, int_rsdd_2));
                    let r_vec = vec![and_1, and_2];
                    let new_sdd = man.alloc.get_or_insert(Sdd::Or(r_vec));
                    (man.external_table.gen_or_inc(new_sdd), cnt_r)
                }
                &VTree::VNode(ref l, ref r) => {
                    let (l_sdd_1, cnt_l) = var_helper(man, count, lbl, is_true, true, &l);
                    let (r_sdd_1, cnt_r) = var_helper(man, cnt_l, lbl, is_true, value, &r);
                    let int_lsdd_1 = man.external_table.into_internal(l_sdd_1);
                    let int_rsdd_1 = man.external_table.into_internal(r_sdd_1);
                    let and_1 = man.alloc.get_or_insert(Sdd::And(int_lsdd_1, int_rsdd_1));
                    let r_vec = vec![and_1];
                    let new_sdd = man.alloc.get_or_insert(Sdd::Or(r_vec));
                    (man.external_table.gen_or_inc(new_sdd), cnt_r)
                }
            }
        }
        // TODO this is gross; cloning the vtree should definitely be avoided
        let new_v = self.vtree.clone();
        let (res, _) = var_helper(self, 0, lbl, is_true, true, &new_v);
        res
    }

    pub fn apply(&mut self, op: Op, a: ExternalRef, b: ExternalRef) -> ExternalRef {
        fn helper(
            man: &mut SddManager,
            op: Op,
            a: SddPtr,
            b: SddPtr,
            cnt: usize,
        ) -> (SddPtr, usize) {
            // println!("apply");
            use self::Sdd::*;
            let a_sdd = man.alloc.deref(a).clone();
            let b_sdd = man.alloc.deref(b).clone();
            match (a_sdd, b_sdd) {
                (Or(va), Or(vb)) => {
                    let mut r: Vec<SddPtr> = Vec::new();
                    let mut new_cnt = 0;
                    for a_sdd in va.iter() {
                        for b_sdd in vb.iter() {
                            let (new_sdd, cur_cnt) =
                                helper(man, op, a_sdd.clone(), b_sdd.clone(), cnt);
                            new_cnt = cur_cnt;
                            r.push(new_sdd);
                        }
                    }
                    // normalize r
                    r.sort();
                    r.dedup();
                    let new_node = man.alloc.get_or_insert(Or(r));
                    (new_node, new_cnt)
                }
                (And(p1, s1), And(p2, s2)) => {
                    // TODO: this can be parallelized!
                    let (new_p, new_cnt) = helper(man, Op::BddAnd, p1, p2, cnt);
                    let (new_s, final_cnt) = helper(man, op, s1, s2, new_cnt);
                    let new_node = man.alloc.get_or_insert(And(new_p, new_s));
                    (new_node, final_cnt)
                }
                (Bdd(b1), Bdd(b2)) => {
                    let r = man.bdd_managers[cnt].apply(op, b1, b2);
                    let new_node = man.alloc.get_or_insert(Bdd(r));
                    (new_node, cnt + 1)
                }
                _ => panic!("invalid SDD structure"),
            }
        }
        let i_a = self.external_table.into_internal(a);
        let i_b = self.external_table.into_internal(b);
        let (r, _) = helper(self, op, i_a, i_b, 0);
        self.external_table.gen_or_inc(r)
    }

    pub fn eval_sdd(&self, ptr: ExternalRef, assgn: &HashMap<VarLabel, bool>) -> bool {
        fn helper(
            man: &SddManager,
            a: SddPtr,
            cnt: usize,
            assgn: &HashMap<VarLabel, bool>,
        ) -> (bool, usize) {
            use self::Sdd::*;
            let a_sdd = man.alloc.deref(a).clone();
            match a_sdd {
                Or(va) => {
                    let mut new_cnt = 0;
                    let mut v = false;
                    for a_sdd in va.iter() {
                        let (cur_v, cur_cnt) = helper(man, a_sdd.clone(), cnt, assgn);
                        v = v || cur_v;
                        new_cnt = cur_cnt;
                    }
                    (v, new_cnt)
                }
                And(p1, s1) => {
                    let (v1, new_cnt) = helper(man, p1, cnt, assgn);
                    let (v2, final_cnt) = helper(man, s1, new_cnt, assgn);
                    (v1 && v2, final_cnt)
                }
                Bdd(b1) => {
                    // we must convert `assgn`, which is a hash map of SDD values, into a
                    // hash map of BDD values

                    // a set of all var labels which belong to this BDD
                    let mut labels: HashSet<VarLabel> = HashSet::new();
                    for lbl in man.bdd_to_sdd[cnt].values() {
                        labels.insert(lbl.clone());
                    }
                    let mut new_m: HashMap<VarLabel, bool> = HashMap::new();
                    for (key, value) in assgn.iter() {
                        if labels.contains(key) {
                            let translated = man.sdd_to_bdd.get(key).unwrap();
                            new_m.insert(*translated, *value);
                        }
                    }
                    // now new_m has the correct variable mappings in it, so we can evaluate it
                    let v = man.bdd_managers[cnt].eval_bdd(b1, &new_m);
                    (v, cnt+1)
                }
            }
        }
        let i_a = self.external_table.into_internal(ptr);
        let (r, _) = helper(self, i_a, 0, assgn);
        r
    }

    pub fn print_sdd(&self, ptr: ExternalRef) -> String {
        fn helper(man: &SddManager, ptr: &SddPtr, cnt: usize) -> (String, usize) {
            match man.alloc.deref(ptr.clone()) {
                &Sdd::And(ref p, ref s) => {
                    let (s1, c1) = helper(man, p, cnt);
                    let (s2, c2) = helper(man, s, c1);
                    (format!("(/\\ {} {})", s1, s2), c2)
                }
                &Sdd::Or(ref v) => {
                    let mut s = String::from("(\\/ ");
                    let mut cur_cnt = 0;
                    for ptr in v.into_iter() {
                        let (new_s, new_cnt) = helper(man, ptr, cnt);
                        cur_cnt = new_cnt;
                        s.push_str(&new_s);
                    }
                    (s + ")", cur_cnt)
                }
                &Sdd::Bdd(ref b) => (man.bdd_managers[cnt].print_bdd(*b), cnt + 1),
            }
        }
        let int_ptr = self.external_table.into_internal(ptr);
        let (r, _) = helper(self, &int_ptr, 0);
        r
    }
}


#[test]
fn make_sdd() {
    let simple_vtree = VTree::VNode(
        Rc::new(VTree::VLeaf(vec![VarLabel::new(0), VarLabel::new(1)])),
        Rc::new(VTree::VLeaf(vec![VarLabel::new(2), VarLabel::new(3)])),
    );
    let mut man = SddManager::new(simple_vtree);
    let v = man.var(VarLabel::new(1), true);
    println!("sdd: {}", man.print_sdd(v));
}


#[test]
fn sdd_simple_apply() {
    let simple_vtree = VTree::VNode(
        Rc::new(VTree::VLeaf(vec![VarLabel::new(0), VarLabel::new(1)])),
        Rc::new(VTree::VLeaf(vec![VarLabel::new(2), VarLabel::new(3)])),
    );
    let mut man = SddManager::new(simple_vtree);
    let v1 = man.var(VarLabel::new(1), true);
    let v2 = man.var(VarLabel::new(3), true);
    let v3 = man.apply(Op::BddAnd, v1, v2);
    println!("sdd: {}", man.print_sdd(v3));
}
