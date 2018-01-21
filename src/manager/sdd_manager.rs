use repr::sdd::*;
use backing_store::sdd_table::*;
use repr::var_label::VarLabel;
use std::collections::{HashMap, HashSet};
use manager::ref_table::*;
use manager::cache::lru::*;
use repr::cnf::Cnf;
use quickersort;
use util::btree::*;
use repr::boolexpr::BoolExpr;

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
                let mut l = helper(l, level + 1, Some(*v));
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
fn is_prime(vtree: &VTree, idx_a: usize, idx_b: usize) -> bool {
    idx_a < idx_b
}

impl SddManager {
    pub fn new(vtree: VTree) -> SddManager {
        let mut app_cache = Vec::new();
        for _ in vtree.in_order_iter() {
            app_cache.push(Lru::new(17));
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
    // TODO: Optimize for 2-variable case
    #[inline(never)]
    fn compress(&mut self, mut r: Vec<(SddPtr, SddPtr)>) -> Vec<(SddPtr, SddPtr)> {
        // this sort guarantees several things:
        // (1) if there is a constant sub (i.e., false), it will be at the *end*
        //     of the returned vector (because we reverse the order by pushing)
        // (2) two equal subs will be adjacent in the ordering, which allows for a
        //     single pass for doing compression
        quickersort::sort_by(&mut r[..], &|a, b| a.1.cmp(&b.1));
        r.dedup();
        // to compress, we must disjoin all primes which share a sub
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
        // now all the subs are unique, sort by primes now to guarantee
        // canonicity
        // TODO: combine these into a single initial sort
        quickersort::sort_by(&mut n[..], &|a, b| a.0.cmp(&b.0));
        n
    }

    pub fn or_internal(&mut self, a: SddPtr, b: SddPtr) -> SddPtr {
        self.and_rec(a.neg(), b.neg()).neg()
    }


    fn and_rec(&mut self, a: SddPtr, b: SddPtr) -> SddPtr {
        // println!("applying\n {}\n {}\n",
        //          self.print_sdd_internal(a), self.print_sdd_internal(b));

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

        //  check if we have this application cached
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
        let outer_v: Vec<(SddPtr, SddPtr)>;
        let inner_v: Vec<(SddPtr, SddPtr)>;
        let (outer, inner) = if av == bv {
            let outer = self.tbl.sdd_slice_or_panic(a);
            let inner = self.tbl.sdd_slice_or_panic(b);
            (outer, inner)
        } else if lca == av {
            let outer = self.tbl.sdd_slice_or_panic(a);
            inner_v = vec![(SddPtr::new_const(true), b.regular())];
            (outer, inner_v.as_slice())
        } else if lca == bv {
            // the sub must always be true
            let v = if a.is_compl() {
                SddPtr::new_const(false)
            } else {
                SddPtr::new_const(true)
            };
            outer_v = vec![(a, v), (a.neg(), v.neg())];
            let inner = self.tbl.sdd_slice_or_panic(b);
            (outer_v.as_slice(), inner)
        } else {
            // avoid double-negations by checking to see if `a` is already
            // negated
            let v = if a.is_compl() {
                SddPtr::new_const(false)
            } else {
                SddPtr::new_const(true)
            };
            outer_v = vec![(a, v), (a.neg(), v.neg())];
            inner_v = vec![(SddPtr::new_const(true), b.regular())];
            (outer_v.as_slice(), inner_v.as_slice())
        };


        // iterate over each prime/sum pair and do the relevant application
        // TODO: Optimize this by avoiding unnecessary conjunctions of primes
        //       specifically, from the SDD library:
        // if p1i = p2j, then p1k x p2j = false for all k<>i
        // if p1i = !p2j, then p1k x p2j = p1k for all k<>i
        // if p1i*p2j=p2j, then pik x p2j = false for all k<>i
        let mut r: Vec<(SddPtr, SddPtr)> = Vec::with_capacity(30);
        for &(ref p1, ref s1) in outer.iter() {
            let s1 = if a.is_compl() { s1.neg() } else { *s1 };
            for &(ref p2, ref s2) in inner.iter() {
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
                    self.app_cache[lca].insert((a, b), new_v.clone());
                    return new_v;
                }
                r.push((p, s));
            }
        }

        if r.len() == 0 {
            let new_v = SddPtr::new_const(false);
            self.app_cache[lca].insert((a, b), new_v.clone());
            return new_v;
        }

        // canonicalize
        r = self.compress(r);
        // trim
        if r.len() == 1 {
            let (p, s) = r[0];
            if p.is_true() {
                // the prime is true, so we can return the sub
                let new_v = s;
                self.app_cache[lca].insert((a, b), new_v.clone());
                return new_v;
            } else if s.is_true() {
                // the sub is true, so return the prime
                let new_v = p;
                self.app_cache[lca].insert((a, b), new_v.clone());
                return new_v;
            }
        }

        if r.len() == 2 {
            // check for terms of the form [(a, T), (!a, F)] => a
            let (p1, s1) = r[0];
            let (p2, s2) = r[1];
            if s1.is_true() && s2.is_false() {
                let new_v = p1;
                self.app_cache[lca].insert((a, b), new_v.clone());
                return new_v;
            } else if s1.is_false() && s2.is_true() {
                let new_v = p2;
                self.app_cache[lca].insert((a, b), new_v.clone());
                return new_v;
            }
        }

        // guarantee first sub in the first node is not complemented
        // (regular form)
        let new_v = if r[0].1.is_compl() {
            let compl_r = r.iter().map(|&(ref p, ref s)| (*p, s.neg())).collect();
            self.tbl
                .get_or_insert_sdd(&SddOr { nodes: compl_r }, lca)
                .neg()
        } else {
            self.tbl.get_or_insert_sdd(&SddOr { nodes: r }, lca)
        };
        self.app_cache[lca].insert((a, b), new_v.clone());
        // println!(
        //     "applying\n{}\n{}\n result:\n{}\n",
        //     self.print_sdd_internal(a),
        //     self.print_sdd_internal(b),
        //     self.print_sdd_internal(new_v)
        // );

        new_v
    }

    pub fn and(&mut self, a: ExternalRef, b: ExternalRef) -> ExternalRef {
        let i_a = self.external_table.into_internal(a);
        let i_b = self.external_table.into_internal(b);
        let r = self.and_rec(i_a, i_b);
        self.external_table.gen_or_inc(r)
    }


    pub fn or(&mut self, a: ExternalRef, b: ExternalRef) -> ExternalRef {
        let i_a = self.external_table.into_internal(a);
        let i_b = self.external_table.into_internal(b);
        let r = self.or_internal(i_a, i_b);
        self.external_table.gen_or_inc(r)
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
                let sl = man.tbl.sdd_slice_or_panic(ptr);
                for &(ref prime, ref sub) in sl.iter() {
                    let new_s1 = helper(man, prime.clone());
                    let new_s2 = helper(man, sub.clone());
                    doc =
                        doc.append(Doc::newline()).append(
                            (Doc::from("/\\").append(Doc::newline()).append(
                                (new_s1.append(Doc::newline()).append(new_s2)),
                            )).nest(2),
                        );
                }
                let d = Doc::from(String::from(
                    format!("{}\\/", if ptr.is_compl() { "!" } else { "" }),
                ));
                d.append(doc.nest(2))
            }
        }
        let d = helper(self, ptr);
        let mut w = Vec::new();
        d.render(10, &mut w).unwrap();
        String::from_utf8(w).unwrap()
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

    pub fn is_true(&self, a: ExternalRef) -> bool {
        let a_int = self.external_table.into_internal(a);
        a_int.is_true()
    }

    pub fn is_false(&self, a: ExternalRef) -> bool {
        let a_int = self.external_table.into_internal(a);
        a_int.is_false()
    }

    pub fn sdd_eq(&self, a: ExternalRef, b: ExternalRef) -> bool {
        let a_int = self.external_table.into_internal(a);
        let b_int = self.external_table.into_internal(b);
        self.eq(a_int, b_int)
    }

    pub fn from_cnf(&mut self, cnf: &Cnf) -> ExternalRef {
        let mut cvec: Vec<ExternalRef> = Vec::with_capacity(cnf.clauses().len());
        for lit_vec in cnf.clauses().iter() {
            assert!(lit_vec.len() > 0, "empty cnf");
            let (vlabel, val) = lit_vec[0];
            let mut sdd = self.var(vlabel, val);
            for i in 1..lit_vec.len() {;
                let (vlabel, val) = lit_vec[i];
                let var = self.var(vlabel, val);
                sdd = self.or(sdd, var);
            }
            cvec.push(sdd);
        }
        // now cvec has a list of all the clauses; collapse it down
        fn helper(vec: &[ExternalRef], man: &mut SddManager) -> Option<ExternalRef> {
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

    pub fn from_boolexpr(&mut self, expr: &BoolExpr) -> ExternalRef {
        match expr {
            &BoolExpr::Var(lbl, polarity) => {
                self.var(VarLabel::new(lbl as u64), polarity)
            }
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
