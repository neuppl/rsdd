//! Primary interface for manipulating and constructing BDDs

use manager::cache::bdd_app::*;
use std::fmt::Debug;
use manager::var_order::VarOrder;
use repr::var_label::VarLabel;
use repr::bdd::*;
use repr::cnf::Cnf;
use repr::boolexpr::BoolExpr;
use std::collections::{HashMap, HashSet};
use backing_store::BackingCacheStats;
use backing_store::bdd_table::BddTable;
use num::traits::Num;

pub struct BddManager {
    compute_table: BddTable,
    apply_table: BddApplyTable,
}

impl BddManager {
    /// Make a BDD manager with a default variable ordering
    pub fn new_default_order(num_vars: usize) -> BddManager {
        let default_order = VarOrder::linear_order(num_vars);
        BddManager::new(default_order)
    }

    pub fn new(order: VarOrder) -> BddManager {
        let len = order.len();
        BddManager {
            compute_table: BddTable::new(order),
            apply_table: BddApplyTable::new(len),
        }
    }

    pub fn get_order(&self) -> &VarOrder {
        self.compute_table.order()
    }

    fn deref(&self, ptr: BddPtr) -> Bdd {
        self.compute_table.deref(ptr)
    }

    /// Fetch the BDD pointed to by the low-node of `ptr`, panics on constant
    // BDDs
    fn low(&self, ptr: BddPtr) -> BddPtr {
        let b = self.deref(ptr).into_node();
        b.low
    }

    /// Fetch the BDD pointed to by the high-node of `ptr`, panics on constant
    /// BDDs
    fn high(&self, ptr: BddPtr) -> BddPtr {
        let b = self.deref(ptr).into_node();
        b.high
    }

    /// Push a variable onto the stack
    pub fn var(&mut self, lbl: VarLabel, is_true: bool) -> BddPtr {
        let bdd = Bdd::new_node(BddPtr::false_node(), BddPtr::true_node(), lbl);
        let r = self.get_or_insert(bdd);
        if is_true { r } else { r.neg() }
    }

    pub fn true_ptr(&self) -> BddPtr {
        BddPtr::true_node()
    }

    pub fn false_ptr(&self) -> BddPtr {
        BddPtr::false_node()
    }

    pub fn is_true(&self, ptr: BddPtr) -> bool {
        ptr.is_true()
    }

    pub fn is_false(&self, ptr: BddPtr) -> bool {
        ptr.is_false()
    }

    fn get_or_insert(&mut self, bdd: Bdd) -> BddPtr {
        self.compute_table.get_or_insert(bdd)
    }

    pub fn print_bdd(&self, ptr: BddPtr) -> String {
        use repr::bdd::PointerType::*;
        fn print_bdd_helper(t: &BddManager, ptr: BddPtr) -> String {
            match ptr.ptr_type() {
                PtrTrue => String::from("T"),
                PtrFalse => String::from("T"),
                PtrNode => {
                    let l_p = t.low(ptr);
                    let h_p = t.high(ptr);
                    let l_s = print_bdd_helper(t, l_p);
                    let r_s = print_bdd_helper(t, h_p);
                    format!(
                        "({}, {}{}, {}{})",
                        ptr.var(),
                        if l_p.is_compl() { "!" } else { "" },
                        l_s,
                        if h_p.is_compl() { "!" } else { "" },
                        r_s
                    )
                }
            }
        }
        let s = print_bdd_helper(self, ptr);
        format!("{}{}", if ptr.is_compl() { "!" } else { "" }, s)
    }


    pub fn negate(&mut self, ptr: BddPtr) -> BddPtr {
        ptr.neg()
    }

    pub fn print_bdd_lbl(&self, ptr: BddPtr, map: &HashMap<VarLabel, VarLabel>) -> String {
        use repr::bdd::PointerType::*;
        fn print_bdd_helper(
            t: &BddManager,
            ptr: BddPtr,
            map: &HashMap<VarLabel, VarLabel>,
        ) -> String {
            match ptr.ptr_type() {
                PtrTrue => String::from("T"),
                PtrFalse => String::from("T"),
                PtrNode => {
                    let l_p = t.low(ptr);
                    let h_p = t.high(ptr);
                    let l_s = print_bdd_helper(t, l_p, map);
                    let r_s = print_bdd_helper(t, h_p, map);
                    format!(
                        "({:?}, {}{}, {}{})",
                        map.get(&ptr.label()).unwrap().value(),
                        if l_p.is_compl() { "!" } else { "" },
                        l_s,
                        if h_p.is_compl() { "!" } else { "" },
                        r_s
                    )
                }
            }
        }
        let s = print_bdd_helper(self, ptr, map);
        format!("{}{}", if ptr.is_compl() { "!" } else { "" }, s)
    }



    /// true if `a` represents a variable (both high and low are constant)
    #[inline]
    pub fn is_var(&self, ptr: BddPtr) -> bool {
        match ptr.ptr_type() {
            PointerType::PtrNode => {
                let b = self.compute_table.deref(ptr).into_node();
                b.low.is_const() && b.high.is_const()
            }
            _ => false,
        }
    }

    pub fn and(&mut self, f: BddPtr, g: BddPtr) -> BddPtr {
        // base case
        let reg_f = f.regular();
        let reg_g = g.regular();
        if reg_f == reg_g {
            if f == g {
                return f;
            } else {
                return BddPtr::false_node();
            }
        }
        if reg_f.is_true() {
            if f.is_true() {
                return g;
            } else {
                return f;
            }
        }
        if reg_g.is_true() {
            if g.is_true() {
                return f;
            } else {
                return g;
            }
        }

        // now, both of the nodes are not constant
        // normalize the nodes to increase cache efficiency
        let (f, g, reg_f, _) = if reg_f < reg_g {
            (f, g, reg_f, reg_g)
        } else {
            (g, f, reg_g, reg_f)
        };
        // check the cache
        match self.apply_table.get(f, g) {
            Some(v) => {
                return v;
            }
            None => {}
        };

        // now we know that these are nodes, compute the cofactors
        let topf = self.get_order().get(f.label());
        let topg = self.get_order().get(g.label());
        let index; // will hold the top variable
        let mut fv;
        let mut gv;
        let mut fnv;
        let mut gnv;
        if topf <= topg {
            index = f.label();
            fv = self.high(reg_f);
            fnv = self.low(reg_f);
            if f.is_compl() {
                fv = fv.neg();
                fnv = fnv.neg();
            }
        } else {
            index = g.label();
            fv = f;
            fnv = f;
        }

        if topg <= topf {
            gv = self.high(g);
            gnv = self.low(g);
            if g.is_compl() {
                gv = gv.neg();
                gnv = gnv.neg();
            }
        } else {
            gv = g;
            gnv = g;
        }

        // now recurse
        let new_h = self.and(fv, gv);
        let new_l = self.and(fnv, gnv);

        // now normalize the result
        if new_h == new_l {
            return new_h;
        } else {
            let r = if new_h.is_compl() {
                let bdd = Bdd::new_node(new_l.neg(), new_h.neg(), index);
                self.get_or_insert(bdd).neg()
            } else {
                let bdd = Bdd::new_node(new_l, new_h, index);
                self.get_or_insert(bdd)
            };
            self.apply_table.insert(f, g, r);
            return r;
        }
    }

    pub fn or(&mut self, f: BddPtr, g: BddPtr) -> BddPtr {
        self.and(f.neg(), g.neg()).neg()
    }

    /// evaluates the top element of the data stack on the values found in
    /// `vars`
    pub fn eval_bdd(&self, bdd: BddPtr, assgn: &HashMap<VarLabel, bool>) -> bool {
        fn eval_bdd_helper(man: &BddManager, ptr: BddPtr, assgn: &HashMap<VarLabel, bool>) -> bool {
            if ptr.is_true() {
                return true;
            } else if ptr.is_false() {
                return false;
            }
            let bdd = man.deref(ptr);
            let r = match bdd {
                Bdd::BddTrue => true,
                Bdd::BddFalse => false,
                Bdd::Node(n) => {
                    let value = assgn.get(&n.var).unwrap();
                    if *value {
                        eval_bdd_helper(man, n.high, assgn)
                    } else {
                        eval_bdd_helper(man, n.low, assgn)
                    }
                }
            };
            if ptr.is_compl() { !r } else { r }
        }
        eval_bdd_helper(self, bdd, assgn)
    }

    /// Returns true if `a` == `b`
    pub fn eq_bdd(&self, a: BddPtr, b: BddPtr) -> bool {
        // the magic of BDDs!
        a == b
    }

    // pub fn get_apply_cache_stats(&self) -> Vec<ApplyCacheStats> {
    //     self.apply_table.get_stats()
    // }

    pub fn get_backing_store_stats(&self) -> BackingCacheStats {
        self.compute_table.get_stats().clone()
    }

    pub fn num_nodes(&self) -> usize {
        self.compute_table.num_nodes()
    }

    fn count_nodes_h(&self, ptr: BddPtr, set: &mut HashSet<BddPtr>) -> usize {
        if set.contains(&ptr) || ptr.is_const() {
            return 0;
        }
        set.insert(ptr);
        match ptr.ptr_type() {
            PointerType::PtrFalse => 1,
            PointerType::PtrTrue => 1,
            PointerType::PtrNode => {
                let n = self.deref(ptr).into_node();
                let c_l = self.count_nodes_h(n.low, set);
                let c_r = self.count_nodes_h(n.high, set);
                return c_l + c_r + 1;
            }
        }
    }

    pub fn count_nodes(&self, ptr: BddPtr) -> usize {
        self.count_nodes_h(ptr, &mut HashSet::new())
    }

    /// a helper function for WMC which tracks the current variable level for
    /// on-the-fly smoothing. Returns a pair: the first element is the sum of
    /// the node, and the second element is the expected parent of that node; in
    /// the case of the node being the top variable, then `None` is returned
    fn wmc_helper<T: Num + Clone + Debug>(
        &self,
        ptr: BddPtr,
        lbl_to_v: &[(T, T)],
        zero: &T,
        one: &T,
    ) -> (T, Option<VarLabel>) {
        use repr::bdd::PointerType;
        match ptr.ptr_type() {
            PointerType::PtrTrue => (
                one.clone(),
                Some(self.get_order().last_var()),
            ),
            PointerType::PtrFalse => (
                zero.clone(),
                Some(self.get_order().last_var())
            ),
            PointerType::PtrNode => {
                let order = self.get_order();
                let bdd = self.deref(ptr).into_node();
                let (low, high) = if ptr.is_compl() {
                    (bdd.low.neg(), bdd.high.neg() )
                } else {
                    (bdd.low, bdd.high)
                };
                let (mut low_v, low_lvl_op) = self.wmc_helper(low, lbl_to_v, zero, one);
                let (mut high_v, high_lvl_op) = self.wmc_helper(high, lbl_to_v, zero, one);
                let mut low_lvl = low_lvl_op.unwrap();
                let mut high_lvl = high_lvl_op.unwrap();
                // smooth low
                while order.lt(ptr.label(), low_lvl) {
                    let (low_factor, high_factor) = lbl_to_v[low_lvl.value() as usize].clone();
                    low_v = (low_v.clone() * low_factor) + (low_v * high_factor);
                    low_lvl = order.above(low_lvl).unwrap();
                }
                // smooth high
                while order.lt(ptr.label(), high_lvl) {
                    let (low_factor, high_factor) = lbl_to_v[high_lvl.value() as usize].clone();
                    high_v = (high_v.clone() * low_factor) + (high_v * high_factor);
                    high_lvl = order.above(high_lvl).unwrap();
                }
                // compute new
                let (low_factor, high_factor) = lbl_to_v[bdd.var.value() as usize].clone();
                let res = (low_v * low_factor.clone()) + (high_v * high_factor.clone());
                if order.get(ptr.label()) == 0 {
                    (res, None)
                } else {
                    (res, Some(order.above(ptr.label()).unwrap()))
                }
            }
        }
    }
    /// Weighted-model count. `v` is a vector of pairs of numeric values, with
    /// the first value being the low value and the second being the high value.
    /// The vector is an associative array, keyed by the variable label.
    pub fn wmc<T: Num + Clone + Debug>(
        &self,
        ptr: BddPtr,
        lbl_to_v: &[(T, T)],
        zero: &T,
        one: &T,
    ) -> T {
        // call wmc_helper and smooth the result
        let (mut v, lvl_op) = self.wmc_helper(ptr, lbl_to_v, zero, one);
        if lvl_op.is_none() {
            // no smoothing required
            v
        } else {
            // need to smooth
            let mut lvl = lvl_op;
            let order = self.get_order();
            while lvl.is_some() {
                let (low_factor, high_factor) = lbl_to_v[lvl.unwrap().value() as usize].clone();
                v = (v.clone() * low_factor) + (v * high_factor);
                lvl = order.above(lvl.unwrap());
            }
            v
        }
    }

    pub fn from_cnf(&mut self, cnf: &Cnf) -> BddPtr {
        let mut cvec: Vec<BddPtr> = Vec::with_capacity(cnf.clauses().len());
        for lit_vec in cnf.clauses().iter() {
            assert!(lit_vec.len() > 0, "empty cnf");
            let (vlabel, val) = lit_vec[0];
            let mut bdd = self.var(vlabel, val);
            for i in 1..lit_vec.len() {;
                let (vlabel, val) = lit_vec[i];
                let var = self.var(vlabel, val);
                bdd = self.or(bdd, var);
            }
            cvec.push(bdd);
        }
        // now cvec has a list of all the clauses; collapse it down
        fn helper(vec: &[BddPtr], man: &mut BddManager) -> Option<BddPtr> {
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

    pub fn from_boolexpr(&mut self, expr: &BoolExpr) -> BddPtr {
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

// check that (a \/ b) /\ a === a
#[test]
fn simple_equality() {
    let mut man = BddManager::new_default_order(3);
    let v1 = man.var(VarLabel::new(0), true);
    let v2 = man.var(VarLabel::new(1), true);
    let r1 = man.or(v1, v2);
    let r2 = man.and(r1, v1);
    assert!(
        man.eq_bdd(v1, r2),
        "Not eq:\n {}\n{}",
        man.print_bdd(v1),
        man.print_bdd(r2)
    );
}

#[test]
fn test_wmc() {
    let mut man = BddManager::new_default_order(2);
    let v1 = man.var(VarLabel::new(0), true);
    let v2 = man.var(VarLabel::new(1), true);
    let r1 = man.or(v1, v2);
    let weights : Vec<(usize, usize)> = vec![(2, 3), (5, 7)];
    let wmc = man.wmc(r1, &weights, &0, &1);
    assert_eq!(wmc, 50);
}

#[test]
fn test_wmc_smooth() {
    let mut man = BddManager::new_default_order(3);
    let v1 = man.var(VarLabel::new(0), true);
    let v2 = man.var(VarLabel::new(2), true);
    let r1 = man.or(v1, v2);
    let weights : Vec<(usize, usize)> = vec![(2, 3), (5, 7), (11, 13)];
    let wmc = man.wmc(r1, &weights, &0, &1);
    assert_eq!(wmc, 1176);
}

#[test]
fn test_wmc_smooth2() {
    let mut man = BddManager::new_default_order(3);
    let weights : Vec<(usize, usize)> = vec![(2, 3), (5, 7), (11, 13)];
    let r1 = BddPtr::true_node();
    let wmc = man.wmc(r1, &weights, &0, &1);
    assert_eq!(wmc, 1440);
}

