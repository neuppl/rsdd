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
use manager::cache::lru::ApplyCacheStats;
use backing_store::bdd_table_robinhood::BddTable;
use num::traits::Num;

#[macro_use]
use maplit::*;

use crate::backing_store;
use crate::backing_store::bdd_table_robinhood::TraverseTable;


/// Weighted model counting parameters for a BDD. It primarily is a storage for
/// the weight on each variable.
#[derive(Debug)]
pub struct BddWmc<T: Num + Clone + Debug + Copy> {
    pub zero: T,
    pub one: T,
    /// a vector which maps variable labels to `(low, high)`
    /// valuations.
    var_to_val: HashMap<VarLabel, (T, T)>,
}

impl<T: Num + Clone + Debug + Copy> BddWmc<T> {
    /// Generates a new `BddWmc` with a default `var_to_val`; it is private because we
    /// do not want to expose the structure of the associative array
    pub fn new_with_default(zero: T, one: T, var_to_val: HashMap<VarLabel, (T, T)>) -> BddWmc<T> {
        BddWmc {
            zero: zero,
            one: one,
            var_to_val: var_to_val,
        }
    }

    /// Generate a new `BddWmc` with no associations
    pub fn new(zero: T, one: T) -> BddWmc<T> {
        BddWmc {
            zero: zero,
            one: one,
            var_to_val: HashMap::new(),
        }
    }

    /// Sets the weight of a literal
    pub fn set_weight(&mut self, idx: VarLabel, low: T, high: T) -> () {
        self.var_to_val.insert(idx, (low, high));
    }
}

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

    /// Generate a new variable which was not in the original order. Places the
    /// new variable at the end of the current order. Returns the label of the
    /// new variable
    pub fn new_var(&mut self) -> VarLabel {
        self.apply_table.new_last();
        self.compute_table.new_last()
    }

    pub fn get_order(&self) -> &VarOrder {
        self.compute_table.order()
    }

    fn deref(&self, ptr: BddPtr) -> Bdd {
        self.compute_table.deref(ptr)
    }

    /// Fetch the BDD pointed to by the low-node of `ptr`, panics on constant
    // BDDs
    pub fn low(&self, ptr: BddPtr) -> BddPtr {
        assert!(!ptr.is_const());
        let b = self.deref(ptr).into_node();
        b.low
    }

    /// Fetch the BDD pointed to by the high-node of `ptr`, panics on constant
    /// BDDs
    pub fn high(&self, ptr: BddPtr) -> BddPtr {
        assert!(!ptr.is_const());
        let b = self.deref(ptr).into_node();
        b.high
    }

    pub fn var(&mut self, lbl: VarLabel, is_true: bool) -> BddPtr {
        let bdd = BddNode::new(BddPtr::false_node(), BddPtr::true_node(), lbl);
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

    pub fn topvar(&self, ptr: BddPtr) -> VarLabel {
        let bdd = self.deref(ptr).into_node();
        bdd.var
    }

    /// normalizes and fetches a node from the store
    fn get_or_insert(&mut self, bdd: BddNode) -> BddPtr {
        if bdd.high.is_compl() {
            let bdd = Bdd::new_node(bdd.low.neg(), bdd.high.neg(), bdd.var);
            self.compute_table.get_or_insert(bdd).neg()
        } else {
            let bdd = Bdd::new_node(bdd.low, bdd.high, bdd.var);
            self.compute_table.get_or_insert(bdd)
        }
    }

    pub fn print_bdd(&self, ptr: BddPtr) -> String {
        use repr::bdd::PointerType::*;
        fn print_bdd_helper(t: &BddManager, ptr: BddPtr) -> String {
            match ptr.ptr_type() {
                PtrTrue => String::from("T"),
                PtrFalse => String::from("F"),
                PtrNode => {
                    let l_p = if ptr.is_compl() { t.low(ptr).neg() } else { t.low(ptr) };
                    let h_p = if ptr.is_compl() { t.high(ptr).neg() } else { t.high(ptr) };
                    let l_s = print_bdd_helper(t, l_p);
                    let h_s = print_bdd_helper(t, h_p);
                    format!("({}, {}, {})", ptr.var(), h_s, l_s)
                }
            }
        }
        print_bdd_helper(self, ptr)
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

    /// Compose `g` into `f` by substituting for `lbl`
    pub fn compose(&mut self, f: BddPtr, lbl: VarLabel, g: BddPtr) -> BddPtr {
        let var = self.var(lbl, true);
        let iff = self.iff(var, g);
        let a = self.and(iff, f);
        self.exists(a, lbl)
    }

    /// true if `a` represents a variable (both high and low are constant)
    pub fn is_var(&self, ptr: BddPtr) -> bool {
        match ptr.ptr_type() {
            PointerType::PtrNode => {
                let b = self.compute_table.deref(ptr).into_node();
                b.low.is_const() && b.high.is_const()
            }
            _ => false,
        }
    }

    // condition a BDD *only* if the top variable is `v`; used in `ite`
    fn condition_essential(&self, f: BddPtr, lbl: VarLabel, v: bool) -> BddPtr {
        if f.is_const() || f.var() != lbl.value() { return f };
        let r = if v { 
            self.high(f) 
        } else { 
            self.low(f) 
        };
        if f.is_compl() {r.neg()} else {r} 
    }

    fn ite_helper(&mut self, f: BddPtr, g: BddPtr, h: BddPtr) -> BddPtr {
        // standardize
        // println!("ite: if {} then {} else {}", self.print_bdd(f), self.print_bdd(g), self.print_bdd(h));
        // See pgs. 115-117 of "Algorithms and Data Structures in VLSI Design"
        // first, introduce constants if possible
        // let (f, g, h) = match (f, g, h) {
        //     (f, g, h) if g == h => (f, BddPtr::true_node(), g),
        //     (f, g, h) if f == h => (f, g, BddPtr::false_node()),
        //     (f, g, h) if f == h.neg() => (f, g, BddPtr::true_node()),
        //     (f, g, h) if f == g.neg() => (f, BddPtr::false_node(), g),
        //     _ => (f, g, h)
        // };

        // attempt a base case
        match (f, g, h) {
            (_, g, h) if g.is_false() && h.is_false() => return BddPtr::false_node(),
            (_, g, h) if g.is_true() && h.is_true() => return BddPtr::true_node(),
            (f, g, h) if g.is_true() && h.is_false() => return f,
            (f, g, h) if g.is_false() && h.is_true() => return f.neg(),
            (f, g, _) if f.is_true() => return g,
            (f, _, h) if f.is_false() => return h,
            (_, g, h) if g == h => return g,
            _ => ()
        };

        // attempt to place the variable that comes first in the order as f
        let (f, g, h) = match (f, g, h) {
            (f, g, h) if g.is_true() && self.get_order().lt(h.label(), f.label()) => { (h, g, f) },
            (f, g, h) if h.is_false() && self.get_order().lt(g.label(), f.label()) => { (g, f, h) },
            (f, g, h) if h.is_true() && self.get_order().lt(g.label(), f.label()) => { (g.neg(), f.neg(), h) },
            (f, g, h) if g.is_false() && self.get_order().lt(h.label(), f.label()) => { (h.neg(), g, f.neg()) },
            (f, g, h) if g == h && self.get_order().lt(g.label(), f.label()) => { (g, f, f.neg()) },
            _ => (f, g, h)
        };

        // ok now it is normalized, see if this is in the apply table
        match self.apply_table.get(f, g, h) {
            Some(v) => return v,
            None => ()
        };


        // ok the work! 
        // find the first essential variable for f, g, or h
        let lbl = self.get_order().first_essential(f, g, h);
        let fx = self.condition_essential(f, lbl, true);
        let gx = self.condition_essential(g, lbl, true);
        let hx = self.condition_essential(h, lbl, true);
        let fxn = self.condition_essential(f, lbl, false);
        let gxn = self.condition_essential(g, lbl, false);
        let hxn = self.condition_essential(h, lbl, false);
        let T = self.ite(fx, gx, hx);
        let F = self.ite(fxn, gxn, hxn);

        // println!("tbdd: {}, fbdd: {}", self.print_bdd(T), self.print_bdd(F));
        if T == F { return T };

        // now we have a new BDD
        let node = BddNode { 
            low: F,
            high: T,
            var: lbl
        };
        let r = self.get_or_insert(node);
        self.apply_table.insert(f, g, h, r);
        r
    }

    /// if f then g else h
    pub fn ite(&mut self, f: BddPtr, g: BddPtr, h: BddPtr) -> BddPtr {
        let r = self.ite_helper(f, g, h);
        r
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
        match self.apply_table.get(f, g, BddPtr::false_node()) {
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
            let n = BddNode {
                low: new_l,
                high: new_h,
                var: index,
            };
            let r = self.get_or_insert(n);
            self.apply_table.insert(f, g, BddPtr::false_node(), r);
            return r;
        }
    }

    /// Compute the Boolean function `f || g`
    pub fn or(&mut self, f: BddPtr, g: BddPtr) -> BddPtr {
        self.and(f.neg(), g.neg()).neg()
    }

    /// disjoins a list of BDDs
    pub fn or_lst(&mut self, f : &[BddPtr]) -> BddPtr {
        let mut cur_bdd = self.false_ptr();
        for &itm in f {
            cur_bdd = self.or(cur_bdd, itm);
        }
        cur_bdd
    }

    /// disjoins a list of BDDs
    pub fn and_lst(&mut self, f : &[BddPtr]) -> BddPtr {
        let mut cur_bdd = self.true_ptr();
        for &itm in f {
            cur_bdd = self.and(cur_bdd, itm);
        }
        cur_bdd
    }

    /// Compute the Boolean function `f iff g`
    pub fn iff(&mut self, f: BddPtr, g: BddPtr) -> BddPtr {
        self.ite(f, g, g.neg())
    }

    pub fn xor(&mut self, f: BddPtr, g: BddPtr) -> BddPtr {
        self.ite(f, g.neg(), g)
    }


    /// An abstract transformation on a BDD which applies a transformation `f`
    /// to all nodes for a particular variable
    fn map_var(&mut self,
               bdd: BddPtr,
               lbl: VarLabel,
               seen: &mut HashSet<BddPtr>,
               f:&Fn(&mut BddManager, BddPtr) -> BddPtr) -> BddPtr {
        if seen.contains(&bdd) {
            return bdd;
        }
        if self.get_order().lt(lbl, bdd.label()) || bdd.is_const() {
            // we passed the variable in the order, we will never find it
            bdd
        } else if bdd.label() == lbl {
            f(self, bdd)
        } else {
            // recurse on the children
            let n = self.deref(bdd).into_node();
            let l = self.map_var(n.low, lbl, seen, f);
            let h = self.map_var(n.high, lbl, seen, f);
            let res = if l != n.low || h != n.high {
                // cache and return the new BDD
                let new_bdd = BddNode {
                    low: l,
                    high: h,
                    var: bdd.label(),
                };
                let r = self.get_or_insert(new_bdd);
                if bdd.is_compl() { r.neg() } else { r }
            } else {
                // nothing changed
                bdd
            };
            seen.insert(res);
            res
        }
    }

    fn cond_helper(&mut self, bdd: BddPtr, lbl: VarLabel,
                   value: bool,
                   seen: &mut HashSet<BddPtr>) -> BddPtr {
        if self.get_order().lt(lbl, bdd.label()) || bdd.is_const() {
            // we passed the variable in the order, we will never find it
            bdd
        } else if bdd.label() == lbl {
            let node = self.deref(bdd).into_node();
            let r = if value {node.high} else {node.low};
            if bdd.is_compl() { r.neg() } else { r }
        } else {
            // recurse on the children
            let n = self.deref(bdd).into_node();
            let l = self.cond_helper(n.low, lbl, value, seen);
            let h = self.cond_helper(n.high, lbl, value, seen);
            if l == h {
                if bdd.is_compl() {
                    return l.neg();
                } else {
                    return l;
                };
            };
            let res = if l != n.low || h != n.high {
                // cache and return the new BDD
                let new_bdd = BddNode {
                    low: l,
                    high: h,
                    var: bdd.label(),
                };
                let r = self.get_or_insert(new_bdd);
                if bdd.is_compl() { r.neg() } else { r }
            } else {
                // nothing changed
                bdd
            };
            seen.insert(res);
            res
        }
    }

    /// Compute the Boolean function `f | var = value`
    pub fn condition(&mut self, bdd: BddPtr, lbl: VarLabel, value: bool) -> BddPtr {
        self.cond_helper(bdd, lbl, value, &mut HashSet::new())
    }

    /// Existentially quantifies out the variable `lbl` from `f`
    pub fn exists(&mut self, bdd: BddPtr, lbl: VarLabel) -> BddPtr {
        // TODO this can be optimized by specializing it
        let v1 = self.condition(bdd, lbl, true);
        let v2 = self.condition(bdd, lbl, false);
        self.or(v1, v2)
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

    pub fn get_backing_store_stats(&self) -> BackingCacheStats {
        self.compute_table.get_stats().clone()
    }

    fn count_nodes_h(&self, ptr: BddPtr, set: &mut HashSet<BddPtr>) -> usize {
        if set.contains(&ptr) {
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



    /// The total number of nodes allocated by the manager
    pub fn total_nodes(&self) -> usize {
        self.compute_table.num_nodes()
    }

    // fn count_nodes_h(&self, ptr: BddPtr, set: &mut TraverseTable<()>) -> usize {
    //     if !set.get(&ptr).is_none() || ptr.is_const() {
    //         return 0;
    //     }
    //     set.set(&ptr, ());
    //     match ptr.ptr_type() {
    //         PointerType::PtrFalse => 1,
    //         PointerType::PtrTrue => 1,
    //         PointerType::PtrNode => {
    //             let n = self.deref(ptr).into_node();
    //             let c_l = self.count_nodes_h(n.low, set);
    //             let c_r = self.count_nodes_h(n.high, set);
    //             return c_l + c_r + 1;
    //         }
    //     }
    // }

    // /// Count the number of nodes in a BDD
    // pub fn count_nodes(&self, ptr: BddPtr) -> usize {
    //     self.count_nodes_h(ptr, &mut TraverseTable::new(&self.compute_table))
    // }

    /// a helper function for WMC which tracks the current variable level for
    /// on-the-fly smoothing. Returns a pair: the first element is the sum of
    /// the node, and the second element is the expected parent of that node; in
    /// the case of the node being the top variable, then `None` is returned
    fn wmc_helper<T: Num + Clone + Debug + Copy>(
        &self,
        ptr: BddPtr,
        wmc: &BddWmc<T>,
        compute_table: &mut TraverseTable<(T, Option<VarLabel>)>
    ) -> (T, Option<VarLabel>) {
        match ptr.ptr_type() {
            PointerType::PtrTrue => (wmc.one.clone(), Some(self.get_order().last_var())),
            PointerType::PtrFalse => (wmc.zero.clone(), Some(self.get_order().last_var())),
            PointerType::PtrNode => {
                match compute_table.get(&ptr) {
                    Some(a) => return *a,
                    None => ()
                };

                let order = self.get_order();
                let bdd = self.deref(ptr).into_node();
                let (low, high) = if ptr.is_compl() {
                    (bdd.low.neg(), bdd.high.neg())
                } else {
                    (bdd.low, bdd.high)
                };
                let (mut low_v, low_lvl_op) = self.wmc_helper(low, wmc, compute_table);
                let (mut high_v, high_lvl_op) = self.wmc_helper(high, wmc, compute_table);
                let mut low_lvl = low_lvl_op.unwrap();
                let mut high_lvl = high_lvl_op.unwrap();
                // smooth low
                while order.lt(ptr.label(), low_lvl) {
                    let (low_factor, high_factor) = wmc.var_to_val.get(&VarLabel::new(low_lvl.value())).unwrap();
                    low_v = (low_v.clone() * (*low_factor)) + (low_v * (*high_factor));
                    low_lvl = order.above(low_lvl).unwrap();
                }
                // smooth high
                while order.lt(ptr.label(), high_lvl) {
                    let (low_factor, high_factor) = wmc.var_to_val.get(&VarLabel::new(high_lvl.value())).unwrap();
                    high_v = (high_v.clone() * (*low_factor)) + (high_v * (*high_factor));
                    high_lvl = order.above(high_lvl).unwrap();
                }
                // compute new
                let (low_factor, high_factor) = wmc.var_to_val.get(&VarLabel::new(bdd.var.value())).unwrap();
                let res = (low_v * low_factor.clone()) + (high_v * high_factor.clone());
                if order.get(ptr.label()) == 0 {
                    (res, None)
                } else {
                    let r = (res, Some(order.above(ptr.label()).unwrap()));
                    compute_table.set(&ptr, r);
                    r
                }
            }
        }
    }

    /// Weighted-model count.
    pub fn wmc<T: Num + Clone + Debug + Copy>(&self, ptr: BddPtr, params: &BddWmc<T>) -> T {
        // call wmc_helper and smooth the result
        let mut tbl = TraverseTable::new(&self.compute_table);
        let (mut v, lvl_op) = self.wmc_helper(ptr, params, &mut tbl);
        if lvl_op.is_none() {
            // no smoothing required
            v
        } else {
            // need to smooth
            let mut lvl = lvl_op;
            let order = self.get_order();
            while lvl.is_some() {
                let (low_factor, high_factor) =
                    params.var_to_val.get(&VarLabel::new(lvl.unwrap().value())).unwrap();
                v = (v.clone() * (*low_factor)) + (v * (*high_factor));
                lvl = order.above(lvl.unwrap());
            }
            v
        }
    }

    pub fn from_cnf(&mut self, cnf: &Cnf) -> BddPtr {
        let mut cvec: Vec<BddPtr> = Vec::with_capacity(cnf.clauses().len());
        for lit_vec in cnf.clauses().iter() {
            assert!(lit_vec.len() > 0, "empty cnf");
            let (vlabel, val) = (lit_vec[0].get_label(), lit_vec[0].get_polarity());
            let mut bdd = self.var(vlabel, val);
            for i in 1..lit_vec.len() {
                let (vlabel, val) = (lit_vec[i].get_label(), lit_vec[i].get_polarity());
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

    pub fn print_stats(&self) -> () {
        let compute_stats = self.get_backing_store_stats();
        let apply_stats = self.apply_table.get_stats();
        println!("BDD Manager Stats\nCompute hit count: {}\nCompute lookup count: {}\nCompute total elements: {}\nCompute average offset: {}\nApply lookup: {}\nApply miss: {}\nApply evictions: {}", 
        compute_stats.hit_count, compute_stats.lookup_count, compute_stats.num_elements, compute_stats.avg_offset, apply_stats.lookup_count, apply_stats.miss_count, apply_stats.conflict_count);
    }

    pub fn from_boolexpr(&mut self, expr: &BoolExpr) -> BddPtr {
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
fn simple_ite1() {
    let mut man = BddManager::new_default_order(3);
    let v1 = man.var(VarLabel::new(0), true);
    let v2 = man.var(VarLabel::new(1), true);
    let r1 = man.or(v1, v2);
    let r2 = man.ite(r1, v1, BddPtr::false_node());
    assert!(
        man.eq_bdd(v1, r2),
        "Not eq:\n {}\n{}",
        man.print_bdd(v1),
        man.print_bdd(r2)
    );
}

#[test]
fn test_newvar() {
    let mut man = BddManager::new_default_order(0);
    let l1 = man.new_var();
    let l2 = man.new_var();
    let v1 = man.var(l1, true);
    let v2 = man.var(l2, true);
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
    let weights = hashmap!{VarLabel::new(0) => (2,3),
                           VarLabel::new(1) => (5,7)};
    let params = BddWmc::new_with_default(0, 1, weights);
    let wmc = man.wmc(r1, &params);
    assert_eq!(wmc, 50);
}

#[test]
fn test_wmc_smooth() {
    let mut man = BddManager::new_default_order(3);
    let v1 = man.var(VarLabel::new(0), true);
    let v2 = man.var(VarLabel::new(2), true);
    let r1 = man.or(v1, v2);
    let weights = hashmap!{
        VarLabel::new(0) => (2,3),
        VarLabel::new(1) => (5,7),
        VarLabel::new(2) => (11,13)};
    let params = BddWmc::new_with_default(0, 1, weights);
    let wmc = man.wmc(r1, &params);
    assert_eq!(wmc, 1176);
}

#[test]
fn test_wmc_smooth2() {
    let man = BddManager::new_default_order(3);
    let r1 = BddPtr::true_node();
    let weights = hashmap!{
        VarLabel::new(0) => (2,3),
        VarLabel::new(1) => (5,7),
        VarLabel::new(2) => (11,13)};
    let params = BddWmc::new_with_default(0, 1, weights);
    let wmc = man.wmc(r1, &params);
    assert_eq!(wmc, 1440);
}

#[test]
fn test_condition() {
    let mut man = BddManager::new_default_order(3);
    let v1 = man.var(VarLabel::new(0), true);
    let v2 = man.var(VarLabel::new(1), true);
    let r1 = man.or(v1, v2);
    let r3 = man.condition(r1, VarLabel::new(1), false);
    assert!(man.eq_bdd(r3, v1));
}

#[test]
fn test_condition_compl() {
    let mut man = BddManager::new_default_order(3);
    let v1 = man.var(VarLabel::new(0), false);
    let v2 = man.var(VarLabel::new(1), false);
    let r1 = man.and(v1, v2);
    let r3 = man.condition(r1, VarLabel::new(1), false);
    assert!(
        man.eq_bdd(r3, v1),
        "Not eq:\nOne: {}\nTwo: {}",
        man.print_bdd(r3),
        man.print_bdd(v1)
    );
}




#[test]
fn test_exist() {
    let mut man = BddManager::new_default_order(3);
    // 1 /\ 2 /\ 3
    let v1 = man.var(VarLabel::new(0), true);
    let v2 = man.var(VarLabel::new(1), true);
    let v3 = man.var(VarLabel::new(2), true);
    let a1 = man.and(v1, v2);
    let r1 = man.and(a1, v3);
    let r_expected = man.and(v1, v3);
    let res = man.exists(r1, VarLabel::new(1));
    assert!(
        man.eq_bdd(r_expected, res),
        "Got:\nOne: {}\nExpected: {}",
        man.print_bdd(res),
        man.print_bdd(r_expected)
    );
}

#[test]
fn test_exist_compl() {
    let mut man = BddManager::new_default_order(3);
    // 1 /\ 2 /\ 3
    let v1 = man.var(VarLabel::new(0), false);
    let v2 = man.var(VarLabel::new(1), false);
    let v3 = man.var(VarLabel::new(2), false);
    let a1 = man.and(v1, v2);
    let r1 = man.and(a1, v3);
    let r_expected = man.and(v1, v3);
    let res = man.exists(r1, VarLabel::new(1));
    // let res = r1;
    assert!(
        man.eq_bdd(r_expected, res),
        "Got:\n: {}\nExpected: {}",
        man.print_bdd(res),
        man.print_bdd(r_expected)
    );
}

#[test]
fn test_compose() {
    let mut man = BddManager::new_default_order(3);
    // 1 /\ 2 /\ 3
    let v0 = man.var(VarLabel::new(0), true);
    let v1 = man.var(VarLabel::new(1), true);
    let v2 = man.var(VarLabel::new(2), true);
    let v0_and_v1 = man.and(v0, v1);
    let v0_and_v2 = man.and(v0, v2);
    let res = man.compose(v0_and_v1, VarLabel::new(1), v2);
    assert!(
        man.eq_bdd(res, v0_and_v2),
        "\nGot: {}\nExpected: {}",
        man.print_bdd(res),
        man.print_bdd(v0_and_v2)
    );
}

#[test]
fn test_new_var() {
    let mut man = BddManager::new_default_order(0);
    let vlbl1 = man.new_var();
    let vlbl2 = man.new_var();
    let v1 = man.var(vlbl1, false);
    let v2 = man.var(vlbl2, false);
    let r1 = man.and(v1, v2);
    let r3 = man.condition(r1, VarLabel::new(1), false);
    assert!(
        man.eq_bdd(r3, v1),
        "Not eq:\nOne: {}\nTwo: {}",
        man.print_bdd(r3),
        man.print_bdd(v1)
    );
}

#[test]
fn circuit1() {
    let mut man = BddManager::new_default_order(3);
    let x = man.var(VarLabel::new(0), false);
    let y = man.var(VarLabel::new(1), true);
    let delta = man.and(x, y);
    let yp = man.var(VarLabel::new(2), true);
    let inner = man.iff(yp, y);
    let conj = man.and(inner, delta);
    let res = man.exists(conj, VarLabel::new(1));

    let expected = man.and(x, yp);
    assert!(
        man.eq_bdd(res, expected),
        "Not eq:\nGot: {}\nExpected: {}",
        man.print_bdd(res),
        man.print_bdd(expected)
    );
}

#[test]
fn simple_cond() {
    let mut man = BddManager::new_default_order(3);
    let x = man.var(VarLabel::new(0), true);
    let y = man.var(VarLabel::new(1), false);
    let z = man.var(VarLabel::new(2), false);
    let r1 = man.and(x, y);
    let r2 = man.and(r1, z);
    // now r2 is x /\ !y /\ !z

    let res = man.condition(r2, VarLabel::new(1), true);
    let expected = BddPtr::false_node();
    assert!(
        man.eq_bdd(res, expected),
        "\nOriginal BDD: {}\nNot eq:\nGot: {}\nExpected: {}",
        man.print_bdd(r2),
        man.print_bdd(res),
        man.print_bdd(expected)
    );

}

