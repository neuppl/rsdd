//! Binary decision diagram representation
use super::{var_label::{VarLabel, Literal}, var_order::VarOrder, model::PartialModel, wmc::WmcParams};
use core::fmt::Debug;
use std::iter::FromIterator;



/// Core BDD pointer datatype
#[derive(Debug, Clone, Eq, PartialEq, Hash, Copy, PartialOrd, Ord)]
pub enum BddPtr {
    Compl(*mut BddNode),
    Reg(*mut BddNode),
    PtrTrue,
    PtrFalse,
}

use BddPtr::*;
use bit_set::BitSet;
use num::Num;

impl BddPtr {
    pub fn new_reg(n: *mut BddNode) -> BddPtr {
        Reg(n)
    }

    pub fn new_compl(n: *mut BddNode) -> BddPtr {
        Compl(n)
    }

    pub fn true_ptr() -> BddPtr {
        PtrTrue
    }

    pub fn false_ptr() -> BddPtr {
        PtrFalse
    }

    /// Gets the varlabel of &self 
    /// Panics if not a node
    pub fn var(&self) -> VarLabel {
        self.into_node().var
    }

    /// Get a mutable reference to the node that &self points to
    /// 
    /// Panics if not a node pointer
    pub fn mut_node_ref(&self) -> &mut BddNode {
        unsafe {
            match &self {
                Reg(x) => {
                    &mut (**x)
                },
                Compl(x) => {
                    &mut (**x)
                },
                _ => panic!("Dereferencing constant in deref_or_panic")
            }
        }       
    }

    /// Dereferences the BddPtr into a BddNode 
    /// The pointer is returned in regular-form (i.e., if &self is complemented, then 
    /// the returned BddNode incorporates this information)
    /// 
    /// Panics if the pointer is constant (i.e., true or false)
    pub fn into_node(&self) -> BddNode {
        unsafe {
            match &self {
                Reg(x) => {
                    (**x).clone()
                },
                Compl(x) => {
                    let BddNode {var, low, high, data } = **x;
                    BddNode { var, low: low.compl(), high: high.compl(), data}
                },
                _ => panic!("Dereferencing constant in deref_or_panic")
            }
        }
    }

    /// convert a BddPtr into a regular (non-complemented) pointer
    pub fn to_reg(&self) -> BddPtr {
        match &self {
            Compl(x) => Reg(*x),
            Reg(x) => Reg(*x),
            PtrTrue => PtrTrue,
            PtrFalse => PtrTrue
        }
    }

    /// Returns a complemented version of &self
    pub fn compl(&self) -> BddPtr {
         match &self {
            Compl(x) => Reg(*x),
            Reg(x) => Compl(*x),
            PtrTrue => PtrFalse,
            PtrFalse => PtrTrue
        }
    }

    pub fn low(&self) -> BddPtr {
        unsafe {
            match &self {
                Compl(x) => (**x).low,
                Reg(x) => (**x).low,
                PtrTrue | PtrFalse => panic!("deref constant BDD")
            }
        }
    }

    pub fn high(&self) -> BddPtr {
        unsafe {
            match &self {
                Compl(x) => (**x).high,
                Reg(x) => (**x).high,
                PtrTrue | PtrFalse => panic!("deref constant BDD")
            }
        }
    }


    /// Traverses the BDD and clears all scratch memory (sets it equal to 0)
    pub fn clear_scratch(&self) -> () {
        if self.is_const() {
            return;
        } else {
            let n = self.mut_node_ref();
            if n.data.is_some() {
                n.data = None;
                n.low.clear_scratch();
                n.high.clear_scratch();
            }
        }
    }

    /// Fill the scratch space of each node with a counter that
    /// indexes an in-order left-first depth-first traversal
    /// returns the new count (will be #nodes in the BDD at the end)
    ///
    /// Pre-condition: cleared scratch
    pub fn unique_label_nodes(&self, count: usize) -> usize {
        if self.is_const() {
            return count
        } else {
            let n = self.mut_node_ref();
            if n.data.is_some() {
                return count
            } else {
                n.data = Some(count);
                let newcount = n.low.unique_label_nodes(count+1);
                return n.high.unique_label_nodes(newcount)
            }
        }
    }

    /// evaluates the top element of the data stack on the values found in
    /// `vars`
    pub fn eval(&self, assgn: &HashMap<VarLabel, bool>) -> bool {
        fn eval_bdd_helper(ptr: BddPtr, assgn: &HashMap<VarLabel, bool>) -> bool {
            if ptr.is_true() {
                return true;
            } else if ptr.is_false() {
                return false;
            }
            let bdd = ptr.into_node();
            let value = assgn.get(&bdd.var).unwrap();
            let r = if *value {
                eval_bdd_helper(bdd.high, assgn)
            } else {
                eval_bdd_helper(bdd.low, assgn)
            };
            if ptr.is_compl() {
                !r
            } else {
                r
            }
        }
        eval_bdd_helper(*self, assgn)
    }


    /// Counts the number of nodes present in the BDD (not including constant nodes)
    /// ```
    /// # use rsdd::builder::bdd_builder::BddManager;
    /// # use rsdd::repr::var_label::VarLabel;
    /// let mut mgr = BddManager::new_default_order(10);
    /// let lbl_a = mgr.new_var();
    /// let lbl_b = mgr.new_var();
    /// let a = mgr.var(lbl_a, true);
    /// let b = mgr.var(lbl_b, true);
    /// let a_and_b = mgr.and(a, b);
    /// // This BDD has size 2, as it looks like (if a then b else false)
    /// assert_eq!(mgr.count_nodes(a_and_b), 2);
    /// ```
    pub fn count_nodes(&self) -> usize {
        let s = self.unique_label_nodes(0);
        self.clear_scratch();
        s
    }

    /// true if the BddPtr points to a constant (i.e., True or False)
    pub fn is_const(&self) -> bool {
        match &self {
            Compl(_) => false,
            Reg(_) => false,
            PtrTrue => true,
            PtrFalse => true 
        }
    }

    pub fn is_true(&self) -> bool {
        match &self {
            Compl(_) | Reg(_) | PtrFalse  => false,
            PtrTrue => true,
        }
    }

    pub fn is_false(&self) -> bool {
        match &self {
            Compl(_) | Reg(_) | PtrTrue => false,
            PtrFalse => true,
        }
    }


    /// True is this is a complemented edge pointer
    pub fn is_compl(&self) -> bool {
         match &self {
            Compl(_) => true,
            Reg(_) => false,
            PtrTrue => false,
            PtrFalse => false
        }
    }

    /// gets scratch (panics if not node)
    pub fn get_scratch(&self) -> Option<usize> {
        self.into_node().data
    }

    pub fn to_string_debug(&self) -> String {
        fn print_bdd_helper(ptr: BddPtr) -> String {
            if ptr.is_true() {
                return String::from("T");
            } else if ptr.is_false() {
                return String::from("F");
            } else {
                let l_p = if ptr.is_compl() {
                    ptr.low().compl()
                } else {
                    ptr.low()
                };
                let h_p = if ptr.is_compl() {
                    ptr.high().compl()
                } else {
                    ptr.high()
                };
                let l_s = print_bdd_helper(l_p);
                let h_s = print_bdd_helper(h_p);
                format!("({}, {}, {})", ptr.var().value(), h_s, l_s)
            }
        }
        print_bdd_helper(*self)
    }


    /// Print a debug form of the BDD with the label remapping given by `map`
    pub fn print_bdd_lbl(&self, ptr: BddPtr, map: &HashMap<VarLabel, VarLabel>) -> String {
        panic!("todo")
        // use crate::builder::repr::builder_bdd::PointerType::*;
        // fn print_bdd_helper(
        //     t: &BddManager,
        //     ptr: BddPtr,
        //     map: &HashMap<VarLabel, VarLabel>,
        // ) -> String {
        //     match ptr.ptr_type() {
        //         PtrTrue => String::from("T"),
        //         PtrFalse => String::from("T"),
        //         PtrNode => {
        //             let l_p = t.low(ptr);
        //             let h_p = t.high(ptr);
        //             let l_s = print_bdd_helper(t, l_p, map);
        //             let r_s = print_bdd_helper(t, h_p, map);
        //             let lbl = ptr.label();
        //             format!(
        //                 "({:?}, {}{}, {}{})",
        //                 map.get(&lbl).unwrap_or(&lbl).value(),
        //                 if l_p.is_compl() { "!" } else { "" },
        //                 l_s,
        //                 if h_p.is_compl() { "!" } else { "" },
        //                 r_s
        //             )
        //         }
        //     }
        // }
        // let s = print_bdd_helper(self, ptr, map);
        // format!("{}{}", if ptr.is_compl() { "!" } else { "" }, s)
    }

    fn smoothed_bottomup_pass_h<T: Clone + Copy + Debug, F: Fn(VarLabel, T, T) -> T>(
        &self,
        order: &VarOrder,
        f: &F,
        low_v: T,
        high_v: T,
        expected_level: usize,
        tbl: &mut Vec<(Option<T>, Option<T>)>,
    ) -> T {
        let mut r = {
            if self.is_true() {
                high_v
            } else if self.is_false() {
                low_v
            } else {
                let idx = self.get_scratch().unwrap();
                match tbl[idx] {
                    (_, Some(v)) if self.is_compl() => v,
                    (Some(v), _) if !self.is_compl() => v,
                    (cur_reg, cur_compl) => {
                        let (low, high) = if self.is_compl() {
                            (self.low().compl(), self.high().compl())
                        } else {
                            (self.low(), self.high())
                        };
                        let cur_level = order.get(self.var());
                        let low_val = low.smoothed_bottomup_pass_h(
                            order,
                            f,
                            low_v,
                            high_v,
                            cur_level + 1,
                            tbl,
                        );
                        let high_val = high.smoothed_bottomup_pass_h(
                            order,
                            f,
                            low_v,
                            high_v,
                            cur_level + 1,
                            tbl,
                        );

                        let res = f(self.var(), low_val, high_val);

                        // cache result and return
                        tbl[idx] = if self.is_compl() {
                            (cur_reg, Some(res))
                        } else {
                            (Some(res), cur_compl)
                        };
                        res
                    }
                }
            }
        };

        // smoothing
        if expected_level >= order.num_vars() {
            return r;
        }
        let smoothing_depth = {
            if self.is_const() {
                order.num_vars()
            } else {
                let toplabel = self.var();
                order.get(toplabel)
           }
        };
        for v in order.between_iter(expected_level, smoothing_depth)
        {
            r = f(v, r, r);
        }
        r
    }

    /// performs an amortized bottom-up smoothed pass with aggregating function `f`
    /// calls `f` on every (smoothed) BDD node and caches and reuses the results
    /// `f` has type `cur_label -> low_value -> high_value -> aggregated_value`
    fn smoothed_bottomup_pass<T: Clone + Copy + Debug, F: Fn(VarLabel, T, T) -> T>(
        &self,
        order: &VarOrder,
        f: F,
        low_v: T,
        high_v: T,
    ) -> T {
        let n = self.unique_label_nodes(0);
        let mut cache: Vec<(Option<T>, Option<T>)> = vec![(None, None); n];
        let res = self.smoothed_bottomup_pass_h(order, &f, low_v, high_v, 0, &mut cache);
        self.clear_scratch();
        res
    }


    /// Weighted-model count
    pub fn wmc<T: Num + Clone + Debug + Copy>(&self, order: &VarOrder, params: &WmcParams<T>) -> T {
        self.smoothed_bottomup_pass(
            order,
            |varlabel, low, high| {
                let (low_w, high_w) = params.get_var_weight(varlabel);
                (*low_w * low) + (*high_w * high)
            },
            params.zero,
            params.one,
        )
    }

    /// evaluates a circuit on a partial marginal MAP assignment to get an upper-bound on the wmc
    /// maxes over the `map_vars`, applies the `partial_map_assgn`
    fn marginal_map_eval(
        &self,
        order: &VarOrder,
        partial_map_assgn: &PartialModel,
        map_vars: &BitSet,
        wmc: &WmcParams<f64>,
    ) -> f64 {
        self.smoothed_bottomup_pass(
            order,
            |varlabel, low, high| {
                let (low_w, high_w) = wmc.get_var_weight(varlabel);
                match partial_map_assgn.get(varlabel) {
                    None => {
                        if map_vars.contains(varlabel.value_usize()) {
                            f64::max(*low_w * low, *high_w * high)
                        } else {
                            (*low_w * low) + (*high_w * high)
                        }
                    }
                    Some(true) => *high_w * high,
                    Some(false) => *low_w * low,
                }
            },
            wmc.zero,
            wmc.one,
        )
    }

    fn marginal_map_h(
        &self,
        order: &VarOrder,
        cur_lb: f64,
        cur_best: PartialModel,
        margvars: &[VarLabel],
        wmc: &WmcParams<f64>,
        cur_assgn: PartialModel,
    ) -> (f64, PartialModel) {
        match margvars {
            [] => {
                let margvar_bits = BitSet::new();
                let possible_best = self.marginal_map_eval(order, &cur_assgn, &margvar_bits, wmc);
                if possible_best > cur_lb {
                    (possible_best, cur_assgn)
                } else {
                    (cur_lb, cur_best)
                }
            }
            [x, end @ ..] => {
                let mut best_model = cur_best;
                let mut best_lb = cur_lb;
                let margvar_bits = BitSet::from_iter(end.iter().map(|x| x.value_usize()));

                let mut true_model = cur_assgn.clone();
                true_model.set(*x, true);
                let mut false_model = cur_assgn.clone();
                false_model.set(*x, false);

                let true_ub = self.marginal_map_eval(order, &true_model, &margvar_bits, wmc);
                let false_ub = self.marginal_map_eval(order, &false_model, &margvar_bits, wmc);

                // branch on the greater upper-bound first
                let o = if true_ub > false_ub {
                    [(true_ub, true_model), (false_ub, false_model)]
                } else {
                    [(false_ub, false_model), (true_ub, true_model)]
                };
                for (upper_bound, partialmodel) in o {
                    // branch + bound
                    if upper_bound > best_lb {
                        (best_lb, best_model) = self.marginal_map_h(
                            order,
                            best_lb,
                            best_model,
                            end,
                            wmc,
                            partialmodel.clone(),
                        );
                    }
                }
                (best_lb, best_model)
            }
        }
    }

    /// Computes the marginal map over variables `vars` of `ptr` with evidence `evidence`
    /// I.e., computes argmax_{v in vars} \sum_{v not in vars} w(ptr /\ evidence)
    /// ```
    /// use rsdd::builder::bdd_builder::{BddManager, BddWmc};
    /// use rsdd::repr::var_label::{VarLabel, Literal};
    /// use rsdd::repr::model::PartialModel;
    /// use std::collections::HashMap;
    /// use rsdd::repr::cnf::Cnf;
    /// let cnf = Cnf::from_string(String::from("(1 || 2 || 3 || 4)"));
    /// let mut mgr = BddManager::new_default_order(cnf.num_vars());
    /// let w : HashMap<VarLabel, (f64, f64)> = (0..5).map(|x| (VarLabel::new(x), (0.3, 0.7))).collect();
    /// let wmc = BddWmc::new_with_default(0.0, 1.0, w);
    /// let evidence = mgr.true_ptr();
    /// let bdd = mgr.from_cnf(&cnf);
    /// let (p, marg_map) = mgr.marginal_map(bdd, evidence, &vec![VarLabel::new(0), VarLabel::new(1)], &wmc);
    /// let expected_model = PartialModel::from_litvec(&vec![Literal::new(VarLabel::new(0), true), Literal::new(VarLabel::new(1), true)], cnf.num_vars());
    /// let expected_prob = 0.49;
    /// assert_eq!(marg_map, expected_model);
    /// ```
    pub fn marginal_map(
        &self,
        order: &VarOrder,
        vars: &[VarLabel],
        wmc: &WmcParams<f64>,
    ) -> (f64, PartialModel) {
        let mut marg_vars = BitSet::new();
        for v in vars {
            marg_vars.insert(v.value_usize());
        }

        let all_true: Vec<Literal> = vars.iter().map(|x| Literal::new(*x, true)).collect();
        let cur_assgn = PartialModel::from_litvec(&all_true, order.num_vars());
        let lower_bound = self.marginal_map_eval(order, &cur_assgn, &BitSet::new(), wmc);

        self.marginal_map_h(
            order,
            lower_bound,
            cur_assgn,
            vars,
            wmc,
            PartialModel::from_litvec(&[], order.num_vars()),
        )
    }

}

/// Core BDD node storage
#[derive(Debug, Clone, Eq)]
pub struct BddNode {
    pub var: VarLabel,
    pub low: BddPtr,
    pub high: BddPtr,
    /// scratch space used for caching data during traversals; ignored during
    /// equality checking and hashing
    data: Option<usize>
}

impl BddNode {
    pub fn new(var: VarLabel, low: BddPtr, high: BddPtr) -> BddNode {
        BddNode { var, low, high, data: None}
    }
}

impl PartialEq for BddNode {
    fn eq(&self, other: &Self) -> bool {
        self.var == other.var && self.low == other.low && self.high == other.high
    }
}

use std::{hash::{Hash, Hasher}, collections::HashMap};
impl Hash for BddNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.var.hash(state);
        self.low.hash(state);
        self.high.hash(state);
    }
}