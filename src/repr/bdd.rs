//! Binary decision diagram representation

use crate::{
    repr::PartialModel,
    repr::VarOrder,
    repr::WmcParams,
    repr::{DDNNFPtr, DDNNF},
    repr::{Literal, VarLabel, VarSet},
    util::semirings::{BBSemiring, FiniteField, JoinSemilattice, RealSemiring},
    util::semirings::{ExpectedUtility, LatticeWithChoose, MeetSemilattice},
};
use bit_set::BitSet;
use core::fmt::Debug;
use std::{
    any::Any,
    cell::RefCell,
    collections::HashMap,
    hash::{Hash, Hasher},
    iter::FromIterator,
    ptr,
};
use BddPtr::*;

use super::var_order::PartialVariableOrder;

/// Core BDD pointer datatype
#[derive(Debug, Clone, Eq, Copy, PartialOrd, Ord)]
pub enum BddPtr<'a> {
    Compl(&'a BddNode<'a>),
    Reg(&'a BddNode<'a>),
    PtrTrue,
    PtrFalse,
}

impl<'a> PartialEq for BddPtr<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Compl(l0), Self::Compl(r0)) => std::ptr::eq(*l0, *r0),
            (Self::Reg(l0), Self::Reg(r0)) => std::ptr::eq(*l0, *r0),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl<'a> Hash for BddPtr<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Compl(n) | Reg(n) => ptr::hash(*n, state),
            _ => (),
        }
    }
}

impl<'a> PartialVariableOrder for BddPtr<'a> {
    fn var(&self) -> Option<VarLabel> {
        match self {
            Compl(n) | Reg(n) => Some(n.var),
            PtrTrue | PtrFalse => None,
        }
    }
}

/// The intermediate representation for a BddPtr that is being folded in a
/// [`Fold`] computation.
///
/// - [`FoldNode::parent_is_compl`]: tells you if the parent node was a [`BddPtr::Compl`].
/// - [`FoldNode::node`]: gives you the pointer to the current node.
/// - [`FoldNode::var`]: gives you the [`VarLabel`] if the node is a [`BddPtr::Reg`].
pub struct FoldNode<'a> {
    pub parent_is_compl: bool,
    pub node: BddPtr<'a>,
    pub var: Option<VarLabel>,
}
impl<'a> FoldNode<'a> {
    fn new(node: BddPtr<'a>, parent_is_compl: bool, var: Option<VarLabel>) -> Self {
        Self {
            parent_is_compl,
            node,
            var,
        }
    }
}

/// Folds in the style of [foldl on
/// hackage](https://hackage.haskell.org/package/foldl-1.4.14/docs/Control-Foldl.html#t:Fold).
/// This performs specialized folds over [`FoldNode`] representations. With a
/// (currently non-existent) composition, one can perform multiple folds in a
/// single walk of the BDD.
///
/// See [`Fold::mut_fold`]'s documentation for an example of how to write this
/// style of fold.
pub struct Fold<'a, T, U> {
    /// [`FnMut`] step which takes in an aggregation `T` and a node
    /// representation [`FoldNode`].
    pub step: &'a mut dyn FnMut(T, FoldNode) -> T,
    /// The initial value for the aggregation.
    pub initial: T,
    /// How to extract the final value `U` from a node. If a node has children,
    /// provide the final outputs of their values as a tuple.
    pub extract: &'a dyn Fn(T, Option<(U, U)>) -> U,
}

impl<'a, T: Clone, U> Fold<'a, T, U> {
    /// Construct a new [`Fold`].
    pub fn new(
        step: &'a mut dyn FnMut(T, FoldNode) -> T,
        initial: T,
        extract: &'a dyn Fn(T, Option<(U, U)>) -> U,
    ) -> Fold<'a, T, U> {
        Self {
            step,
            initial,
            extract,
        }
    }

    fn mut_fold_h(&mut self, bdd: &BddPtr, p_compl: bool, r: &T) -> U {
        match bdd {
            BddPtr::PtrTrue => {
                let t = (self.step)(r.clone(), FoldNode::new(*bdd, p_compl, None));
                (self.extract)(t, None)
            }
            BddPtr::PtrFalse => {
                let t = (self.step)(r.clone(), FoldNode::new(*bdd, p_compl, None));
                (self.extract)(t, None)
            }
            BddPtr::Compl(n) => self.mut_fold_h(&BddPtr::Reg(n), true, r),
            BddPtr::Reg(n) => {
                let l_p = n.low;
                let r_p = n.high;

                let lbl = n.var;
                let t = (self.step)(r.clone(), FoldNode::new(*bdd, p_compl, Some(lbl)));
                let l_r = self.mut_fold_h(&l_p, false, &r.clone());
                let r_r = self.mut_fold_h(&r_p, false, &r.clone());
                (self.extract)(t, Some((l_r, r_r)))
            }
        }
    }
    /// A mutable fold. An example of how to use this fold can be seen by collecting all VarLabels in the sub-tree referenced by a given [`BddPtr`]:
    ///
    /// ```rust
    /// use rsdd::repr::VarLabel;
    /// use rsdd::repr::BddPtr;
    /// use rsdd::repr::Fold;
    ///
    /// pub fn variables(bdd: BddPtr) -> Vec<VarLabel> {
    ///     Fold::new(
    ///         &mut |vs: Vec<Option<VarLabel>>, bdd| {
    ///             let mut vs = vs;
    ///             vs.push(bdd.node.var_safe());
    ///             vs
    ///         },
    ///         vec![],
    ///         &|ret, lo_hi| match lo_hi {
    ///             None => ret,
    ///             Some((lo, hi)) => {
    ///                 let mut v = ret;
    ///                 v.extend(lo);
    ///                 v.extend(hi);
    ///                 v
    ///             }
    ///         },
    ///     )
    ///     .mut_fold(&bdd)
    ///     .into_iter()
    ///     .flatten()
    ///     .collect()
    /// }
    /// ```
    pub fn mut_fold(&mut self, bdd: &BddPtr) -> U {
        let initial = self.initial.clone();
        self.mut_fold_h(bdd, false, &initial)
    }
}

impl<'a> BddPtr<'a> {
    /// Gets the varlabel of &self
    #[inline]
    pub fn var_safe(&self) -> Option<VarLabel> {
        match self {
            Compl(n) | Reg(n) => Some(n.var),
            _ => None,
        }
    }

    /// convert a BddPtr into a regular (non-complemented) pointer,
    /// but does not change the underlying node.
    /// ```
    /// use rsdd::repr::{
    ///     BddNode, BddPtr,
    ///     VarLabel
    /// };
    ///
    /// assert_eq!(BddPtr::PtrTrue, BddPtr::PtrTrue.to_reg());
    /// assert_eq!(BddPtr::PtrTrue, BddPtr::PtrFalse.to_reg());
    ///
    /// // this node represents the positive literal 0
    /// let node = BddNode::new(VarLabel::new(0), BddPtr::PtrFalse, BddPtr::PtrTrue);
    ///
    /// assert_eq!(BddPtr::Reg(&node), BddPtr::Compl(&node).to_reg());
    /// assert_eq!(BddPtr::Reg(&node), BddPtr::Reg(&node).to_reg());
    /// ```
    pub fn to_reg(&self) -> BddPtr {
        match &self {
            Compl(x) => Reg(x),
            Reg(x) => Reg(x),
            PtrTrue => PtrTrue,
            PtrFalse => PtrTrue,
        }
    }

    /// ```
    /// use rsdd::repr::{
    ///     BddNode, BddPtr,
    ///     VarLabel
    /// };
    ///
    /// // this node represents the positive literal 0
    /// let node = BddNode::new(VarLabel::new(0), BddPtr::PtrFalse, BddPtr::PtrTrue);
    ///
    /// // for regular BDDs, this behaves "normally"
    /// assert_eq!(BddPtr::Reg(&node).low(), BddPtr::PtrFalse);
    /// // but, for complemented edges, it negates the low edge
    /// assert_eq!(BddPtr::Compl(&node).low(), BddPtr::PtrTrue);
    /// ```
    pub fn low(&self) -> BddPtr<'a> {
        match &self {
            Compl(x) => x.low.neg(),
            Reg(x) => x.low,
            PtrTrue | PtrFalse => panic!("deref constant BDD"),
        }
    }

    /// ```
    /// use rsdd::repr::{
    ///     BddNode, BddPtr,
    ///     VarLabel
    /// };
    ///
    /// // this node represents the positive literal 0
    /// let node = BddNode::new(VarLabel::new(0), BddPtr::PtrFalse, BddPtr::PtrTrue);
    ///
    /// // for regular BDDs, this behaves "normally"
    /// assert_eq!(BddPtr::Reg(&node).low_raw(), BddPtr::PtrFalse);
    /// // but, for complemented edges, it does not negate the low edge
    /// assert_eq!(BddPtr::Compl(&node).low_raw(), BddPtr::PtrFalse);
    /// ```
    pub fn low_raw(&self) -> BddPtr<'a> {
        match &self {
            Compl(x) => x.low,
            Reg(x) => x.low,
            PtrTrue | PtrFalse => panic!("deref constant BDD"),
        }
    }

    /// ```
    /// use rsdd::repr::{
    ///     BddNode, BddPtr,
    ///     VarLabel
    /// };
    ///
    /// // this node represents the positive literal 0
    /// let node = BddNode::new(VarLabel::new(0), BddPtr::PtrFalse, BddPtr::PtrTrue);
    ///
    /// // for regular BDDs, this behaves "normally"
    /// assert_eq!(BddPtr::Reg(&node).high_raw(), BddPtr::PtrTrue);
    /// // but, for complemented edges, it does not negate the high edge
    /// assert_eq!(BddPtr::Compl(&node).high_raw(), BddPtr::PtrTrue);
    /// ```
    pub fn high_raw(&self) -> BddPtr<'a> {
        match &self {
            Compl(x) => x.high,
            Reg(x) => x.high,
            PtrTrue | PtrFalse => panic!("deref constant BDD"),
        }
    }

    /// ```
    /// use rsdd::repr::{
    ///     BddNode, BddPtr,
    ///     VarLabel
    /// };
    ///
    /// // this node represents the positive literal 0
    /// let node = BddNode::new(VarLabel::new(0), BddPtr::PtrFalse, BddPtr::PtrTrue);
    ///
    /// // for regular BDDs, this behaves "normally"
    /// assert_eq!(BddPtr::Reg(&node).high(), BddPtr::PtrTrue);
    /// // but, for complemented edges, it negates the high edge
    /// assert_eq!(BddPtr::Compl(&node).high(), BddPtr::PtrFalse);
    /// ```
    pub fn high(&self) -> BddPtr<'a> {
        match &self {
            Compl(x) => x.high.neg(),
            Reg(x) => x.high,
            PtrTrue | PtrFalse => panic!("deref constant BDD"),
        }
    }

    /// Traverses the BDD and clears all scratch memory (sets it equal to 0)
    pub fn clear_scratch(&self) {
        match &self {
            Compl(x) | Reg(x) => {
                if x.data.borrow().is_some() {
                    x.data.take();
                    x.low.clear_scratch();
                    x.high.clear_scratch();
                }
            }
            PtrTrue | PtrFalse => (),
        }
    }

    /// true if the BddPtr points to a constant (i.e., True or False)
    /// ```
    /// use rsdd::repr::{
    ///     BddNode, BddPtr,
    ///     VarLabel
    /// };
    ///
    /// assert!(BddPtr::is_const(&BddPtr::PtrTrue));
    /// assert!(BddPtr::is_const(&BddPtr::PtrFalse));
    ///
    /// let node = BddNode::new(VarLabel::new(0), BddPtr::PtrFalse, BddPtr::PtrTrue);
    ///
    /// assert!(!BddPtr::is_const(&BddPtr::Reg(&node)));
    /// assert!(!BddPtr::is_const(&BddPtr::Compl(&node)));
    /// ```
    pub fn is_const(&self) -> bool {
        match &self {
            Reg(_) | Compl(_) => false,
            PtrTrue | PtrFalse => true,
        }
    }

    /// Gets the scratch value stored in `&self`
    ///
    /// Panics if not node.
    pub fn scratch<T: ?Sized + Clone + 'static>(&self) -> Option<T> {
        match self {
            Compl(n) | Reg(n) => {
                if self.is_scratch_cleared() {
                    return None;
                }
                // println!("dereferencing {:?}", n.data.as_ptr());
                n.data
                    .borrow()
                    .as_ref()
                    .unwrap()
                    .as_ref()
                    .downcast_ref::<T>()
                    .cloned()
            }
            PtrTrue => None,
            PtrFalse => None,
        }
    }

    /// Set the scratch in this node to the value `v`.
    ///
    /// Panics if not a node.
    ///
    /// Invariant: values stored in `set_scratch` must not outlive
    /// the provided allocator `alloc` (i.e., calling `scratch`
    /// involves dereferencing a pointer stored in `alloc`)
    pub fn set_scratch<T: 'static>(&self, v: T) {
        match self {
            Compl(n) | Reg(n) => {
                *n.data.borrow_mut() = Some(Box::new(v));
            }
            _ => panic!("attempting to store scratch on constant"),
        }
    }

    /// true if the scratch is current cleared
    pub fn is_scratch_cleared(&self) -> bool {
        // return true;
        match self {
            Compl(n) | Reg(n) => n.data.borrow().is_none(),
            PtrTrue => true,
            PtrFalse => true,
        }
    }

    pub fn to_string_debug(&self) -> String {
        fn print_bdd_helper(ptr: BddPtr) -> String {
            match ptr {
                PtrTrue => String::from("T"),
                PtrFalse => String::from("F"),
                Reg(node) | Compl(node) => {
                    let l_p = if ptr.is_neg() {
                        ptr.low_raw().neg()
                    } else {
                        ptr.low_raw()
                    };
                    let h_p = if ptr.is_neg() {
                        ptr.high_raw().neg()
                    } else {
                        ptr.high_raw()
                    };
                    let l_s = print_bdd_helper(l_p);
                    let h_s = print_bdd_helper(h_p);
                    format!("({}, {}, {})", node.var.value(), h_s, l_s)
                }
            }
        }
        print_bdd_helper(*self)
    }

    /// Print a debug form of the BDD with the label remapping given by `map`
    pub fn print_bdd_lbl(&self, map: &HashMap<VarLabel, VarLabel>) -> String {
        match self {
            BddPtr::PtrTrue => String::from("T"),
            BddPtr::PtrFalse => String::from("F"),
            BddPtr::Compl(n) => {
                let s = BddPtr::Reg(n).print_bdd_lbl(map);
                format!("!{}", s)
            }
            BddPtr::Reg(n) => {
                let l_p = n.low;
                let r_p = n.high;
                let l_s = l_p.print_bdd_lbl(map);
                let r_s = r_p.print_bdd_lbl(map);
                let lbl = n.var;
                format!(
                    "({:?}, {}, {})",
                    map.get(&lbl).unwrap_or(&lbl).value(),
                    l_s,
                    r_s
                )
            }
        }
    }
    #[inline]
    pub fn print_bdd(&self) -> String {
        self.print_bdd_lbl(&HashMap::new())
    }

    fn bdd_fold_h<T: Clone + Copy + Debug, F: Fn(VarLabel, T, T) -> T>(
        &self,
        f: &F,
        low_v: T,
        high_v: T,
    ) -> T
    where
        T: 'static,
    {
        match self {
            // If current node is true leaf, return accumulated high_v value
            PtrTrue => high_v,
            // Else if current node is false leaf, return accumulated low_v value
            PtrFalse => low_v,
            Reg(node) | Compl(node) => {
                // Every node has an associated cache (scratch)
                // Scratch data is arbitrary (depends on use case)
                // fst is negated, snd is non-negated accumulator

                let fold_helper = |prev_low, prev_high| {
                    // Standard fold stuff
                    let l = self.low().bdd_fold_h(f, low_v, high_v);
                    let h = self.high().bdd_fold_h(f, low_v, high_v);
                    let res = f(node.var, l, h);
                    // Set cache (accumulator)
                    // Then corrects scratch so it traverses correctly in a recursive case downstream
                    if self.is_neg() {
                        self.set_scratch::<(Option<T>, Option<T>)>((Some(res), prev_high));
                    } else {
                        self.set_scratch::<(Option<T>, Option<T>)>((prev_low, Some(res)));
                    }
                    res
                };

                match self.scratch::<(Option<T>, Option<T>)>() {
                    // If complemented and accumulated, use the already memoized value
                    Some((Some(v), _)) if self.is_neg() => v,
                    // Same for not complemented
                    Some((_, Some(v))) if !self.is_neg() => v,
                    // (Some(v), None) but not a complemented node
                    // (None, Some(v)) but a complemented node
                    Some((prev_low, prev_high)) => fold_helper(prev_low, prev_high),
                    None => fold_helper(None, None),
                }
            }
        }
    }

    pub fn bdd_fold<T: Clone + Copy + Debug, F: Fn(VarLabel, T, T) -> T>(
        &self,
        f: &F,
        low_v: T,
        high_v: T,
    ) -> T
    where
        T: 'static,
    {
        let r = self.bdd_fold_h(f, low_v, high_v);
        self.clear_scratch();
        r
    }

    /// evaluates a circuit on a partial marginal MAP assignment to get an upper-bound on the wmc
    /// maxes over the `map_vars`, applies the `partial_map_assgn`
    fn marginal_map_eval(
        &self,
        partial_map_assgn: &PartialModel,
        map_vars: &BitSet,
        wmc: &WmcParams<RealSemiring>,
    ) -> RealSemiring {
        let mut v = self.bdd_fold(
            &|varlabel, low, high| {
                let (low_w, high_w) = wmc.var_weight(varlabel);
                match partial_map_assgn.get(varlabel) {
                    None => {
                        if map_vars.contains(varlabel.value_usize()) {
                            RealSemiring(f64::max((*low_w * low).0, (*high_w * high).0))
                        } else {
                            (*low_w * low) + (*high_w * high)
                        }
                    }
                    Some(true) => high,
                    Some(false) => low,
                }
            },
            wmc.zero,
            wmc.one,
        );
        // multiply in weights of all variables in the partial assignment
        for lit in partial_map_assgn.assignment_iter() {
            let (l, h) = wmc.var_weight(lit.label());
            if lit.polarity() {
                v = v * (*h);
            } else {
                v = v * (*l);
            }
        }
        v
    }

    fn marginal_map_h(
        &self,
        cur_lb: f64,
        cur_best: PartialModel,
        margvars: &[VarLabel],
        wmc: &WmcParams<RealSemiring>,
        cur_assgn: PartialModel,
    ) -> (f64, PartialModel) {
        match margvars {
            [] => {
                let margvar_bits = BitSet::new();
                let possible_best = self.marginal_map_eval(&cur_assgn, &margvar_bits, wmc);
                if possible_best.0 > cur_lb {
                    (possible_best.0, cur_assgn)
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

                let mut false_model = cur_assgn;
                false_model.set(*x, false);

                let true_ub = self.marginal_map_eval(&true_model, &margvar_bits, wmc);
                let false_ub = self.marginal_map_eval(&false_model, &margvar_bits, wmc);

                // branch on the greater upper-bound first
                let order = if true_ub.0 > false_ub.0 {
                    [(true_ub, true_model), (false_ub, false_model)]
                } else {
                    [(false_ub, false_model), (true_ub, true_model)]
                };
                for (upper_bound, partialmodel) in order {
                    // branch + bound
                    if upper_bound.0 > best_lb {
                        (best_lb, best_model) = self.marginal_map_h(
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

    /// Computes the marginal map over variables `vars` of `ptr`
    /// I.e., computes argmax_{v in vars} \sum_{v not in vars} w(ptr)
    pub fn marginal_map(
        &self,
        vars: &[VarLabel],
        num_vars: usize,
        wmc: &WmcParams<RealSemiring>,
    ) -> (f64, PartialModel) {
        let all_true: Vec<Literal> = vars.iter().map(|x| Literal::new(*x, true)).collect();
        let cur_assgn = PartialModel::from_litvec(&all_true, num_vars);
        let lower_bound = self.marginal_map_eval(&cur_assgn, &BitSet::new(), wmc);

        self.marginal_map_h(
            lower_bound.0,
            cur_assgn,
            vars,
            wmc,
            PartialModel::from_litvec(&[], num_vars),
        )
    }

    /// upper-bounding the expected utility, for meu_h
    fn eu_ub(
        &self,
        partial_decisions: &PartialModel,
        decision_vars: &BitSet,
        wmc: &WmcParams<ExpectedUtility>,
    ) -> ExpectedUtility {
        // println!("Assigned decision variables: {:?}", partial_decisions);

        // accumulator for EU via bdd_fold

        self.bdd_fold(
            &|varlabel, low: ExpectedUtility, high: ExpectedUtility| {
                // get True and False weights for VarLabel
                let (false_w, true_w) = wmc.var_weight(varlabel);
                // Check if our partial model has already assigned my variable.
                match partial_decisions.get(varlabel) {
                    // If not...
                    None => {
                        // If it's a decision variable, we do
                        if decision_vars.contains(varlabel.value_usize()) {
                            let max_pr = f64::max(low.0, high.0);
                            let max_eu = f64::max(low.1, high.1);
                            ExpectedUtility(max_pr, max_eu)
                        // Otherwise it's just a probabilistic variable so you do
                        // the usual stuff...
                        } else {
                            (*false_w * low) + (*true_w * high)
                        }
                    }
                    Some(true) => high,
                    Some(false) => low,
                }
            },
            wmc.zero,
            wmc.one,
        )
    }

    fn meu_h(
        &self,
        evidence: BddPtr,
        cur_lb: ExpectedUtility,
        cur_best: PartialModel,
        decision_vars: &[VarLabel],
        wmc: &WmcParams<ExpectedUtility>,
        cur_assgn: PartialModel,
    ) -> (ExpectedUtility, PartialModel) {
        match decision_vars {
            // If all decision variables are assigned,
            [] => {
                // Run the eu ub
                let decision_bitset = BitSet::new();
                let possible_best = self.eu_ub(&cur_assgn, &decision_bitset, wmc)
                    / evidence.bb_lb(&cur_assgn, &decision_bitset, wmc);
                // If it's a better lb, update.
                if possible_best.1 > cur_lb.1 {
                    (possible_best, cur_assgn)
                } else {
                    (cur_lb, cur_best)
                }
            }
            // If there exists an unassigned decision variable,
            [x, end @ ..] => {
                let mut best_model = cur_best;
                let mut best_lb = cur_lb;
                let margvar_bits = BitSet::from_iter(end.iter().map(|x| x.value_usize()));
                // Consider the assignment of it to true...
                let mut true_model = cur_assgn.clone();
                true_model.set(*x, true);
                // ... and false...
                let mut false_model = cur_assgn;
                false_model.set(*x, false);

                // and calculate their respective upper bounds.
                let true_ub_num = self.eu_ub(&true_model, &margvar_bits, wmc);
                let false_ub_num = self.eu_ub(&false_model, &margvar_bits, wmc);

                let true_ub_dec = evidence.bb_lb(&true_model, &margvar_bits, wmc);
                let false_ub_dec = evidence.bb_lb(&false_model, &margvar_bits, wmc);

                let true_ub = true_ub_num / true_ub_dec;
                let false_ub = false_ub_num / false_ub_dec;

                // branch on the greater upper-bound first
                let order = if true_ub.1 > false_ub.1 {
                    [(true_ub, true_model), (false_ub, false_model)]
                } else {
                    [(false_ub, false_model), (true_ub, true_model)]
                };
                for (upper_bound, partialmodel) in order {
                    // branch + bound
                    if upper_bound.1 > best_lb.1 {
                        (best_lb, best_model) = self.meu_h(
                            evidence,
                            best_lb,
                            best_model,
                            end,
                            wmc,
                            partialmodel.clone(),
                        )
                    }
                }
                (best_lb, best_model)
            }
        }
    }

    /// maximum expected utility calc, scaled for evidence.
    /// introduced in Section 5 of the daPPL paper
    pub fn meu(
        &self,
        evidence: BddPtr,
        decision_vars: &[VarLabel],
        num_vars: usize,
        wmc: &WmcParams<ExpectedUtility>,
    ) -> (ExpectedUtility, PartialModel) {
        // Initialize all the decision variables to be true
        let all_true: Vec<Literal> = decision_vars
            .iter()
            .map(|x| Literal::new(*x, true))
            .collect();
        let cur_assgn = PartialModel::from_litvec(&all_true, num_vars);
        // Calculate bound wrt the partial instantiation.
        let lower_bound = self.eu_ub(&cur_assgn, &BitSet::new(), wmc)
            / evidence.bb_lb(&cur_assgn, &BitSet::new(), wmc);
        self.meu_h(
            evidence,
            lower_bound,
            cur_assgn,
            decision_vars,
            wmc,
            PartialModel::from_litvec(&[], num_vars),
        )
    }

    /// Below is experimental code with a generic branch and bound for T a BBAlgebra.
    /// upper-bounding the expected utility, for meu_h
    fn bb_ub<T: BBSemiring>(
        &self,
        partial_join_assgn: &PartialModel,
        join_vars: &BitSet,
        wmc: &WmcParams<T>,
    ) -> T
    where
        T: 'static,
    {
        let mut partial_join_acc = T::one();
        for lit in partial_join_assgn.assignment_iter() {
            let (l, h) = wmc.var_weight(lit.label());
            if lit.polarity() {
                partial_join_acc = partial_join_acc * (*h);
            } else {
                partial_join_acc = partial_join_acc * (*l);
            }
        }
        // top-down UB calculation via bdd_fold
        let v = self.bdd_fold(
            &|varlabel, low: T, high: T| {
                // get True and False weights for node VarLabel
                let (w_l, w_h) = wmc.var_weight(varlabel);
                // Check if our partial model has already assigned the node.
                match partial_join_assgn.get(varlabel) {
                    // If not...
                    None => {
                        // If it's a join variable, (w_l * low) ∨ (w_h * high)
                        if join_vars.contains(varlabel.value_usize()) {
                            let lhs = *w_l * low;
                            let rhs = *w_h * high;
                            JoinSemilattice::join(&lhs, &rhs)
                        // Otherwise it is a sum variables, so
                        } else {
                            (*w_l * low) + (*w_h * high)
                        }
                    }
                    // If our node has already been assigned, then we
                    // reached a base case. We return the accumulated value.
                    Some(true) => high,
                    Some(false) => low,
                }
            },
            wmc.zero,
            wmc.one,
        );
        partial_join_acc * v
    }

    /// lower-bounding the expected utility, for meu_h
    fn bb_lb<T: LatticeWithChoose>(
        &self,
        partial_join_assgn: &PartialModel,
        join_vars: &BitSet,
        wmc: &WmcParams<T>,
    ) -> T
    where
        T: 'static,
    {
        let mut partial_join_acc = T::one();
        for lit in partial_join_assgn.assignment_iter() {
            let (l, h) = wmc.var_weight(lit.label());
            if lit.polarity() {
                partial_join_acc = partial_join_acc * (*h);
            } else {
                partial_join_acc = partial_join_acc * (*l);
            }
        }
        // top-down LB calculation via bdd_fold
        let v = self.bdd_fold(
            &|varlabel, low: T, high: T| {
                // get True and False weights for node VarLabel
                let (w_l, w_h) = wmc.var_weight(varlabel);
                // Check if our partial model has already assigned the node.
                match partial_join_assgn.get(varlabel) {
                    // If not...
                    None => {
                        // If it's a meet variable, (w_l * low) n (w_h * high)
                        if join_vars.contains(varlabel.value_usize()) {
                            let lhs = *w_l * low;
                            let rhs = *w_h * high;
                            MeetSemilattice::meet(&lhs, &rhs)
                        // Otherwise it is a sum variables, so
                        } else {
                            (*w_l * low) + (*w_h * high)
                        }
                    }
                    // If our node has already been assigned, then we
                    // reached a base case. We return the accumulated value.
                    Some(true) => high,
                    Some(false) => low,
                }
            },
            wmc.zero,
            wmc.one,
        );
        partial_join_acc * v
    }

    fn bb_h<T: BBSemiring>(
        &self,
        cur_lb: T,
        cur_best: PartialModel,
        join_vars: &[VarLabel],
        wmc: &WmcParams<T>,
        cur_assgn: PartialModel,
    ) -> (T, PartialModel)
    where
        T: 'static,
    {
        match join_vars {
            // If all join variables are assigned,
            [] => {
                // Run the bb_ub
                let empty_join_vars = BitSet::new();
                let possible_best = self.bb_ub(&cur_assgn, &empty_join_vars, wmc);
                // If it's a better lb, update.
                let best = BBSemiring::choose(&cur_lb, &possible_best);
                if cur_lb == best {
                    (cur_lb, cur_best)
                } else {
                    (possible_best, cur_assgn)
                }
            }
            // If there exists an unassigned decision variable,
            [x, end @ ..] => {
                let mut best_model = cur_best.clone();
                let mut best_lb = cur_lb;
                let join_vars_bits = BitSet::from_iter(end.iter().map(|x| x.value_usize()));
                // Consider the assignment of it to true...
                let mut true_model = cur_assgn.clone();
                true_model.set(*x, true);
                // ... and false...
                let mut false_model = cur_assgn;
                false_model.set(*x, false);

                // and calculate their respective upper bounds.
                let true_ub = self.bb_ub(&true_model, &join_vars_bits, wmc);
                let false_ub = self.bb_ub(&false_model, &join_vars_bits, wmc);

                // arbitrarily order the T/F bounds
                let order = if true_ub == BBSemiring::choose(&true_ub, &false_ub) {
                    [(true_ub, true_model), (false_ub, false_model)]
                } else {
                    [(false_ub, false_model), (true_ub, true_model)]
                };
                // the actual branching and bounding
                for (upper_bound, partialmodel) in order {
                    // if upper_bound == BBAlgebra::choose(&upper_bound, &best_lb) {
                    if !PartialOrd::le(&upper_bound, &cur_lb) {
                        let (rec, rec_pm) =
                            self.bb_h(best_lb, best_model.clone(), end, wmc, partialmodel.clone());
                        let new_lb = BBSemiring::choose(&cur_lb, &rec);
                        if new_lb == rec {
                            (best_lb, best_model) = (rec, rec_pm);
                        } else {
                            (best_lb, best_model) = (cur_lb, cur_best.clone());
                        }
                    }
                }
                (best_lb, best_model)
            }
        }
    }

    /// branch and bound generic over T a BBAlgebra.
    pub fn bb<T: BBSemiring>(
        &self,
        join_vars: &[VarLabel],
        num_vars: usize,
        wmc: &WmcParams<T>,
    ) -> (T, PartialModel)
    where
        T: 'static,
    {
        // Initialize all the decision variables to be true, partially instantianted resp. to this
        let all_true: Vec<Literal> = join_vars.iter().map(|x| Literal::new(*x, true)).collect();
        let cur_assgn = PartialModel::from_litvec(&all_true, num_vars);
        // Calculate bound wrt the partial instantiation.
        let lower_bound = self.bb_ub(&cur_assgn, &BitSet::new(), wmc);
        self.bb_h(
            lower_bound,
            cur_assgn,
            join_vars,
            wmc,
            PartialModel::from_litvec(&[], num_vars),
        )
    }

    /// performs a semantic hash and caches the result on the node
    pub fn cached_semantic_hash<const P: u128>(
        &self,
        order: &VarOrder,
        map: &WmcParams<FiniteField<P>>,
    ) -> FiniteField<P> {
        match self {
            PtrTrue => FiniteField::new(1),
            PtrFalse => FiniteField::new(0),
            Reg(node) => node.cached_semantic_hash(order, map),
            Compl(_) => self.neg().cached_semantic_hash(order, map).negate(),
        }
    }
}

type DDNNFCache<T> = (Option<T>, Option<T>);

impl<'a> DDNNFPtr<'a> for BddPtr<'a> {
    fn true_ptr() -> BddPtr<'a> {
        PtrTrue
    }

    fn false_ptr() -> BddPtr<'a> {
        PtrFalse
    }

    /// True is this is a complemented edge pointer
    fn is_neg(&self) -> bool {
        match &self {
            Compl(_) => true,
            Reg(_) => false,
            PtrTrue => false,
            PtrFalse => false,
        }
    }

    fn is_true(&self) -> bool {
        match &self {
            Compl(_) | Reg(_) | PtrFalse => false,
            PtrTrue => true,
        }
    }

    fn is_false(&self) -> bool {
        match &self {
            Compl(_) | Reg(_) | PtrTrue => false,
            PtrFalse => true,
        }
    }

    fn fold<T: Clone + Copy + Debug, F: Fn(DDNNF<T>) -> T>(&self, f: F) -> T
    where
        T: 'static,
    {
        debug_assert!(self.is_scratch_cleared());
        fn bottomup_pass_h<T: Clone + Copy + Debug, F: Fn(DDNNF<T>) -> T>(ptr: BddPtr, f: &F) -> T
        where
            T: 'static,
        {
            match ptr {
                PtrTrue => f(DDNNF::True),
                PtrFalse => f(DDNNF::False),
                Compl(node) | Reg(node) => {
                    // inside the cache, store a (compl, non_compl) pair corresponding to the
                    // complemented and uncomplemented pass over this node

                    // helper performs actual fold-and-cache work
                    let bottomup_helper = |cached| {
                        let (l, h) = if ptr.is_neg() {
                            (ptr.low_raw().neg(), ptr.high_raw().neg())
                        } else {
                            (ptr.low_raw(), ptr.high_raw())
                        };

                        let low_v = bottomup_pass_h(l, f);
                        let high_v = bottomup_pass_h(h, f);
                        let top = node.var;

                        let lit_high = f(DDNNF::Lit(top, true));
                        let lit_low = f(DDNNF::Lit(top, false));

                        let and_low = f(DDNNF::And(lit_low, low_v));
                        let and_high = f(DDNNF::And(lit_high, high_v));

                        // in a BDD, each decision only depends on the topvar
                        let mut varset = VarSet::new();
                        varset.insert(top);

                        let or_v = f(DDNNF::Or(and_low, and_high, varset));

                        // cache and return or_v
                        if ptr.is_neg() {
                            ptr.set_scratch::<DDNNFCache<T>>((Some(or_v), cached));
                        } else {
                            ptr.set_scratch::<DDNNFCache<T>>((cached, Some(or_v)));
                        }
                        or_v
                    };

                    match ptr.scratch::<DDNNFCache<T>>() {
                        // first, check if cached; explicit arms here for clarity
                        Some((Some(l), Some(h))) => {
                            if ptr.is_neg() {
                                l
                            } else {
                                h
                            }
                        }
                        Some((Some(v), None)) if ptr.is_neg() => v,
                        Some((None, Some(v))) if !ptr.is_neg() => v,
                        // no cached value found, compute it
                        Some((None, cached)) | Some((cached, None)) => bottomup_helper(cached),
                        None => bottomup_helper(None),
                    }
                }
            }
        }

        let r = bottomup_pass_h(*self, &f);
        self.clear_scratch();
        r
    }

    fn count_nodes(&self) -> usize {
        debug_assert!(self.is_scratch_cleared());
        fn count_h(ptr: BddPtr, count: &mut usize) {
            if ptr.is_const() {
                return;
            }
            match ptr.scratch::<usize>() {
                Some(_) => (),
                None => {
                    // found a new node
                    *count += 1;
                    ptr.set_scratch::<usize>(0);
                    count_h(ptr.low_raw(), count);
                    count_h(ptr.high_raw(), count);
                }
            }
        }
        let mut count = 0;
        count_h(*self, &mut count);
        self.clear_scratch();
        count
    }

    fn neg(&self) -> Self {
        match &self {
            Compl(x) => Reg(x),
            Reg(x) => Compl(x),
            PtrTrue => PtrFalse,
            PtrFalse => PtrTrue,
        }
    }
}

/// Core BDD node storage
#[derive(Debug)]
pub struct BddNode<'a> {
    pub var: VarLabel,
    pub low: BddPtr<'a>,
    pub high: BddPtr<'a>,
    /// scratch space used for caching data during traversals; ignored during
    /// equality checking and hashing
    data: RefCell<Option<Box<dyn Any>>>,
    semantic_hash: RefCell<Option<u128>>,
}

impl<'a> BddNode<'a> {
    pub fn new(var: VarLabel, low: BddPtr<'a>, high: BddPtr<'a>) -> BddNode<'a> {
        BddNode {
            var,
            low,
            high,
            data: RefCell::new(None),
            semantic_hash: RefCell::new(None),
        }
    }

    pub fn semantic_hash<const P: u128>(
        &self,
        order: &VarOrder,
        map: &WmcParams<FiniteField<P>>,
    ) -> FiniteField<P> {
        let (low_w, high_w) = map.var_weight(self.var);
        self.low.cached_semantic_hash(order, map) * (*low_w)
            + self.high.cached_semantic_hash(order, map) * (*high_w)
    }

    pub fn cached_semantic_hash<const P: u128>(
        &self,
        order: &VarOrder,
        map: &WmcParams<FiniteField<P>>,
    ) -> FiniteField<P> {
        if let Some(h) = *(self.semantic_hash.borrow()) {
            return FiniteField::new(h);
        }

        let h = self.semantic_hash(order, map);
        *(self.semantic_hash.borrow_mut()) = Some(h.value());

        h
    }
}

impl<'a> PartialEq for BddNode<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.var == other.var && self.low == other.low && self.high == other.high
    }
}

impl<'a> Hash for BddNode<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.var.hash(state);
        self.low.hash(state);
        self.high.hash(state);
    }
}

impl<'a> Eq for BddNode<'a> {}

impl<'a> PartialOrd for BddNode<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.var.partial_cmp(&other.var) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        match self.low.partial_cmp(&other.low) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        match self.high.partial_cmp(&other.high) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        Some(core::cmp::Ordering::Equal)
    }
}

impl<'a> Clone for BddNode<'a> {
    fn clone(&self) -> Self {
        Self {
            var: self.var,
            low: self.low,
            high: self.high,
            data: RefCell::new(None),
            semantic_hash: RefCell::new(None),
        }
    }
}

impl<'a> Ord for BddNode<'a> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}
