//! Binary decision diagram representation
use crate::{repr::var_label::VarSet, util::semiring::RealSemiring, util::semiring::ExpectedUtility};

pub use super::{
    ddnnf::*,
    model::PartialModel,
    var_label::{Literal, VarLabel},
    var_order::VarOrder,
    wmc::WmcParams,
};
use core::fmt::Debug;
use std::{iter::FromIterator};

/// Core BDD pointer datatype
#[derive(Debug, Clone, Eq, PartialEq, Hash, Copy, PartialOrd, Ord)]
pub enum BddPtr {
    Compl(*mut BddNode),
    Reg(*mut BddNode),
    PtrTrue,
    PtrFalse,
}

use bit_set::BitSet;
use bumpalo::Bump;
use BddPtr::*;

/// The intermediate representation for a BddPtr that is being folded in a
/// [`Fold`] computation.
///
/// - [`FoldNode::parent_is_compl`]: tells you if the parent node was a [`BddPtr::Compl`].
/// - [`FoldNode::node`]: gives you the pointer to the current node.
/// - [`FoldNode::var`]: gives you the [`VarLabel`] if the node is a [`BddPtr::Reg`].
pub struct FoldNode {
    pub parent_is_compl: bool,
    pub node: BddPtr,
    pub var: Option<VarLabel>,
}
impl FoldNode {
    fn new(node: BddPtr, parent_is_compl: bool, var: Option<VarLabel>) -> Self {
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
            BddPtr::Compl(n) => self.mut_fold_h(&BddPtr::Reg(*n), true, r),
            BddPtr::Reg(n) => unsafe {
                let l_p = (*(*n)).low;
                let r_p = (*(*n)).high;

                let lbl = (*(*n)).var;
                let t = (self.step)(r.clone(), FoldNode::new(*bdd, p_compl, Some(lbl)));
                let l_r = self.mut_fold_h(&l_p, false, &r.clone());
                let r_r = self.mut_fold_h(&r_p, false, &r.clone());
                (self.extract)(t, Some((l_r, r_r)))
            },
        }
    }
    /// A mutable fold. An example of how to use this fold can be seen by collecting all VarLabels in the sub-tree referenced by a given [`BddPtr`]:
    ///
    /// ```rust
    /// use rsdd::repr::bdd::*;
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

impl BddPtr {
    #[inline]
    pub fn new_reg(n: *mut BddNode) -> BddPtr {
        Reg(n)
    }

    #[inline]
    pub fn from_bool(b: bool) -> BddPtr {
        if b {
            PtrTrue
        } else {
            PtrFalse
        }
    }

    #[inline]
    pub fn new_compl(n: *mut BddNode) -> BddPtr {
        Compl(n)
    }

    /// Gets the varlabel of &self
    /// Panics if not a node
    #[inline]
    pub fn var(&self) -> VarLabel {
        self.into_node().var
    }
    /// Gets the varlabel of &self
    #[inline]
    pub fn var_safe(&self) -> Option<VarLabel> {
        self.into_node_safe().and_then(|x| Some(x.var))
    }

    /// Get a mutable reference to the node that &self points to
    ///
    /// Panics if not a node pointer
    #[inline]
    #[allow(clippy::mut_from_ref)]
    pub fn mut_node_ref(&self) -> &mut BddNode {
        unsafe {
            match &self {
                Reg(x) => &mut (**x),
                Compl(x) => &mut (**x),
                _ => panic!("Dereferencing constant in deref_or_panic"),
            }
        }
    }

    /// Dereferences the BddPtr into a BddNode
    /// The pointer is returned in regular-form (i.e., if &self is complemented, then
    /// the returned BddNode incorporates this information)
    ///
    /// Panics if the pointer is constant (i.e., true or false)
    #[inline]
    pub fn into_node(&self) -> BddNode {
        match self.into_node_safe() {
            None => panic!("Dereferencing constant in deref_or_panic"),
            Some(n) => n,
        }
    }

    /// Dereferences the BddPtr into a BddNode
    /// The pointer is returned in regular-form (i.e., if &self is complemented, then
    /// the returned BddNode incorporates this information)
    pub fn into_node_safe(&self) -> Option<BddNode> {
        unsafe {
            match &self {
                Reg(x) => Some((**x).clone()),
                Compl(x) => {
                    let BddNode {
                        var,
                        low,
                        high,
                        data,
                    } = **x;
                    Some(BddNode {
                        var,
                        low: low.neg(),
                        high: high.neg(),
                        data,
                    })
                }
                _ => None,
            }
        }
    }

    /// convert a BddPtr into a regular (non-complemented) pointer
    pub fn to_reg(&self) -> BddPtr {
        match &self {
            Compl(x) => Reg(*x),
            Reg(x) => Reg(*x),
            PtrTrue => PtrTrue,
            PtrFalse => PtrTrue,
        }
    }

    pub fn low(&self) -> BddPtr {
        unsafe {
            match &self {
                Compl(x) => (**x).low.neg(),
                Reg(x) => (**x).low,
                PtrTrue | PtrFalse => panic!("deref constant BDD"),
            }
        }
    }

    pub fn low_raw(&self) -> BddPtr {
        unsafe {
            match &self {
                Compl(x) => (**x).low,
                Reg(x) => (**x).low,
                PtrTrue | PtrFalse => panic!("deref constant BDD"),
            }
        }
    }

    pub fn high_raw(&self) -> BddPtr {
        unsafe {
            match &self {
                Compl(x) => (**x).high,
                Reg(x) => (**x).high,
                PtrTrue | PtrFalse => panic!("deref constant BDD"),
            }
        }
    }

    pub fn high(&self) -> BddPtr {
        unsafe {
            match &self {
                Compl(x) => (**x).high.neg(),
                Reg(x) => (**x).high,
                PtrTrue | PtrFalse => panic!("deref constant BDD"),
            }
        }
    }

    /// gets the raw pointer that this BDD points to
    /// panics if not a ptr
    pub fn ptr_raw(&self) -> *mut BddNode {
        match &self {
            Compl(x) => *x,
            Reg(x) => *x,
            PtrTrue | PtrFalse => panic!("deref constant BDD"),
        }
    }

    /// Traverses the BDD and clears all scratch memory (sets it equal to 0)
    pub fn clear_scratch(&self) {
        if !self.is_const() {
            let n = self.mut_node_ref();
            if n.data != 0 {
                n.data = 0;
                n.low.clear_scratch();
                n.high.clear_scratch();
            }
        }
    }

    /// true if the BddPtr points to a constant (i.e., True or False)
    pub fn is_const(&self) -> bool {
        match &self {
            Compl(_) => false,
            Reg(_) => false,
            PtrTrue => true,
            PtrFalse => true,
        }
    }

    /// Gets the scratch value stored in `&self`
    ///
    /// Panics if not node.
    pub fn get_scratch<T>(&self) -> Option<&T> {
        unsafe {
            let ptr = self.mut_node_ref().data;
            if ptr == 0 {
                None
            } else {
                Some(&*(self.into_node().data as *const T))
            }
        }
    }

    /// Set the scratch in this node to the value `v`.
    ///
    /// Panics if not a node.
    ///
    /// Invariant: values stored in `set_scratch` must not outlive
    /// the provided allocator `alloc` (i.e., calling `get_scratch`
    /// involves dereferencing a pointer stored in `alloc`)
    pub fn set_scratch<T>(&self, alloc: &mut Bump, v: T) {
        self.mut_node_ref().data = (alloc.alloc(v) as *const T) as usize;
    }

    /// true if the scratch is current cleared
    pub fn is_scratch_cleared(&self) -> bool {
        if !self.is_const() {
            self.mut_node_ref().data == 0
        } else {
            true
        }
    }

    pub fn to_string_debug(&self) -> String {
        fn print_bdd_helper(ptr: BddPtr) -> String {
            if ptr.is_true() {
                String::from("T")
            } else if ptr.is_false() {
                String::from("F")
            } else {
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
                format!("({}, {}, {})", ptr.var().value(), h_s, l_s)
            }
        }
        print_bdd_helper(*self)
    }

    /// Print a debug form of the BDD with the label remapping given by `map`
    pub fn print_bdd_lbl(&self, map: &HashMap<VarLabel, VarLabel>) -> String {
        match self {
            BddPtr::PtrTrue => String::from("T"),
            // BddPtr::PtrFalse => String::from("T"),
            BddPtr::PtrFalse => String::from("F"), // TODO: check that this is right?
            BddPtr::Compl(n) => {
                let s = BddPtr::Reg(*n).print_bdd_lbl(map);
                format!("!{}", s)
            }
            BddPtr::Reg(n) => unsafe {
                let l_p = (*(*n)).low;
                let r_p = (*(*n)).high;
                let l_s = l_p.print_bdd_lbl(map);
                let r_s = r_p.print_bdd_lbl(map);
                let lbl = (*(*n)).var;
                format!(
                    "({:?}, {}, {})",
                    map.get(&lbl).unwrap_or(&lbl).value(),
                    l_s,
                    r_s
                )
            },
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
        alloc: &mut Bump,
    ) -> T {
        // If current node is true leaf, return accumulated high_v value
        if self.is_true() {
            high_v
        // Else if current node is false leaf, return accumulated high_v value
        } else if self.is_false() {
            low_v
        } else {
            // Every node has an associated cache (scratch)
            // Scratch data is arbitrary (depends on use case)
            // fst is negated, snd is non-negated accumulator
            if self.get_scratch::<(Option<T>, Option<T>)>().is_none() {
                self.set_scratch::<(Option<T>, Option<T>)>(alloc, (None, None));
            }
            // Actual case
            match self.get_scratch::<(Option<T>, Option<T>)>() {
                // If complemented and accumulated, use the already memoized value
                Some((Some(v), _)) if self.is_neg() => *v,
                // Same for not complemented
                Some((_, Some(v))) if !self.is_neg() => *v,
                // (None, None), 
                // (Some(v), None) but not a complemented node
                // (None, Some(v)) but a complemented node
                Some((prev_low, prev_high)) => {
                    // Standard fold stuff
                    let l = self.low().bdd_fold_h(f, low_v, high_v, alloc);
                    let h = self.high().bdd_fold_h(f, low_v, high_v, alloc);
                    let res = f(self.var(), l, h);
                    // Set cache (accumulator)
                    // Then corrects scratch so it traverses correctly in a recursive case downstream
                    if self.is_neg() {
                        self.set_scratch::<(Option<T>, Option<T>)>(alloc, (Some(res), *prev_high));
                    } else {
                        self.set_scratch::<(Option<T>, Option<T>)>(alloc, (*prev_low, Some(res)));
                    }
                    res
                }
                _ => panic!("unreachable"),
            }
        }
    }

    pub fn bdd_fold<T: Clone + Copy + Debug, F: Fn(VarLabel, T, T) -> T>(
        &self,
        f: &F,
        low_v: T,
        high_v: T,
    ) -> T {
        let r = self.bdd_fold_h(f, low_v, high_v, &mut Bump::new());
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
                let (low_w, high_w) = wmc.get_var_weight(varlabel);
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
            let (l, h) = wmc.get_var_weight(lit.get_label());
            if lit.get_polarity() {
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
                #[allow(clippy::redundant_clone)]
                // TODO: remove this, it seems like it's a reasonable lint
                let mut false_model = cur_assgn.clone();
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
        let mut marg_vars = BitSet::new();
        for v in vars {
            marg_vars.insert(v.value_usize());
        }

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
        println!("Assigned decision variables: {:?}", partial_decisions);

        // accumulator for EU via bdd_fold
        let mut v = self.bdd_fold(
            &|varlabel, low : ExpectedUtility, high : ExpectedUtility| {
                // get True and False weights for VarLabel
                let (false_w, true_w) = wmc.get_var_weight(varlabel);
                // Check if our partial model has already assigned my variable.
                match partial_decisions.get(varlabel) {
                    // If not...
                    None => {
                        // If it's a decision variable, we do  
                        if decision_vars.contains(varlabel.value_usize()) {
                            let max_pr = f64::max(low.0, high.0);
                            let max_eu = f64::max(low.1, high.1);
                            ExpectedUtility(max_pr,max_eu)
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
        );
        for lit in partial_decisions.assignment_iter() {
            let (l, h) = wmc.get_var_weight(lit.get_label());
            if lit.get_polarity() {
                v = v * (*h);
            } else {
                v = v * (*l);
            }
        }
        println!("{}, {}",v.0, v.1 );
        v
    }

    fn meu_h(
        &self,
        cur_lb: ExpectedUtility,
        cur_best: PartialModel,
        decision_vars: &[VarLabel],
        wmc: &WmcParams<ExpectedUtility>,
        cur_assgn: PartialModel,
    ) -> (ExpectedUtility, PartialModel) {
        match decision_vars {
            [] => {
                let decision_bitset = BitSet::new();
                let possible_best = self.eu_ub(&cur_assgn, &decision_bitset, wmc);
                if possible_best.1 > cur_lb.1 {
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

                let true_ub = self.eu_ub(&true_model, &margvar_bits, wmc);
                let false_ub = self.eu_ub(&false_model, &margvar_bits, wmc);

                // branch on the greater upper-bound first
                let order = if true_ub.0 > false_ub.0 {
                    [(true_ub, true_model), (false_ub, false_model)]
                } else {
                    [(false_ub, false_model), (true_ub, true_model)]
                };
                for (upper_bound, partialmodel) in order {
                    // branch + bound
                    if upper_bound.1 > best_lb.1 {
                        (best_lb, best_model) = self.meu_h(
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

    /// maximum expected utility calc
    pub fn meu(
        &self,
        decision_vars: &[VarLabel],
        num_vars: usize,
        wmc: &WmcParams<ExpectedUtility>,
    ) -> (ExpectedUtility, PartialModel) {
        // copying decision vars into a new mutable bitset
        // let mut decisions = BitSet::new();
        // for v in decision_vars {
        //     decisions.insert(v.value_usize());
        // }

        // Initialize all the decision variables to be true, partially instantianted resp. to this
        let all_true: Vec<Literal> = decision_vars.iter().map(|x| Literal::new(*x, true)).collect();
        let cur_assgn = PartialModel::from_litvec(&all_true, num_vars);
        // Calculate bound wrt the partial instantiation.
        let lower_bound = self.eu_ub(&cur_assgn, &BitSet::new(), wmc);
        self.meu_h(
            lower_bound,
            cur_assgn,
            decision_vars,
            wmc,
            PartialModel::from_litvec(&[], num_vars),
        )
    }
}

type DDNNFCache<T> = (Option<T>, Option<T>);

impl DDNNFPtr for BddPtr {
    type Order = VarOrder;

    fn true_ptr() -> BddPtr {
        PtrTrue
    }

    fn false_ptr() -> BddPtr {
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

    // TODO: we should be able to remove this; e.g. replace v.clone() with *v
    #[allow(clippy::clone_on_copy)]
    fn fold<T: Clone + Copy + Debug, F: Fn(DDNNF<T>) -> T>(&self, _o: &VarOrder, f: F) -> T {
        debug_assert!(self.is_scratch_cleared());
        fn bottomup_pass_h<T: Clone + Copy + Debug, F: Fn(DDNNF<T>) -> T>(
            ptr: BddPtr,
            f: &F,
            alloc: &mut Bump,
        ) -> T {
            match ptr {
                PtrTrue => f(DDNNF::True),
                PtrFalse => f(DDNNF::False),
                Compl(_) | Reg(_) => {
                    // inside the cache, store a (compl, non_compl) pair corresponding to the
                    // complemented and uncomplemented pass over this node
                    if ptr.get_scratch::<DDNNFCache<T>>().is_none() {
                        ptr.set_scratch::<DDNNFCache<T>>(alloc, (None, None));
                    }
                    match ptr.get_scratch::<DDNNFCache<T>>() {
                        Some((Some(v), _)) if ptr.is_neg() => v.clone(),
                        Some((_, Some(v))) if !ptr.is_neg() => v.clone(),
                        Some((None, cached)) | Some((cached, None)) => {
                            // no cached value found, compute it
                            let l = if ptr.is_neg() {
                                ptr.low_raw().neg()
                            } else {
                                ptr.low_raw()
                            };
                            let h = if ptr.is_neg() {
                                ptr.high_raw().neg()
                            } else {
                                ptr.high_raw()
                            };

                            let low_v = bottomup_pass_h(l, f, alloc);
                            let high_v = bottomup_pass_h(h, f, alloc);
                            let top = ptr.var();

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
                                ptr.set_scratch::<DDNNFCache<T>>(alloc, (Some(or_v), *cached));
                            } else {
                                ptr.set_scratch::<DDNNFCache<T>>(alloc, (*cached, Some(or_v)));
                            }
                            or_v
                        }
                        _ => panic!("unreachable"),
                    }
                }
            }
        }

        let mut alloc = Bump::new();
        let r = bottomup_pass_h(*self, &f, &mut alloc);
        self.clear_scratch();
        r
    }

    fn count_nodes(&self) -> usize {
        debug_assert!(self.is_scratch_cleared());
        fn count_h(ptr: BddPtr, alloc: &mut Bump) -> usize {
            if ptr.is_const() {
                return 0;
            }
            match ptr.get_scratch::<usize>() {
                Some(_) => 0,
                None => {
                    // found a new node
                    ptr.set_scratch::<usize>(alloc, 0);
                    let sub_l = count_h(ptr.low_raw(), alloc);
                    let sub_h = count_h(ptr.high_raw(), alloc);
                    sub_l + sub_h + 1
                }
            }
        }
        let r = count_h(*self, &mut Bump::new());
        self.clear_scratch();
        return r;
    }

    fn neg(&self) -> Self {
        match &self {
            Compl(x) => Reg(*x),
            Reg(x) => Compl(*x),
            PtrTrue => PtrFalse,
            PtrFalse => PtrTrue,
        }
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
    data: usize,
}

impl BddNode {
    pub fn new(var: VarLabel, low: BddPtr, high: BddPtr) -> BddNode {
        BddNode {
            var,
            low,
            high,
            data: 0,
        }
    }
}

impl PartialEq for BddNode {
    fn eq(&self, other: &Self) -> bool {
        self.var == other.var && self.low == other.low && self.high == other.high
    }
}

use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
};
impl Hash for BddNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.var.hash(state);
        self.low.hash(state);
        self.high.hash(state);
    }
}
