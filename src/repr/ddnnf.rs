//! Implementing of a generic decision decomposable deterministic negation normal form
//! (d-DNNF) pointer type
use core::fmt::Debug;
use num::Num;

use crate::util::semiring::Semiring;

use super::{
    var_label::{VarLabel, VarSet},
    wmc::WmcParams,
};
use std::hash::Hash;
/// A base d-DNNF type
pub enum DDNNF<T> {
    /// contains the cached values for the children, and the VarSet that
    /// contains the set of decision variables that this Or node was made with
    Or(T, T, VarSet),
    And(T, T),
    Lit(VarLabel, bool),
    True,
    False,
}

pub trait DDNNFPtr: Clone + Debug + PartialEq + Eq + Hash + Copy {
    /// A generic Ordering type
    /// For BDDs, this is a VarOrder
    /// For SDDs, this is a VTree
    /// For decisionDNNF, this is a DTree
    type Order;

    /// performs a memoized bottom-up pass with aggregating function `f` calls
    fn fold<T: Semiring, F: Fn(DDNNF<T>) -> T>(&self, o: &Self::Order, f: F) -> T;

    /// Weighted-model count
    fn wmc<T: Semiring  + std::ops::Add<Output = T> + std::ops::Mul<Output = T>>(&self, o: &Self::Order, params: &WmcParams<T>) -> T {
        self.fold(o, |ddnnf| {
            use DDNNF::*;
            match ddnnf {
                Or(l, r, _) => l + r,
                And(l, r) => l * r,
                True => params.one,
                False => params.zero,
                Lit(lbl, polarity) => {
                    let (low_w, high_w) = params.get_var_weight(lbl);
                    if polarity {
                        *high_w
                    } else {
                        *low_w
                    }
                }
            }
        })
    }

    /// Negate the pointer
    fn neg(&self) -> Self;
    /// Generate a pointer to the false constant
    fn false_ptr() -> Self;
    /// Generate a pointer to the true constant
    fn true_ptr() -> Self;
    /// True if `self` is a true constant, false otherwise
    fn is_true(&self) -> bool;
    /// True if `self` is a false constant, false otherwise
    fn is_false(&self) -> bool;
    /// True if `self` is a negated pointer, false otherwise
    fn is_neg(&self) -> bool;

    /// count the number of nodes in this representation
    fn count_nodes(&self) -> usize;
}
