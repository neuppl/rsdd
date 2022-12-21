//! Implementing of a generic decision decomposable deterministic negation normal form
//! (d-DNNF) pointer type
use core::fmt::Debug;
use std::collections::HashMap;
use num::Num;

use super::{var_label::{VarLabel, VarSet}, wmc::WmcParams};

/// A base d-DNNF type
pub enum DDNNF<T> {
    /// contains the cached values for the children, and the VarSet that
    /// contains the set of decision variables that this Or node was made with
    Or(T, T),
    And(T, T),
    Lit(VarLabel, bool),
    True,
    False,
}

pub trait DDNNFPtr {
    /// performs a memoized bottom-up pass with aggregating function `f` calls
    fn fold<T: Clone + Copy + Debug, F: Fn(DDNNF<T>) -> T>(&self, f: F) -> T;

    /// Weighted-model count
    fn wmc<T: Num + Clone + Debug + Copy>(&self, params: &WmcParams<T>) -> T {
        self.fold(|ddnnf| {
            use DDNNF::*;
            match ddnnf {
                Or(l, r) => l + r,
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

    fn eval(&self, assgn: &HashMap<VarLabel, bool>) -> bool {
        todo!()
    }

    /// count the number of nodes in this representation
    fn count_nodes(&self) -> usize;
}
