//! Implementing of a generic decision decomposable deterministic negation normal form
//! (d-DNNF) pointer type
use core::fmt::Debug;

use crate::util::semiring::{FiniteField, Semiring};
use rand::{Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;

/// creates a weighting that can be used for semantically hashing a DDNNF node
/// the constant `P` denotes the size of the field over which the semantic hash will
/// be computed. For more info, see <https://tr.inf.unibe.ch/pdf/iam-06-001.pdf>
pub fn create_semantic_hash_map<const P: u128>(num_vars: usize) -> WmcParams<FiniteField<P>> {
    let vars: Vec<VarLabel> = (0..num_vars).map(|x| VarLabel::new_usize(x)).collect();

    // theoretical guarantee from paper; need to verify more!
    assert!((2 * vars.len() as u128) < P);

    // seed the RNG deterministically for reproducible weights across
    // different calls to `create_semantic_hash_map`
    // let mut rng = ChaCha8Rng::seed_from_u64(101249);
    let mut rng = ChaCha8Rng::from_entropy();

    let value_range: Vec<(FiniteField<P>, FiniteField<P>)> = (0..vars.len() as u128)
        .map(|_| {
            let h = FiniteField::new(rng.gen_range(2..P));
            let l = FiniteField::new(P - h.value() + 1);
            (l, h)
        })
        .collect();

    let mut map = HashMap::new();

    for (&var, &value) in vars.iter().zip(value_range.iter()) {
        map.insert(var, value);
    }

    WmcParams::new_with_default(FiniteField::zero(), FiniteField::one(), map)
}

use super::{
    var_label::{VarLabel, VarSet},
    wmc::WmcParams,
};
use std::{collections::HashMap, hash::Hash};
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
    fn wmc<T: Semiring + std::ops::Add<Output = T> + std::ops::Mul<Output = T>>(
        &self,
        o: &Self::Order,
        params: &WmcParams<T>,
    ) -> T {
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

    /// compute the semantic hash for this pointer
    fn semantic_hash<const P: u128>(
        &self,
        order: &Self::Order,
        map: &WmcParams<FiniteField<P>>,
    ) -> FiniteField<P> {
        self.wmc(order, map)
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
