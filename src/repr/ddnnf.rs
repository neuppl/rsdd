//! Implementing of a generic decision decomposable deterministic negation normal form
//! (d-DNNF) pointer type
use crate::util::semirings::semiring_traits::Semiring;
use crate::util::semirings::{finitefield::FiniteField, semiring_traits::BBSemiring};
use bit_set::BitSet;
use core::fmt::Debug;
use rand::{Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;

/// creates a weighting that can be used for semantically hashing a DDNNF node
/// the constant `P` denotes the size of the field over which the semantic hash will
/// be computed. For more info, see <https://tr.inf.unibe.ch/pdf/iam-06-001.pdf>
pub fn create_semantic_hash_map<const P: u128>(num_vars: usize) -> WmcParams<FiniteField<P>> {
    let vars: Vec<VarLabel> = (0..num_vars).map(VarLabel::new_usize).collect();

    // theoretical guarantee from paper; need to verify more!
    // in "theory", this should be a 0.1% fail rate in one-shot for a BDD.
    // not sure how to extend to SDDs (and this does not happen in practice)
    assert!(((vars.len() * 1000) as u128) < P);

    // seed the RNG deterministically for reproducible weights across
    // different calls to `create_semantic_hash_map`
    let mut rng = ChaCha8Rng::seed_from_u64(101249);
    // let mut rng = ChaCha8Rng::from_entropy();

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

use super::model::PartialModel;
use super::var_label::Literal;
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

pub trait DDNNFPtr<'a>: Clone + Debug + PartialEq + Eq + Hash + Copy {
    /// A generic Ordering type
    /// For BDDs, this is a VarOrder
    /// For SDDs, this is a VTree
    /// For decisionDNNF, this is a DTree
    type Order;

    /// performs a memoized bottom-up pass with aggregating function `f` calls
    fn fold<T: Semiring, F: Fn(DDNNF<T>) -> T>(&self, o: &Self::Order, f: F) -> T
    where
        T: 'static;

    /// Weighted-model count
    fn wmc<T: Semiring + std::ops::Add<Output = T> + std::ops::Mul<Output = T>>(
        &self,
        o: &Self::Order,
        params: &WmcParams<T>,
    ) -> T
    where
        T: 'static,
    {
        self.fold(o, |ddnnf| {
            use DDNNF::*;
            match ddnnf {
                Or(l, r, _) => l+r,
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

    // upper-bound as seen in the branch-and-bound function
    fn ub<T: BBSemiring>(
        &self,
        o: &Self::Order,
        params: &WmcParams<T>,
        partial_join_assgn: &PartialModel,
        join_vars: &BitSet,
    ) -> T
    where
        T: 'static,
    {
        println!("{:?}", self);
        let mut partial_join_acc = T::one();
        for lit in partial_join_assgn.assignment_iter() {
            let (l, h) = params.get_var_weight(lit.get_label());
            if lit.get_polarity() {
                partial_join_acc = partial_join_acc * (*h);
            } else {
                partial_join_acc = partial_join_acc * (*l);
            }
        }
        let v = self.fold(o, |ddnnf: DDNNF<T>| {
            use DDNNF::*;
            match ddnnf {
                Or(l, r, vars) => {
                    // if C is associated with a MAP variable,
                    // take the join.
                    println!("{:?}", vars);
                    let mut ans = false ;
                    for vl in vars.iter() {
                        if join_vars.contains(vl.value_usize()) {ans = true;};
                    }
                    if ans {
                        T::join(&l, &r)
                    } else {
                        l+r
                    }
                }
                And(l, r) => l * r,
                True => params.one,
                False => params.zero,
                Lit(lbl, polarity) => {
                    let x = partial_join_assgn.get(lbl);
                    if x == Some(true) || x == Some(false) {
                        if x.unwrap() != polarity {
                            println!("wow this hit");
                            return params.zero;
                        } else if x.unwrap() == polarity {
                            return params.one;
                        }
                    }
                    let (low_w, high_w) = params.get_var_weight(lbl);
                    if polarity {
                        *high_w
                    } else {
                        *low_w
                    }
                }
            }
        });
        partial_join_acc * v
    }

    // fn bb_h<T: BBSemiring>(
    //     &self,
    //     o: &Self::Order,
    //     cur_lb: T,
    //     cur_best: PartialModel,
    //     join_vars: &[VarLabel],
    //     wmc: &WmcParams<T>,
    //     cur_assgn: PartialModel,
    // ) -> (T, PartialModel)
    // where
    //     T: 'static,
    // {
    //     match join_vars {
    //         // If all join variables are assigned,
    //         [] => {
    //             // Run the ub
    //             let empty_join_vars = VarSet::new();
    //             let possible_best = self.ub(o, wmc, &cur_assgn, &empty_join_vars);
    //             // If it's a better lb, update.
    //             let best = BBSemiring::choose(&cur_lb, &possible_best);
    //             if cur_lb == best {
    //                 (cur_lb, cur_best)
    //             } else {
    //                 (possible_best, cur_assgn)
    //             }
    //         }
    //         // If there exists an unassigned decision variable,
    //         [x, end @ ..] => {
    //             let mut best_model = cur_best.clone();
    //             let mut best_lb = cur_lb;
    //             let join_vars_bits =
    //                 VarSet::new_from_bitset(BitSet::from_iter(end.iter().map(|x| x.value_usize())));
    //             // Consider the assignment of it to true...
    //             let mut true_model = cur_assgn.clone();
    //             true_model.set(*x, true);
    //             // ... and false...
    //             let mut false_model = cur_assgn;
    //             false_model.set(*x, false);

    //             // and calculate their respective upper bounds.
    //             let true_ub = self.ub(o, wmc, &true_model, &join_vars_bits);
    //             let false_ub = self.ub(o, wmc, &false_model, &join_vars_bits);

    //             // arbitrarily order the T/F bounds
    //             let order = if true_ub == BBSemiring::choose(&true_ub, &false_ub) {
    //                 [(true_ub, true_model), (false_ub, false_model)]
    //             } else {
    //                 [(false_ub, false_model), (true_ub, true_model)]
    //             };
    //             // the actual branching and bounding
    //             for (upper_bound, partialmodel) in order {
    //                 // if upper_bound == BBAlgebra::choose(&upper_bound, &best_lb) {
    //                 if !PartialOrd::le(&upper_bound, &cur_lb) {
    //                     let (rec, rec_pm) = self.bb_h(
    //                         o,
    //                         best_lb,
    //                         best_model.clone(),
    //                         end,
    //                         wmc,
    //                         partialmodel.clone(),
    //                     );
    //                     let new_lb = BBSemiring::choose(&cur_lb, &rec);
    //                     if new_lb == rec {
    //                         (best_lb, best_model) = (rec, rec_pm);
    //                     } else {
    //                         (best_lb, best_model) = (cur_lb, cur_best.clone());
    //                     }
    //                 }
    //             }
    //             (best_lb, best_model)
    //         }
    //     }
    // }

    // /// branch and bound generic over T a BBAlgebra.
    // fn bb<T: BBSemiring>(
    //     &self,
    //     o: &Self::Order,
    //     join_vars: &[VarLabel],
    //     num_vars: usize,
    //     wmc: &WmcParams<T>,
    // ) -> (T, PartialModel)
    // where
    //     T: 'static,
    // {
    //     // Initialize all the decision variables to be true, partially instantianted resp. to this
    //     let all_true: Vec<Literal> = join_vars.iter().map(|x| Literal::new(*x, true)).collect();
    //     let cur_assgn = PartialModel::from_litvec(&all_true, num_vars);
    //     // Calculate bound wrt the partial instantiation.
    //     let lower_bound = self.ub(o, wmc, &cur_assgn, &VarSet::new());
    //     self.bb_h(
    //         o,
    //         lower_bound,
    //         cur_assgn,
    //         join_vars,
    //         wmc,
    //         PartialModel::from_litvec(&[], num_vars),
    //     )
    // }
}
