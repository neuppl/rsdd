use crate::{
    repr::{Literal, VarLabel},
    util::semirings::Semiring,
};
use core::fmt::Debug;
use std::collections::HashMap;
/// Weighted model counting parameters for a BDD. It primarily is a storage for
/// the weight on each variable.
#[repr(C)]
#[derive(Clone)]
pub struct WmcParams<T: Semiring> {
    pub zero: T,
    pub one: T,
    /// a vector which maps variable labels to `(low, high)`
    /// valuations.
    var_to_val: Vec<Option<(T, T)>>,
}

impl<T: Semiring> WmcParams<T> {
    /// Parametrize a weighted model count (over a semiring) with default weights.
    /// The weights are a mapping from variables to (negative, positive) weights.
    /// ```
    /// use rsdd::repr::{Literal, VarLabel};
    /// use rsdd::repr::WmcParams;
    /// use rsdd::util::semirings::{Semiring, RealSemiring};
    /// use std::collections::HashMap;
    ///
    /// let weights = HashMap::from([
    ///     (VarLabel::new(0), (RealSemiring(0.0), RealSemiring(1.0))),
    ///     (VarLabel::new(1), (RealSemiring(0.3), RealSemiring(0.7)))
    /// ]);
    ///
    /// let params = WmcParams::<RealSemiring>::new(weights);
    ///
    /// let all_true = [
    ///     Literal::new(VarLabel::new(0), true),
    ///     Literal::new(VarLabel::new(1), true),
    /// ];
    ///
    /// assert_eq!(params.assignment_weight(&all_true).0, 0.7)
    /// ```
    pub fn new(var_to_val: HashMap<VarLabel, (T, T)>) -> WmcParams<T> {
        let mut var_to_val_vec: Vec<Option<(T, T)>> = vec![None; var_to_val.len()];
        for (key, value) in var_to_val.iter() {
            var_to_val_vec[key.value_usize()] = Some(*value);
        }
        WmcParams {
            zero: T::zero(),
            one: T::one(),
            var_to_val: var_to_val_vec,
        }
    }

    /// get the weight of an asignment
    /// ```
    /// use rsdd::repr::{Literal, VarLabel};
    /// use rsdd::repr::WmcParams;
    /// use rsdd::util::semirings::{Semiring, RealSemiring};
    /// use std::collections::HashMap;
    ///
    /// let weights = HashMap::from([
    ///     (VarLabel::new(0), (RealSemiring(0.0), RealSemiring(1.0))),
    ///     (VarLabel::new(1), (RealSemiring(0.3), RealSemiring(0.7)))
    /// ]);
    ///
    /// let params = WmcParams::<RealSemiring>::new(weights);
    ///
    /// let all_true = [
    ///     Literal::new(VarLabel::new(0), true),
    ///     Literal::new(VarLabel::new(1), true),
    /// ];
    ///
    /// assert_eq!(params.assignment_weight(&all_true).0, 0.7)
    /// ```
    pub fn assignment_weight(&self, assgn: &[Literal]) -> T {
        let mut prod = self.one;
        for lit in assgn.iter() {
            if lit.polarity() {
                prod = prod * self.var_to_val[lit.label().value_usize()].unwrap().1
            } else {
                prod = prod * self.var_to_val[lit.label().value_usize()].unwrap().0
            }
        }
        prod
    }

    /// ```
    /// use rsdd::repr::{Literal, VarLabel};
    /// use rsdd::repr::WmcParams;
    /// use rsdd::util::semirings::{Semiring, RealSemiring};
    /// use std::collections::HashMap;
    ///
    /// let weights = HashMap::from([
    ///     (VarLabel::new(0), (RealSemiring(0.0), RealSemiring(1.0))),
    ///     (VarLabel::new(1), (RealSemiring(0.3), RealSemiring(0.7)))
    /// ]);
    ///
    /// let mut params = WmcParams::<RealSemiring>::new(weights);
    ///
    /// let all_true = [
    ///     Literal::new(VarLabel::new(0), true),
    ///     Literal::new(VarLabel::new(1), true),
    /// ];
    ///
    /// assert_eq!(params.assignment_weight(&all_true).0, 0.7);
    ///
    /// params.set_weight(VarLabel::new(1), RealSemiring(0.5), RealSemiring(0.5));
    /// assert_eq!(params.assignment_weight(&all_true).0, 0.5);
    /// ```
    pub fn set_weight(&mut self, lbl: VarLabel, low: T, high: T) {
        let n = lbl.value_usize();
        while n >= self.var_to_val.len() {
            self.var_to_val.push(None);
        }
        self.var_to_val[n] = Some((low, high));
    }

    /// ```
    /// use rsdd::repr::VarLabel;
    /// use rsdd::repr::WmcParams;
    /// use rsdd::util::semirings::{Semiring, RealSemiring};
    /// use std::collections::HashMap;
    ///
    /// let weights = HashMap::from([
    ///     (VarLabel::new(0), (RealSemiring(0.0), RealSemiring(1.0))),
    ///     (VarLabel::new(1), (RealSemiring(0.3), RealSemiring(0.7)))
    /// ]);
    ///
    /// let params = WmcParams::<RealSemiring>::new(weights);
    ///
    /// assert_eq!(*params.var_weight(VarLabel::new(1)), (RealSemiring(0.3), RealSemiring(0.7)))
    /// ```
    // gives you the weight of `(low, high)` literals for a given VarLabel
    pub fn var_weight(&self, label: VarLabel) -> &(T, T) {
        return (self.var_to_val[label.value_usize()]).as_ref().unwrap();
    }

    /// ```
    /// use rsdd::repr::VarLabel;
    /// use rsdd::repr::WmcParams;
    /// use rsdd::util::semirings::{Semiring, RealSemiring};
    /// use std::collections::HashMap;
    ///
    /// let weights = HashMap::from([
    ///     (VarLabel::new(0), (RealSemiring(0.0), RealSemiring(1.0))),
    ///     (VarLabel::new(1), (RealSemiring(0.3), RealSemiring(0.7)))
    /// ]);
    ///
    /// let params = WmcParams::<RealSemiring>::new(weights);
    ///
    /// assert!(params.has_smoothed_weights());
    ///
    /// let weights = HashMap::from([
    ///     (VarLabel::new(0), (RealSemiring(1.0), RealSemiring(1.0))),
    ///     (VarLabel::new(1), (RealSemiring(1.0), RealSemiring(1.0)))
    /// ]);
    ///
    /// let params = WmcParams::<RealSemiring>::new(weights);
    ///
    /// assert!(!params.has_smoothed_weights());
    /// ```
    /// returns whether or not the weights within the parameters are normalized,
    /// i.e. each true/false weight pair sums to the `One` defined in the Semiring
    pub fn has_smoothed_weights(&self) -> bool {
        self.var_to_val.iter().all(|v| match v {
            Some((low, high)) => *low + *high == T::one(),
            None => todo!(),
        })
    }
}

impl<T: Semiring> Debug for WmcParams<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WmcParams")
            .field("zero", &self.zero)
            .field("one", &self.one)
            .field(
                "var_to_val",
                &self
                    .var_to_val
                    .iter()
                    .enumerate()
                    .map(|(index, val)| {
                        if let Some((low, high)) = val {
                            format!("{}: l: {:?}, h: {:?}", index, low, high)
                        } else {
                            format!("{}: None", index)
                        }
                    })
                    .collect::<Vec<String>>(),
            )
            .finish()
    }
}

impl<T: Semiring> Default for WmcParams<T> {
    /// Parametrize a weighted model count (over a semiring) with no default assocations;
    /// requires weights to be set before performing the count.
    /// ```
    /// use rsdd::repr::WmcParams;
    /// use rsdd::util::semirings::{RealSemiring, FiniteField};
    ///
    /// let params = WmcParams::<RealSemiring>::default();
    ///
    /// let params = WmcParams::<FiniteField<2>>::default();
    /// ```
    fn default() -> Self {
        WmcParams {
            zero: T::zero(),
            one: T::one(),
            var_to_val: Vec::new(),
        }
    }
}
