use core::fmt::Debug;
use std::collections::HashMap;

use crate::util::semiring::Semiring;

use super::var_label::{Literal, VarLabel};

/// Weighted model counting parameters for a BDD. It primarily is a storage for
/// the weight on each variable.
#[derive(Debug, Clone)]
pub struct WmcParams<T: Semiring> {
    pub zero: T,
    pub one: T,
    /// a vector which maps variable labels to `(low, high)`
    /// valuations.
    var_to_val: Vec<Option<(T, T)>>,
}

impl<T: Semiring + std::ops::Mul<Output = T> + std::ops::Add<Output = T>> WmcParams<T> {
    /// Generates a new `BddWmc` with a default `var_to_val`; it is private because we
    /// do not want to expose the structure of the associative array
    pub fn new_with_default(
        zero: T,
        one: T,
        var_to_val: HashMap<VarLabel, (T, T)>,
    ) -> WmcParams<T> {
        let mut var_to_val_vec: Vec<Option<(T, T)>> = vec![None; var_to_val.len()];
        for (key, value) in var_to_val.iter() {
            var_to_val_vec[key.value_usize()] = Some(*value);
        }
        WmcParams {
            zero,
            one,
            var_to_val: var_to_val_vec,
        }
    }

    /// Generate a new `BddWmc` with no associations
    pub fn new(zero: T, one: T) -> WmcParams<T> {
        WmcParams {
            zero,
            one,
            var_to_val: Vec::new(),
        }
    }

    /// get the weight of an asignment
    pub fn get_weight(&self, assgn: &[Literal]) -> T {
        let mut prod = self.one;
        for lit in assgn.iter() {
            if lit.get_polarity() {
                prod = prod * self.var_to_val[lit.get_label().value_usize()].unwrap().1
            } else {
                prod = prod * self.var_to_val[lit.get_label().value_usize()].unwrap().0
            }
        }
        prod
    }

    pub fn set_weight(&mut self, lbl: VarLabel, low: T, high: T) {
        let n = lbl.value_usize();
        while n >= self.var_to_val.len() {
            self.var_to_val.push(None);
        }
        self.var_to_val[n] = Some((low, high));
    }

    pub fn get_var_weight(&self, label: VarLabel) -> &(T, T) {
        return (self.var_to_val[label.value_usize()]).as_ref().unwrap();
    }
}
