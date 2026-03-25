// --- Utility types required for WmcParams to compile ---

use core::fmt::Debug;
use std::{collections::HashMap, ops};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct VarLabel(u32);

impl VarLabel {
    pub fn new(v: u32) -> Self { Self(v) }
    pub fn value_usize(&self) -> usize { self.0 as usize }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Literal(VarLabel, bool);

impl Literal {
    pub fn new(label: VarLabel, polarity: bool) -> Self { Self(label, polarity) }
    pub fn polarity(&self) -> bool { self.1 }
    pub fn label(&self) -> VarLabel { self.0 }
}

// --- Semiring Trait Definitions (as provided by user) ---

/// A semiring is a set equipped with addition and multiplication.
pub trait Semiring:
    Debug + Clone + Copy + std::fmt::Display + ops::Add<Self, Output = Self> + ops::Mul<Self, Output = Self>
{
    fn one() -> Self;
    fn zero() -> Self;
}

// (Other traits like Ring, JoinSemilattice, etc., are omitted for brevity as they are not needed for this solution)

// --- WmcParams Struct (as provided by user) ---

/// Weighted model counting parameters for a BDD. It primarily is a storage for
/// the weight on each variable.
#[repr(C)]
#[derive(Clone)]
pub struct WmcParams<T: Semiring> {
    pub zero: T,
    pub one: T,
    /// a vector which maps variable labels to `(low, high)` valuations.
    pub var_to_val: Vec<Option<(T, T)>>,
}

impl<T: Semiring> WmcParams<T> {
    /// Parametrize a weighted model count (over a semiring) with default weights.
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
    pub fn assignment_weight(&self, assgn: &[Literal]) -> T {
        let mut prod = self.one;
        for lit in assgn.iter() {
            if lit.polarity() {
                // Requires T: Clone + Mul
                prod = prod * self.var_to_val[lit.label().value_usize()].as_ref().unwrap().1.clone();
            } else {
                // Requires T: Clone + Mul
                prod = prod * self.var_to_val[lit.label().value_usize()].as_ref().unwrap().0.clone();
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

    pub fn var_weight(&self, label: VarLabel) -> &(T, T) {
        return (self.var_to_val[label.value_usize()]).as_ref().unwrap();
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
    fn default() -> Self {
        WmcParams {
            zero: T::zero(),
            one: T::one(),
            var_to_val: Vec::new(),
        }
    }
}

// IMPORTANT: This fix is necessary because the original WmcParams::assignment_weight uses
// a non-Copy Semiring type (T) by value, which is not possible in this structure,
// so we must clone the weights from var_to_val.
// I have adjusted `assignment_weight` to use `clone()` on the weights.

// Also, the original WmcParams::new uses `*value` which is fine only if T: Copy,
// which is required by the Semiring trait. Since Polynomial cannot be Copy,
// this is a potential issue in the original library's design for non-Copy semirings.
// However, the trait *requires* Copy, so we proceed by satisfying all other traits
// and noting that `Polynomial` cannot satisfy `Copy`.

// --- Simple Inner Semiring for Demonstration ---
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct RealSemiring(pub f64);

impl ops::Add<RealSemiring> for RealSemiring {
    type Output = Self;
    fn add(self, rhs: RealSemiring) -> Self::Output {
        RealSemiring(self.0 + rhs.0)
    }
}

impl ops::Mul<RealSemiring> for RealSemiring {
    type Output = Self;
    fn mul(self, rhs: RealSemiring) -> Self::Output {
        RealSemiring(self.0 * rhs.0)
    }
}

impl std::fmt::Display for RealSemiring {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Semiring for RealSemiring {
    fn one() -> Self { RealSemiring(1.0) }
    fn zero() -> Self { RealSemiring(0.0) }
}
