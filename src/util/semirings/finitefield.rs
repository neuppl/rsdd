use super::semiring_traits::*;
use core::fmt::Debug;
/// Simple real-number semiring abstraction (all operations standard for reals, abstracted as f64)
/// a finite-field abstraction. The parameter `p` is the size of the field.
use std::{fmt::Display, ops};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FiniteField<const P: u128> {
    v: u128,
}

impl<const P: u128> FiniteField<P> {
    pub fn new(v: u128) -> FiniteField<P> {
        FiniteField { v: v % P }
    }
    pub fn value(&self) -> u128 {
        self.v
    }

    /// computes the additive inverse of self
    pub fn negate(&self) -> FiniteField<P> {
        FiniteField::new(P - self.v + 1)
    }
}

impl<const P: u128> Semiring for FiniteField<P> {
    fn one() -> Self {
        FiniteField::new(1)
    }

    fn zero() -> Self {
        FiniteField::new(0)
    }
}

impl<const P: u128> Ring for FiniteField<P> {}

impl<const P: u128> ops::Add<FiniteField<P>> for FiniteField<P> {
    type Output = FiniteField<P>;

    fn add(self, rhs: FiniteField<P>) -> Self::Output {
        FiniteField::new((self.v + rhs.v) % P)
    }
}

impl<const P: u128> ops::Mul<FiniteField<P>> for FiniteField<P> {
    type Output = FiniteField<P>;

    fn mul(self, rhs: FiniteField<P>) -> Self::Output {
        FiniteField::new((self.v * rhs.v) % P)
    }
}

impl<const P: u128> ops::Sub<FiniteField<P>> for FiniteField<P> {
    type Output = FiniteField<P>;

    fn sub(self, rhs: FiniteField<P>) -> Self::Output {
        FiniteField::new(if self.v > rhs.v {
            self.v - rhs.v
        } else {
            rhs.v - self.v
        })
    }
}

impl<const P: u128> Display for FiniteField<P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.v)
    }
}

impl<const P: u128> Debug for FiniteField<P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FiniteField({})", self.v)
    }
}
