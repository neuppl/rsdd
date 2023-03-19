//! A generic semiring trait and some implementations
//! A semiring is a set R equipped with two binary operations (+) and (*) such that:
//! 1. (R, +) is a commutative monoid with identity 0
//! 2. (R, *) is a monoid with identity 1
//! 3. Multiplication distributes over addition
//! 4. Multiplication by 0 annihilates R
//! Compared with a ring, a semiring omits an inverse for addition

use std::fmt::{Debug, Display};
use std::ops;

pub trait Semiring: Debug + Clone + Copy + ops::Add + ops::Mul {
    fn one() -> Self;
    fn zero() -> Self;
}

/// Simple real-number semiring abstraction (all operations standard for reals, abstracted as f64)
#[derive(Debug, Clone, Copy)]
pub struct RealSemiring(pub f64);

impl Display for RealSemiring {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl ops::Add<RealSemiring> for RealSemiring {
    type Output = RealSemiring;

    fn add(self, rhs: RealSemiring) -> Self::Output {
        RealSemiring(self.0 + rhs.0)
    }
}

impl ops::Mul<RealSemiring> for RealSemiring {
    type Output = RealSemiring;

    fn mul(self, rhs: RealSemiring) -> Self::Output {
        RealSemiring(self.0 * rhs.0)
    }
}

impl ops::Sub<RealSemiring> for RealSemiring {
    type Output = RealSemiring;

    fn sub(self, rhs: RealSemiring) -> Self::Output {
        RealSemiring(self.0 - rhs.0)
    }
}

impl Semiring for RealSemiring {
    fn one() -> Self {
        RealSemiring(1.0)
    }

    fn zero() -> Self {
        RealSemiring(0.0)
    }
}

/// a finite-field abstraction. The parameter `p` is the size of the field.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
}

impl<const P: u128> Semiring for FiniteField<P> {
    fn one() -> Self {
        FiniteField::new(1)
    }

    fn zero() -> Self {
        FiniteField::new(0)
    }
}

impl<const P: u128> ops::Add<FiniteField<P>> for FiniteField<P> {
    type Output = FiniteField<P>;

    fn add(self, rhs: FiniteField<P>) -> Self::Output {
        FiniteField::new((self.v + rhs.v) % P)
    }
}

impl<const P: u128> ops::Sub<FiniteField<P>> for FiniteField<P> {
    type Output = FiniteField<P>;

    fn sub(self, rhs: FiniteField<P>) -> Self::Output {
        FiniteField::new((P + self.v - rhs.v) % P)
    }
}

impl<const P: u128> ops::Mul<FiniteField<P>> for FiniteField<P> {
    type Output = FiniteField<P>;

    fn mul(self, rhs: FiniteField<P>) -> Self::Output {
        FiniteField::new((self.v * rhs.v) % P)
    }
}

impl<const P: u128> Display for FiniteField<P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.v)
    }
}

pub trait TropicalSemiring: Debug + Clone + Copy + ops::Add + ops::Mul {
    fn one() -> Self;
    fn zero() -> Self;
    fn max(&self, other: &Self) -> Self;
    fn min(&self, other: &Self) -> Self;
}
