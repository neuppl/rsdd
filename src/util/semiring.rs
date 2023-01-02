//! A generic semiring trait and some implementations
//! A semiring is a set R equipped with two binary operations (+) and (*) such that:
//! 1. (R, +) is a commutative monoid with identity 0
//! 2. (R, *) is a monoid with identity 1
//! 3. Multiplication distributes over addition
//! 4. Multiplication by 0 annihilates R
//! Compared with a ring, a semiring omits an inverse for addition 

use std::ops;
use std::fmt::Debug;

pub trait Semiring : Debug + Clone + Copy + ops::Add + ops::Mul {
    fn one() -> Self;
    fn zero() -> Self;
}

#[derive(Debug, Clone, Copy)]
struct RealSemiring(f64);

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

impl Semiring for RealSemiring {
    fn one() -> Self {
        RealSemiring(1.0)
    }

    fn zero() -> Self {
        RealSemiring(0.0)
    }
}

pub trait TropicalSemiring : Debug + Clone + Copy + ops::Add + ops::Mul {
    fn one() -> Self;
    fn zero() -> Self;
    fn max(&self, other: &Self) -> Self;
    fn min(&self, other: &Self) -> Self;
}