//! A semiring is a set R equipped with two binary operations (+) and (*) such that:
//! 1. (R, +) is a commutative monoid with identity 0
//! 2. (R, *) is a monoid with identity 1
//! 3. Multiplication distributes over addition
//! 4. Multiplication by 0 annihilates R
//! Compared with a ring, a semiring omits an inverse for addition
//!
use std::fmt::{Debug, Display};
use std::ops;

pub trait Semiring:
    Debug + Clone + Copy + Display + ops::Add<Self, Output = Self> + ops::Mul<Self, Output = Self>
{
    fn one() -> Self;
    fn zero() -> Self;
}

// A ring is a semiring with additive inverses, which is equivalent to a notion of subtraction.
pub trait Ring: Semiring + ops::Sub<Self, Output = Self> {}

// A join-semilattice is a set equipped with a partial order
// that also admits a least upper bound (called join) for any two elements.
pub trait JoinSemilattice: PartialOrd {
    fn join(&self, arg: &Self) -> Self;
}

// A branch-and-bound semiring is a semiring join-semilattice with a
// compatible total order `choose`.
pub trait BBSemiring: Semiring + JoinSemilattice {
    fn choose(&self, arg: &Self) -> Self;
}

pub trait BBRing: Ring + JoinSemilattice {
    fn choose(&self, arg: &Self) -> Self;
}

// A meet-semilattice is a set equipped with a partial order
// that also admits a least upper bound (called join) for any two elements.
pub trait MeetSemilattice: PartialOrd {
    fn meet(&self, arg: &Self) -> Self;
}

pub trait Lattice: JoinSemilattice + MeetSemilattice {}

pub trait LatticeWithChoose: BBSemiring + MeetSemilattice {}

pub trait EdgeboundingRing: Lattice + BBRing {}
