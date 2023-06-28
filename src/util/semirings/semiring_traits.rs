//! A generic semiring trait and some implementations
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

pub trait JoinSemilattice: PartialOrd {
    fn join(&self, arg: &Self) -> Self;
}

pub trait BBAlgebra: Semiring + JoinSemilattice {
    fn choose(&self, arg: &Self) -> Self;
}

pub trait MeetSemilattice: PartialOrd {
    fn meet(&self, arg: &Self) -> Self;
}


pub trait Lattice: JoinSemilattice + MeetSemilattice {}

pub trait EdgeboundingSemiring : Lattice + BBAlgebra + ops::Sub<Self, Output = Self> {}
