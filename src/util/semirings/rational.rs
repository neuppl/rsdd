use rational::Rational;
use std::{fmt::Display, ops};

use super::semiring_traits::Semiring;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct RationalSemiring(Rational);

impl Semiring for RationalSemiring {
    fn one() -> Self {
        RationalSemiring(Rational::new(1, 1))
    }

    fn zero() -> Self {
        RationalSemiring(Rational::new(0, 1))
    }
}

impl Display for RationalSemiring {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.0.numerator(), self.0.denominator())
    }
}

impl ops::Add<RationalSemiring> for RationalSemiring {
    type Output = RationalSemiring;

    fn add(self, rhs: RationalSemiring) -> Self::Output {
        RationalSemiring(self.0 + rhs.0)
    }
}

impl ops::Mul<RationalSemiring> for RationalSemiring {
    type Output = RationalSemiring;

    fn mul(self, rhs: RationalSemiring) -> Self::Output {
        RationalSemiring(self.0 * rhs.0)
    }
}
