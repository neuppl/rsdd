use std::{fmt::Display, ops};

use super::semiring_traits::Semiring;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct BooleanSemiring(pub bool);

impl Semiring for BooleanSemiring {
    fn one() -> Self {
        BooleanSemiring(true)
    }

    fn zero() -> Self {
        BooleanSemiring(false)
    }
}

impl Display for BooleanSemiring {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl ops::Add<BooleanSemiring> for BooleanSemiring {
    type Output = BooleanSemiring;

    fn add(self, rhs: BooleanSemiring) -> Self::Output {
        BooleanSemiring(self.0 || rhs.0)
    }
}

impl ops::Mul<BooleanSemiring> for BooleanSemiring {
    type Output = BooleanSemiring;

    fn mul(self, rhs: BooleanSemiring) -> Self::Output {
        BooleanSemiring(self.0 && rhs.0)
    }
}
