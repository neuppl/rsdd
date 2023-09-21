// Expected Utility Semiring.
use super::semiring_traits::*;
use std::{cmp::Ordering, fmt::Display, ops};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ExpectedUtility(pub f64, pub f64);

impl ops::Add<ExpectedUtility> for ExpectedUtility {
    type Output = ExpectedUtility;

    fn add(self, rhs: ExpectedUtility) -> Self::Output {
        ExpectedUtility(self.0 + rhs.0, self.1 + rhs.1)
    }
}

impl ops::Sub<ExpectedUtility> for ExpectedUtility {
    type Output = ExpectedUtility;

    fn sub(self, rhs: ExpectedUtility) -> Self::Output {
        ExpectedUtility(self.0 - rhs.0, self.1 - rhs.1)
    }
}

impl ops::Mul<ExpectedUtility> for ExpectedUtility {
    type Output = ExpectedUtility;

    fn mul(self, rhs: ExpectedUtility) -> Self::Output {
        let eu: f64 = (self.0 * rhs.1) + (self.1 * rhs.0);
        ExpectedUtility(self.0 * rhs.0, eu)
    }
}

impl Display for ExpectedUtility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Prob: {}, EU: {}", self.0, self.1)
    }
}
impl Semiring for ExpectedUtility {
    fn one() -> Self {
        ExpectedUtility(1.0, 0.0)
    }

    fn zero() -> Self {
        ExpectedUtility(0.0, 0.0)
    }
}

impl Ring for ExpectedUtility {}

impl PartialOrd for ExpectedUtility {
    fn partial_cmp(&self, other: &ExpectedUtility) -> Option<Ordering> {
        if self.0 < other.0 && self.1 < other.1 {
            Some(Ordering::Less)
        } else if self.0 > other.0 && self.1 > other.1 {
            Some(Ordering::Greater)
        } else if self.0 == other.0 && self.1 == other.1 {
            Some(Ordering::Equal)
        } else {
            None
        }
    }
}

impl JoinSemilattice for ExpectedUtility {
    fn join(&self, arg: &Self) -> Self {
        ExpectedUtility(f64::max(self.0, arg.0), f64::max(self.1, arg.1))
    }
}

impl BBSemiring for ExpectedUtility {
    fn choose(&self, arg: &ExpectedUtility) -> ExpectedUtility {
        if self.1 > arg.1 {
            *self
        } else {
            *arg
        }
    }
}

impl BBRing for ExpectedUtility {
    fn choose(&self, arg: &ExpectedUtility) -> ExpectedUtility {
        if self.1 > arg.1 {
            *self
        } else {
            *arg
        }
    }
}

impl MeetSemilattice for ExpectedUtility {
    fn meet(&self, arg: &Self) -> Self {
        ExpectedUtility(f64::min(self.0, arg.0), f64::min(self.1, arg.1))
    }
}

impl Lattice for ExpectedUtility {}

impl LatticeWithChoose for ExpectedUtility {}

impl EdgeboundingRing for ExpectedUtility {}

impl ops::Div<ExpectedUtility> for ExpectedUtility {
    type Output = ExpectedUtility;

    fn div(self, rhs: ExpectedUtility) -> Self::Output {
        let y = rhs.0;
        if y != 0.0 {
            ExpectedUtility(self.0 / y, self.1 / y)
        } else {
            ExpectedUtility::zero()
        }
    }
}
