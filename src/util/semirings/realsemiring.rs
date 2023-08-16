use super::semiring_traits::*;
use std::{fmt::Display, ops};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
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

impl Ring for RealSemiring {}

impl JoinSemilattice for RealSemiring {
    fn join(&self, arg: &Self) -> Self {
        RealSemiring(f64::max(self.0, arg.0))
    }
}

impl BBSemiring for RealSemiring {
    fn choose(&self, arg: &RealSemiring) -> RealSemiring {
        JoinSemilattice::join(self, arg)
    }
}

impl BBRing for RealSemiring {
    fn choose(&self, arg: &RealSemiring) -> RealSemiring {
        JoinSemilattice::join(self, arg)
    }
}

impl MeetSemilattice for RealSemiring {
    fn meet(&self, arg: &Self) -> Self {
        RealSemiring(f64::min(self.0, arg.0))
    }
}

impl Lattice for RealSemiring {}

impl EdgeboundingRing for RealSemiring {}
