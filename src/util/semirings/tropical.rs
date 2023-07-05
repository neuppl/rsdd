use std::{fmt::Debug, ops};

pub trait TropicalSemiring: Debug + Clone + Copy + ops::Add + ops::Mul {
    fn one() -> Self;
    fn zero() -> Self;
    fn max(&self, other: &Self) -> Self;
    fn min(&self, other: &Self) -> Self;
}
