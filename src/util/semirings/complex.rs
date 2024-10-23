use super::semiring_traits::*;
use std::{
    fmt::{self, Display, Formatter},
    ops::{Add, Mul, Sub},
};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
#[repr(C)]
pub struct Complex {
    pub re: f64,
    pub im: f64,
}

impl Display for Complex {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.im >= 0. {
            write!(f, "{}+{}i", self.re, self.im)
        } else {
            write!(f, "{}{}i", self.re, self.im)
        }
    }
}

impl Add<Complex> for Complex {
    type Output = Self;

    fn add(self, rhs: Complex) -> Self::Output {
        Self {
            re: self.re + rhs.re,
            im: self.im + rhs.im,
        }
    }
}

impl Mul<Complex> for Complex {
    type Output = Self;

    fn mul(self, rhs: Complex) -> Self::Output {
        Self {
            re: self.re * rhs.re - self.im * rhs.im,
            im: self.re * rhs.im + self.im * rhs.re,
        }
    }
}

impl Sub<Complex> for Complex {
    type Output = Self;

    fn sub(self, rhs: Complex) -> Self::Output {
        Self {
            re: self.re - rhs.re,
            im: self.im - rhs.im,
        }
    }
}

impl Semiring for Complex {
    fn one() -> Self {
        Self { re: 1.0, im: 0.0 }
    }

    fn zero() -> Self {
        Self { re: 0.0, im: 0.0 }
    }
}
