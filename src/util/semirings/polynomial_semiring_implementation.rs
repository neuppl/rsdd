use crate::util::semirings::Semiring;
use std::ops;
use core::fmt::{self, Debug, Display};

// Define a maximum degree (e.g., 32 coefficients).
// This makes the struct size predictable and allows 'Copy'.
pub const MAX_COEFFS: usize = 32;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Polynomial<C> {
    pub coefficients: [C; MAX_COEFFS], // Public so FFI can access it
    pub len: usize,                    // The actual number of used coefficients
}

// --- Debug & Display ---

impl<C: Semiring + Copy + Debug> Debug for Polynomial<C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Polynomial(len={})", self.len)
    }
}

impl<C: Semiring + Copy + Display> Display for Polynomial<C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.len == 0 {
            return write!(f, "0");
        }
        // Simple display logic
        write!(f, "Poly(degree {})", self.len - 1)
    }
}

// --- Semiring Implementation ---

impl<C: Semiring + Copy> Semiring for Polynomial<C> {
    fn zero() -> Self {
        // [C::zero(); 32] works because RealSemiring is Copy
        Polynomial {
            coefficients: [C::zero(); MAX_COEFFS],
            len: 0,
        }
    }

    fn one() -> Self {
        let mut coeffs = [C::zero(); MAX_COEFFS];
        coeffs[0] = C::one();
        Polynomial {
            coefficients: coeffs,
            len: 1,
        }
    }
}

// --- Addition ---

impl<C: Semiring + Copy> ops::Add for Polynomial<C> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let max_len = self.len.max(rhs.len).min(MAX_COEFFS);
        let mut new_coeffs = [C::zero(); MAX_COEFFS];

        for i in 0..max_len {
            new_coeffs[i] = self.coefficients[i] + rhs.coefficients[i];
        }

        Polynomial {
            coefficients: new_coeffs,
            len: max_len,
        }
    }
}

// --- Multiplication ---

impl<C: Semiring + Copy> ops::Mul for Polynomial<C> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        if self.len == 0 || rhs.len == 0 {
            return Self::zero();
        }

        // Result length is len1 + len2 - 1
        let new_len = (self.len + rhs.len).saturating_sub(1).min(MAX_COEFFS);
        let mut new_coeffs = [C::zero(); MAX_COEFFS];

        for i in 0..self.len {
            for j in 0..rhs.len {
                if i + j < MAX_COEFFS {
                    new_coeffs[i + j] = new_coeffs[i + j] + (self.coefficients[i] * rhs.coefficients[j]);
                }
            }
        }

        Polynomial {
            coefficients: new_coeffs,
            len: new_len,
        }
    }
}