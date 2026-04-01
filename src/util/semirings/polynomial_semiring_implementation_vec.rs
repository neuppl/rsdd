use super::semiring_traits::*;
use crate::repr::VarLabel;
use crate::repr::WmcParams;
use crate::repr::Literal;
use std::{collections::HashMap, ops};
use core::fmt::Debug;


use crate::util::semirings::{RealSemiring, Semiring};
use core::fmt::{self, Display};

// 1. Define a constant for max size
const MAX_DEGREE: usize = 32;

// 2. Use 'Copy' on the struct.
// We use an array [C; MAX_DEGREE] and a length field.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Polynomial<C> {
    coefficients: [C; MAX_DEGREE], // Fixed size array is Copy if C is Copy
    len: usize,                    // Track actual degree
}

impl<C: Semiring + Copy> Debug for Polynomial<C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Polynomial(deg {})", self.len)
    }
}

// Minimal Display impl to satisfy trait
impl<C: Semiring + Copy + Display> Display for Polynomial<C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Poly")
    }
}

impl<C: Semiring + Copy> Semiring for Polynomial<C> {
    fn zero() -> Self {
        // [C::zero(); N] requires C: Copy, which RealSemiring is.
        Polynomial {
            coefficients: [C::zero(); MAX_DEGREE],
            len: 0,
        }
    }

    fn one() -> Self {
        let mut coeffs = [C::zero(); MAX_DEGREE];
        coeffs[0] = C::one();
        Polynomial {
            coefficients: coeffs,
            len: 1,
        }
    }
}

impl<C: Semiring + Copy> ops::Add for Polynomial<C> {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        let max_len = self.len.max(rhs.len);
        let mut new_coeffs = [C::zero(); MAX_DEGREE];

        for i in 0..max_len {
            new_coeffs[i] = self.coefficients[i] + rhs.coefficients[i];
        }

        Polynomial {
            coefficients: new_coeffs,
            len: max_len, // (Should technically strip trailing zeros here)
        }
    }
}

impl<C: Semiring + Copy> ops::Mul for Polynomial<C> {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        if self.len == 0 || rhs.len == 0 { return Self::zero(); }

        let new_len = (self.len + rhs.len).saturating_sub(1);
        if new_len > MAX_DEGREE {
            panic!("Polynomial degree overflow!");
        }

        let mut new_coeffs = [C::zero(); MAX_DEGREE];

        for i in 0..self.len {
            for j in 0..rhs.len {
                if i + j < MAX_DEGREE {
                    new_coeffs[i+j] = new_coeffs[i+j] + (self.coefficients[i] * rhs.coefficients[j]);
                }
            }
        }

        Polynomial {
            coefficients: new_coeffs,
            len: new_len,
        }
    }
}

/*
/// Represents a polynomial P(x) = c_0 + c_1*x + c_2*x^2 + ...
/// where coefficients `C` are from a base Semiring.
#[derive(Clone, PartialEq, Eq)]
pub struct Polynomial<C> {
    /// coefficients[i] is the coefficient for x^i
    coefficients: Vec<C>,
}

// --- Required Traits for Polynomial ---

// Because Polynomial contains Vec<C>, it cannot implement `Copy`.
// This is the only trait in `Semiring` it cannot satisfy.
// We implement the others, noting this limitation.

impl<C: Debug> Debug for Polynomial<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let poly_str = self.coefficients.iter().enumerate()
            .filter_map(|(i, c)| {
                // Filter out zero coefficients for cleaner debug output (assuming C::zero() is implied)
                if i == 0 {
                    Some(format!("{:?}", c))
                } else {
                    Some(format!("{:?}x^{}", c, i))
                }
            })
            .collect::<Vec<String>>()
            .join(" + ");

        write!(f, "Polynomial({})", poly_str)
    }
}

impl<C: std::fmt::Display> std::fmt::Display for Polynomial<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let poly_str = self.coefficients.iter().enumerate()
            .rev() // Start from the highest power
            .filter(|(_i, c)| {
                // A quick check to skip coefficients that evaluate to "0"
                format!("{}", c) != "0" && format!("{}", c) != "0.0"
            })
            .map(|(i, c)| {
                match i {
                    0 => format!("{}", c),
                    1 => format!("{}x", c),
                    _ => format!("{}x^{}", c, i),
                }
            })
            .collect::<Vec<String>>()
            .join(" + ");

        if poly_str.is_empty() {
            write!(f, "0")
        } else {
            write!(f, "{}", poly_str)
        }
    }
}


// --- Semiring Implementation for Polynomial ---

impl<C: Semiring + Clone> Semiring for Polynomial<C> {
    // The zero polynomial is the empty vector of coefficients
    fn zero() -> Self {
        Polynomial {
            coefficients: Vec::new(),
        }
    }

    // The one polynomial is 1*x^0, i.e., [C::one()]
    fn one() -> Self {
        Polynomial {
            coefficients: vec![C::one()],
        }
    }
}

// --- Add Implementation for Polynomial (Coefficient-wise addition) ---

impl<C: Semiring + Clone> ops::Add for Polynomial<C> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let n = self.coefficients.len();
        let m = rhs.coefficients.len();
        let max_len = n.max(m);
        let mut new_coeffs = Vec::with_capacity(max_len);

        for i in 0..max_len {
            // Get coefficient or default to C::zero()
            let left_coeff = self.coefficients.get(i).cloned().unwrap_or(C::zero());
            let right_coeff = rhs.coefficients.get(i).cloned().unwrap_or(C::zero());

            // Add the coefficients
            new_coeffs.push(left_coeff + right_coeff);
        }

        // Clean up trailing zeros (optional but good practice)
        while new_coeffs.last().map_or(false, |c| format!("{}", c) == "0" || format!("{}", c) == "0.0") && new_coeffs.len() > 1 {
            new_coeffs.pop();
        }

        Polynomial { coefficients: new_coeffs }
    }
}

// --- Mul Implementation for Polynomial (Convolution) ---

impl<C: Semiring + Clone> ops::Mul for Polynomial<C> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let n = self.coefficients.len();
        let m = rhs.coefficients.len();

        // If either polynomial is zero, the result is zero.
        if n == 0 || m == 0 {
            return Polynomial::zero();
        }

        // The degree of the result is deg(P) + deg(Q), so the length is (n-1) + (m-1) + 1 = n + m - 1
        let mut new_coeffs = vec![C::zero(); n + m - 1];

        for i in 0..n {
            for j in 0..m {
                // Calculate the term: self.coeffs[i] * rhs.coeffs[j]
                let term = self.coefficients[i].clone() * rhs.coefficients[j].clone();
                let result_index = i + j;

                // Add to the resulting coefficient: new_coeffs[i+j] += term
                let current = new_coeffs[result_index].clone();
                new_coeffs[result_index] = current + term;
            }
        }

        // Clean up trailing zeros (optional)
        while new_coeffs.last().map_or(false, |c| format!("{}", c) == "0" || format!("{}", c) == "0.0") && new_coeffs.len() > 1 {
            new_coeffs.pop();
        }

        Polynomial { coefficients: new_coeffs }
    }
}

// --- Example Usage in main ---

fn main() {
    println!("--- Polynomial Semiring for Weighted Model Counting ---");

    // Define the type for our weights: Polynomials over RealSemiring (f64)
    type PolyWeight = Polynomial<RealSemiring>;

    // P(x) = 0.5 + 0.5x
    let p_v0 = PolyWeight {
        coefficients: vec![RealSemiring(0.5), RealSemiring(0.5)],
    };
    // Q(x) = 0.3 + 0.7x
    let p_v1 = PolyWeight {
        coefficients: vec![RealSemiring(0.3), RealSemiring(0.7)],
    };
    // R(x) = 0.9x^2 (for a more interesting example)
    let p_v2 = PolyWeight {
        coefficients: vec![RealSemiring(0.0), RealSemiring(0.0), RealSemiring(0.9)],
    };

    // Construct the WmcParams map:
    // Var 0: low weight = P(x), high weight = P(x)
    // Var 1: low weight = Q(x), high weight = Q(x)
    // Var 2: low weight = 1.0, high weight = R(x)
    let mut weights = HashMap::new();
    weights.insert(VarLabel::new(0), (p_v0.clone(), p_v0.clone()));
    weights.insert(VarLabel::new(1), (p_v1.clone(), p_v1.clone()));
    // For Var 2, we use Polynomial::one() which is 1
    weights.insert(VarLabel::new(2), (PolyWeight::one(), p_v2.clone()));

    // Instantiate WmcParams with the Polynomial Semiring
    let params: WmcParams<PolyWeight> = WmcParams::new(weights);

    println!("\nInstantiated WMC Parameters (PolySemiring):");
    println!("{:?}", params);

    // --- Calculate WMC for a specific assignment ---
    // Assignment: Var 0 = True, Var 1 = False, Var 2 = True
    let assignment = [
        Literal::new(VarLabel::new(0), true),
        Literal::new(VarLabel::new(1), false),
        Literal::new(VarLabel::new(2), true),
    ];

    // Expected result:
    // Weight(0, True) * Weight(1, False) * Weight(2, True)
    // p_v0 (0.5 + 0.5x) * p_v1 (0.3 + 0.7x) * p_v2 (0.9x^2)

    let wmc_result = params.assignment_weight(&assignment);

    // (0.5 + 0.5x) * (0.3 + 0.7x)
    // = 0.5*0.3 + (0.5*0.7 + 0.5*0.3)x + 0.5*0.7x^2
    // = 0.15 + (0.35 + 0.15)x + 0.35x^2
    // = 0.15 + 0.5x + 0.35x^2

    // Result * 0.9x^2
    // = 0.15*0.9x^2 + 0.5*0.9x^3 + 0.35*0.9x^4
    // = 0.135x^2 + 0.45x^3 + 0.315x^4

    println!("\nAssignment: Var0=T, Var1=F, Var2=T");
    println!("Calculated WMC Weight (P(x)): {}", wmc_result);
    println!("Coefficients (Debug format): {:?}", wmc_result);

    // The coefficients should be approximately: [0.0, 0.0, 0.135, 0.45, 0.315]
}

// Running this will output:
// Calculated WMC Weight (P(x)): 0.315x^4 + 0.45x^3 + 0.135x^2
*/
