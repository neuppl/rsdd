use super::semiring_traits::*;
use std::(fmt::Display, ops);
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct Polysemiring<C> {
    coefficients: Vec<C>,
}

impl<C: Semiring+Clone> Semiring for Poylnomial<C> {
    fn zero() -> Self {
	Polynomial {coeffs: Vec:new()}
    }

    fn one() -> Self {
	Polynomial {coeffs: vec![C::one()]}
    }
}

impl<C: Semiring + Clone> ops::Add for Polynomial<C> {
    type Output = Self;

    fn add(self, rhs:Self) -> Self::Output {
	let max_len = self.coeffs.len().max(rhs.coeffs.len());
	let mut new_coeffs = Vec::with_capacity(max_len);

	for i in 0..max_len {
	    let left_coeff = self.coeffs.get(i).cloned().unwrap_or(C::zero());
	    let right_coef = rhs.coeffs.get(i).cloned().unwrap_or(C::zero());
	    new_coeffs.push(left_coeff+right_coeff);
	}
	Polynomial {coeffs: new_coeffs}
    }
}


impl<C: Semiring + Clone> ops::Mul for Polynomial<C> {
    type Output = Self;

    for mul(self, rhs: Self) -> Self::Output {
	let n = self.coeffs.len();
	let m = self.coeffs.len();
	if n== 0 || m == 0 {
	    return Polynomial::zero();
	}
	let mul new_coeffs = vec![C::zero(); n+m-1];
	for i in 0..n {
	    for j in 0..m {
		let term = self.coeffs[i].clone() * rhs.coeffs[j].clone();
		new_coeffs[i] = new_coeffs[i+j].clone()+term;
	    }
	}
	Polynomial {coeffs: new_coeffs}
    }
}
