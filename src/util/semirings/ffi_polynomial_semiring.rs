use super::polynomial_semiring_implementation::{Polynomial, MAX_COEFFS};
use crate::util::semirings::{RealSemiring, Semiring};
// CHANGE: Added DDNNFPtr to imports so we can call unsmoothed_wmc
use crate::repr::{WmcParams, VarLabel, BddPtr, DDNNFPtr};
use std::{collections::HashMap, slice};

type PolyWeight = Polynomial<RealSemiring>;
type PolyWmcParams = WmcParams<PolyWeight>;

#[repr(C)]
pub struct WeightPoly {
    low: *mut PolyWeight,
    high: *mut PolyWeight,
}

// --- Helper: safely create a Polynomial from C pointers ---
unsafe fn from_c_parts(coeffs: *const f64, len: usize) -> PolyWeight {
    if coeffs.is_null() || len == 0 {
        return PolyWeight::zero();
    }

    // Copy at most MAX_COEFFS items
    let actual_len = len.min(MAX_COEFFS);
    let slice = slice::from_raw_parts(coeffs, actual_len);

    // Initialize with zeros
    let mut poly = PolyWeight::zero();

    // Fill the buffer
    for (i, &val) in slice.iter().enumerate() {
        poly.coefficients[i] = RealSemiring(val);
    }
    poly.len = actual_len;

    poly
}

// --- FFI Functions ---

#[no_mangle]
unsafe extern "C" fn new_polynomial(coeffs: *const f64, len: usize) -> *mut PolyWeight {
    let poly = from_c_parts(coeffs, len);
    Box::into_raw(Box::new(poly))
}

#[no_mangle]
unsafe extern "C" fn destroy_polynomial(p: *mut PolyWeight) {
    if !p.is_null() {
        let _ = Box::from_raw(p);
    }
}

#[no_mangle]
unsafe extern "C" fn new_wmc_params_poly() -> *mut PolyWmcParams {
    Box::into_raw(Box::new(PolyWmcParams::new(HashMap::new())))
}

#[no_mangle]
unsafe extern "C" fn destroy_wmc_params_poly(weights: *mut PolyWmcParams) {
    if !weights.is_null() {
        let _ = Box::from_raw(weights);
    }
}

#[no_mangle]
unsafe extern "C" fn wmc_param_poly_set_weight(
    weights: *mut PolyWmcParams,
    var: u64,
    low_coeffs: *const f64,
    low_len: usize,
    high_coeffs: *const f64,
    high_len: usize,
) {
    if weights.is_null() { return; }

    let low_poly = from_c_parts(low_coeffs, low_len);
    let high_poly = from_c_parts(high_coeffs, high_len);

    (*weights).set_weight(VarLabel::new(var), low_poly, high_poly);
}

#[no_mangle]
unsafe extern "C" fn wmc_param_poly_var_weight(
    weights: *mut PolyWmcParams,
    var: u64,
) -> WeightPoly {
    if weights.is_null() {
        return WeightPoly { low: std::ptr::null_mut(), high: std::ptr::null_mut() };
    }

    let (l, h) = (*weights).var_weight(VarLabel::new(var));

    let low_ptr = Box::into_raw(Box::new(*l));
    let high_ptr = Box::into_raw(Box::new(*h));

    WeightPoly { low: low_ptr, high: high_ptr }
}

#[no_mangle]
unsafe extern "C" fn polynomial_len(p: *mut PolyWeight) -> usize {
    if p.is_null() { 0 } else { (*p).len }
}

#[no_mangle]
unsafe extern "C" fn polynomial_get_coeffs(
    p: *mut PolyWeight,
    buffer: *mut f64,
    max_len: usize,
) -> usize {
    if p.is_null() || buffer.is_null() { return 0; }

    let poly = &(*p);
    let count = poly.len.min(max_len);

    let dest = slice::from_raw_parts_mut(buffer, count);
    for i in 0..count {
        dest[i] = poly.coefficients[i].0;
    }

    count
}

// CHANGE: Updated to use DDNNFPtr::unsmoothed_wmc
#[no_mangle]
unsafe extern "C" fn bdd_wmc_poly(
    bdd: *mut BddPtr,
    weights: *mut PolyWmcParams,
) -> *mut PolyWeight {
    if bdd.is_null() || weights.is_null() {
        return std::ptr::null_mut();
    }

    // Correct way to invoke WMC in this version of rsdd
    let result = DDNNFPtr::unsmoothed_wmc(&(*bdd), &(*weights));

    Box::into_raw(Box::new(result))
}
