use crate::{
    // DDNNFPtr is required for the wmc method
    repr::{VarLabel, WmcParams, BddPtr, DDNNFPtr},
    util::semirings::{Complex, RealSemiring, Semiring},
    // We import Polynomial and MAX_COEFFS from your implementation file
    util::semirings::polynomial_semiring_implementation::{Polynomial, MAX_COEFFS},
};
use std::collections::HashMap;
use std::slice;

// =========================================================================
// 1. Existing f64 / Complex Functions (Preserved)
// =========================================================================

#[no_mangle]
unsafe extern "C" fn new_wmc_params_f64() -> *mut WmcParams<RealSemiring> {
    Box::into_raw(Box::new(WmcParams::new(HashMap::from([]))))
}

#[no_mangle]
unsafe extern "C" fn free_wmc_params_f64(weights: *mut WmcParams<RealSemiring>) {
    drop(Box::from_raw(weights))
}

#[no_mangle]
unsafe extern "C" fn new_wmc_params_complex() -> *mut WmcParams<Complex> {
    Box::into_raw(Box::new(WmcParams::new(HashMap::from([]))))
}

#[no_mangle]
unsafe extern "C" fn free_wmc_params_complex(weights: *mut WmcParams<Complex>) {
    drop(Box::from_raw(weights))
}

#[no_mangle]
unsafe extern "C" fn wmc_param_f64_set_weight(
    weights: *mut WmcParams<RealSemiring>,
    var: u64,
    low: f64,
    high: f64,
) {
    (*weights).set_weight(VarLabel::new(var), RealSemiring(low), RealSemiring(high))
}

#[no_mangle]
unsafe extern "C" fn wmc_param_complex_set_weight(
    weights: *mut WmcParams<Complex>,
    var: u64,
    low: Complex,
    high: Complex,
) {
    (*weights).set_weight(VarLabel::new(var), low, high)
}

#[derive(Clone, Copy)]
#[repr(C)]
struct WeightF64(pub f64, pub f64);

#[no_mangle]
unsafe extern "C" fn wmc_param_f64_var_weight(
    weights: *mut WmcParams<RealSemiring>,
    var: u64,
) -> WeightF64 {
    let (l, h) = (*weights).var_weight(VarLabel::new(var));
    WeightF64(l.0, h.0)
}

#[no_mangle]
unsafe extern "C" fn weight_f64_lo(w: WeightF64) -> f64 {
    w.0
}

#[no_mangle]
unsafe extern "C" fn weight_f64_hi(w: WeightF64) -> f64 {
    w.1
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct WeightComplex(pub Complex, pub Complex);

#[no_mangle]
unsafe extern "C" fn wmc_param_complex_var_weight(
    weights: *mut WmcParams<Complex>,
    var: u64,
) -> WeightComplex {
    let (l, h) = (*weights).var_weight(VarLabel::new(var));
    WeightComplex(*l, *h)
}

#[no_mangle]
unsafe extern "C" fn weight_complex_lo(w: WeightComplex) -> Complex {
    w.0
}

#[no_mangle]
unsafe extern "C" fn weight_complex_hi(w: WeightComplex) -> Complex {
    w.1
}

// =========================================================================
// 2. New Polynomial Extensions (Cleaned)
// =========================================================================

// Type Alias
type PolyWeight = Polynomial<RealSemiring>;
type PolyWmcParams = WmcParams<PolyWeight>;

#[repr(C)]
pub struct WeightPoly {
    low: *mut PolyWeight,
    high: *mut PolyWeight,
}

// --- Helper: Safely create a Polynomial from C pointers ---
// This replaces the .collect() logic that was causing syntax errors
unsafe fn from_c_parts(coeffs: *const f64, len: usize) -> PolyWeight {
    if coeffs.is_null() || len == 0 {
        return PolyWeight::zero();
    }

    // Copy at most MAX_COEFFS items to fit in our fixed-size array
    let actual_len = len.min(MAX_COEFFS);
    let slice = slice::from_raw_parts(coeffs, actual_len);

    // Start with zero polynomial
    let mut poly = PolyWeight::zero();

    // Manually fill the array fields
    for (i, &val) in slice.iter().enumerate() {
        poly.coefficients[i] = RealSemiring(val);
    }
    poly.len = actual_len;

    poly
}

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

    // Clone to heap and pass ownership to C/Racket
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

#[no_mangle]
unsafe extern "C" fn bdd_wmc_poly(
    bdd: *mut BddPtr,
    weights: *mut PolyWmcParams,
) -> *mut PolyWeight {
    if bdd.is_null() || weights.is_null() {
        return std::ptr::null_mut();
    }

    // Fix: Use DDNNFPtr::unsmoothed_wmc to perform the count
    let result = DDNNFPtr::unsmoothed_wmc(&(*bdd), &(*weights));

    Box::into_raw(Box::new(result))
}
