use super::semiring_traits::*;
use crate::util::semirings::RealSemiring;
use crate::repr::WmcParams;
use crate::repr::VarLabel;
use super::polynomial_semiring_implementation::Polynomial;
use std::{collections::HashMap, slice};

// --- Type Alias for Clarity ---
/// The concrete polynomial type used in the FFI.
type PolyWeight = Polynomial<RealSemiring>;
type PolyWmcParams = WmcParams<PolyWeight>;

// --- Helper Functions for Polynomial Management ---

/// Creates a new Polynomial<RealSemiring> from a C array of f64 coefficients.
/// The caller (C) is responsible for destroying the returned pointer using `destroy_polynomial`.
#[no_mangle]
unsafe extern "C" fn new_polynomial(
    coeffs: *const f64,
    len: usize,
) -> *mut PolyWeight {
    if coeffs.is_null() || len == 0 {
        return Box::into_raw(Box::new(PolyWeight::zero()));
    }

    // Create a slice from the raw C pointer
    let coeff_slice = slice::from_raw_parts(coeffs, len);

    // Map f64 values to RealSemiring wrapper structs
    let coefficients = coeff_slice.iter().map(|&c| RealSemiring(c)).collect();

    Box::into_raw(Box::new(Polynomial { coefficients }))
}

/// Destroys a Polynomial pointer allocated by new_polynomial.
/// This prevents memory leaks on the Rust side.
#[no_mangle]
unsafe extern "C" fn destroy_polynomial(p: *mut PolyWeight) {
    if !p.is_null() {
        // Retake ownership and drop the Box
        let _ = Box::from_raw(p);
    }
}


// --- FFI for WmcParams<PolyWeight> ---

/// Initializes new WmcParams for the Polynomial Semiring with an empty set of weights.
/// Returns an opaque pointer to the WmcParams object.
#[no_mangle]
unsafe extern "C" fn new_wmc_params_poly() -> *mut PolyWmcParams {
    // We use a small HashMap to satisfy the new() signature, effectively creating a default instance.
    Box::into_raw(Box::new(PolyWmcParams::new(HashMap::new())))
}

/// Destroys the WmcParams object and frees its associated memory.
#[no_mangle]
unsafe extern "C" fn destroy_wmc_params_poly(weights: *mut PolyWmcParams) {
    if !weights.is_null() {
        // Retake ownership and drop the Box
        let _ = Box::from_raw(weights);
    }
}

/// Sets the weight for a given variable using two arrays of f64 coefficients.
/// The coefficients are passed as C arrays (pointers and lengths).
///
/// NOTE: This function creates new Polynomial objects from the C data, which are
/// then cloned into the WmcParams structure.
#[no_mangle]
unsafe extern "C" fn wmc_param_poly_set_weight(
    weights: *mut PolyWmcParams,
    var: u64,
    low_coeffs: *const f64,
    low_len: usize,
    high_coeffs: *const f64,
    high_len: usize,
) {
    if weights.is_null() {
        // Handle null pointer case (or log an error)
        return;
    }

    // 1. Create the Low Polynomial
    let low_slice = slice::from_raw_parts(low_coeffs, low_len);
    let low_poly = Polynomial {
        coefficients: low_slice.iter().map(|&c| RealSemiring(c)).collect(),
    };

    // 2. Create the High Polynomial
    let high_slice = slice::from_raw_parts(high_coeffs, high_len);
    let high_poly = Polynomial {
        coefficients: high_slice.iter().map(|&c| RealSemiring(c)).collect(),
    };

    // 3. Set the weight in WmcParams
    (*weights).set_weight(VarLabel::new(var), low_poly, high_poly);
}

/// A structure designed to return the two Polynomial pointers across the FFI boundary.
/// The caller MUST destroy the returned polynomials using `destroy_polynomial`
/// after they are no longer needed.
#[repr(C)]
pub struct WeightPoly {
    // Opaque pointer to the low polynomial
    low: *mut PolyWeight,
    // Opaque pointer to the high polynomial
    high: *mut PolyWeight,
}

/// Retrieves the (low, high) weight polynomials for a given variable.
///
/// Returns a WeightPoly struct containing two pointers to heap-allocated
/// Polynomial clones. The caller (C) is responsible for destroying these
/// two returned polynomial pointers.
#[no_mangle]
unsafe extern "C" fn wmc_param_poly_var_weight(
    weights: *mut PolyWmcParams,
    var: u64,
) -> WeightPoly {
    if weights.is_null() {
        return WeightPoly { low: std::ptr::null_mut(), high: std::ptr::null_mut() };
    }

    let (l, h) = (*weights).var_weight(VarLabel::new(var));

    // We must clone the polynomials and allocate them on the heap
    // so the Rust `Box` can manage their lifetime, and we can return
    // the pointer to the C caller.
    let low_ptr = Box::into_raw(Box::new(l.clone()));
    let high_ptr = Box::into_raw(Box::new(h.clone()));

    WeightPoly {
        low: low_ptr,
        high: high_ptr,
    }
}

/// Returns the length of the coefficient vector of a Polynomial pointer.
#[no_mangle]
unsafe extern "C" fn polynomial_len(p: *mut PolyWeight) -> usize {
    if p.is_null() {
        return 0;
    }
    // Access the polynomial and return the length of its vector
    (*p).coefficients.len()
}

/// Writes the coefficients of a Polynomial pointer into a C array (buffer).
///
/// # Arguments
/// * `p`: A pointer to the Polynomial object.
/// * `buffer`: A C-allocated buffer of f64, which must have a capacity of at least `polynomial_len(p)`.
/// * `max_len`: The size of the C buffer (for safety).
///
/// Returns the number of coefficients actually written.
#[no_mangle]
unsafe extern "C" fn polynomial_get_coeffs(
    p: *mut PolyWeight,
    buffer: *mut f64,
    max_len: usize,
) -> usize {
    if p.is_null() || buffer.is_null() {
        return 0;
    }

    let poly = &(*p);
    let coeffs = &poly.coefficients;
    let len = coeffs.len().min(max_len);

    let destination_slice = slice::from_raw_parts_mut(buffer, len);

    for i in 0..len {
        destination_slice[i] = coeffs[i].0; // Access the inner f64 of RealSemiring
    }

    len
}

#[no_mangle]
unsafe extern "C" fn bdd_wmc_poly(
    bdd: *const crate::repr::BddPtr,
    weights: *const PolyWmcParams,
) -> *mut PolyWeight {
    if bdd.is_null() || weights.is_null() {
        return std::ptr::null_mut();
    }

    let result = (*bdd).wmc(&*weights);
    // The result is a Semiring value (Polynomial).
    // We clone it into a Box and return the raw pointer to Racket.
    Box::into_raw(Box::new(result.value.clone()))
}
