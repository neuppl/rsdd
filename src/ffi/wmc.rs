use crate::{
    repr::{VarLabel, WmcParams},
    util::semirings::{Complex, RealSemiring},
};
use std::collections::HashMap;

#[no_mangle]
unsafe extern "C" fn new_wmc_params_f64() -> *mut WmcParams<RealSemiring> {
    Box::into_raw(Box::new(WmcParams::new(HashMap::from([]))))
}

#[no_mangle]
unsafe extern "C" fn new_wmc_params_complex() -> *mut WmcParams<Complex> {
    Box::into_raw(Box::new(WmcParams::new(HashMap::from([]))))
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
