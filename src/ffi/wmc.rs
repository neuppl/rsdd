use crate::{
    repr::{VarLabel, WmcParams},
    util::semirings::RealSemiring,
};
use std::collections::HashMap;

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn new_wmc_params_f64() -> *mut WmcParams<RealSemiring> {
    Box::into_raw(Box::new(WmcParams::new(HashMap::from([]))))
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn wmc_param_f64_set_weight(
    weights: *mut WmcParams<RealSemiring>,
    var: u64,
    low: f64,
    high: f64,
) {
    (*weights).set_weight(VarLabel::new(var), RealSemiring(low), RealSemiring(high))
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct WeightF64(pub f64, pub f64);

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn wmc_param_f64_var_weight(
    weights: *mut WmcParams<RealSemiring>,
    var: u64,
) -> WeightF64 {
    let (l, h) = (*weights).var_weight(VarLabel::new(var));
    WeightF64(l.0, h.0)
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn weight_f64_lo(w: WeightF64) -> f64 {
    w.0
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn weight_f64_hi(w: WeightF64) -> f64 {
    w.1
}
