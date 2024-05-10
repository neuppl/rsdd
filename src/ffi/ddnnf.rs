use crate::{
    builder::decision_nnf::{DecisionNNFBuilder, StandardDecisionNNFBuilder},
    ffi::bdd::BddPtr,
    repr::{Cnf, VarOrder},
};

pub type DDNNFBuilder = StandardDecisionNNFBuilder<'static>;

#[allow(clippy::missing_safety_doc)]
#[no_mangle]
pub unsafe extern "C" fn ddnnf_builder_new(order: *mut VarOrder) -> *mut DDNNFBuilder {
    Box::into_raw(Box::new(DDNNFBuilder::new(*Box::from_raw(order))))
}

#[allow(clippy::missing_safety_doc)]
#[no_mangle]
pub unsafe extern "C" fn ddnnf_builder_compile_cnf_topdown(
    builder: *const DDNNFBuilder,
    cnf: *const Cnf,
) -> *mut BddPtr {
    Box::into_raw(Box::new((*builder).compile_cnf_topdown(&*cnf)))
}
