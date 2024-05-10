use crate::{
    builder::{sdd::CompressionSddBuilder, BottomUpBuilder},
    repr::{self, Cnf, DDNNFPtr, VTree, WmcParams},
    util::semirings::RealSemiring,
};

type SddBuilder = CompressionSddBuilder<'static>;
type SddPtr = repr::SddPtr<'static>;

#[allow(clippy::missing_safety_doc)]
#[no_mangle]
pub unsafe extern "C" fn sdd_builder_new(vtree: *mut VTree) -> *mut SddBuilder {
    let vtree = Box::from_raw(vtree);
    Box::into_raw(Box::new(CompressionSddBuilder::new(*vtree)))
}

#[allow(clippy::missing_safety_doc)]
#[no_mangle]
pub unsafe extern "C" fn sdd_builder_compile_cnf(
    builder: *const SddBuilder,
    cnf: *const Cnf,
) -> *mut SddPtr {
    Box::into_raw(Box::new((*builder).compile_cnf(&*cnf)))
}

#[allow(clippy::missing_safety_doc)]
#[no_mangle]
pub unsafe extern "C" fn sdd_wmc(sdd: *const SddPtr, wmc: *const WmcParams<RealSemiring>) -> f64 {
    DDNNFPtr::unsmoothed_wmc(&*sdd, &*wmc).0
}
