use std::os::raw::c_char;
use std::{collections::HashMap, ffi::CStr};

use crate::repr::DDNNFPtr;
use crate::util::semirings::Semiring;
use crate::{
    builder::{bdd::RobddBuilder, cache::AllIteTable, BottomUpBuilder},
    constants::primes,
    repr::{BddPtr, Cnf, VarLabel, VarOrder, WmcParams},
    util::semirings::FiniteField,
};

#[no_mangle]
pub extern "C" fn var_order_linear(num_vars: usize) -> *const VarOrder {
    Box::into_raw(Box::new(VarOrder::linear_order(num_vars)))
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn cnf_from_dimacs(dimacs_str: *const c_char) -> *const Cnf {
    let cstr = CStr::from_ptr(dimacs_str);

    Box::into_raw(Box::new(Cnf::from_dimacs(&String::from_utf8_lossy(
        cstr.to_bytes(),
    ))))
}

// directly inspired by https://users.rust-lang.org/t/how-to-deal-with-lifetime-when-need-to-expose-through-ffi/39583
// and the follow-up at https://users.rust-lang.org/t/can-someone-explain-why-this-is-working/82324/6
#[repr(C)]
pub struct PhantomContext {
    _priv: [u8; 0],
}

/// # Safety
///
/// Requires a valid VarOrder pointer; the constructor will copy the moved value.
#[no_mangle]
pub unsafe extern "C" fn robdd_builder_all_table(order: *mut VarOrder) -> *mut PhantomContext {
    if order.is_null() {
        eprintln!("Fatal error, got NULL `order` pointer");
        std::process::abort();
    }

    let order = *Box::from_raw(order);
    Box::into_raw(Box::new(RobddBuilder::<AllIteTable<BddPtr>>::new(order))).cast()
}

unsafe fn robdd_builder_from_ptr<'_0>(
    ptr: *mut PhantomContext,
) -> &'_0 mut RobddBuilder<'static, AllIteTable<BddPtr<'static>>> {
    if ptr.is_null() {
        eprintln!("Fatal error, got NULL `Context` pointer");
        ::std::process::abort();
    }
    &mut *(ptr.cast())
}

unsafe fn bdd_ptr_from_ptr<'_0>(ptr: *mut PhantomContext) -> &'_0 mut BddPtr<'static> {
    if ptr.is_null() {
        eprintln!("Fatal error, got NULL `Context` pointer");
        ::std::process::abort();
    }
    &mut *(ptr.cast())
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn robdd_builder_compile_cnf(
    builder: *mut PhantomContext,
    cnf: *mut Cnf,
) -> *mut BddPtr<'static> {
    if cnf.is_null() {
        eprintln!("Fatal error, got NULL `cnf` pointer");
        std::process::abort();
    }

    let builder = robdd_builder_from_ptr(builder);
    let cnf = *Box::from_raw(cnf);
    let ptr = builder.compile_cnf(&cnf);
    Box::into_raw(Box::new(ptr))
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn robdd_model_count(
    builder: *mut PhantomContext,
    bdd: *mut PhantomContext,
) -> u64 {
    let builder = robdd_builder_from_ptr(builder);
    let bdd = bdd_ptr_from_ptr(bdd);

    let num_vars = builder.num_vars();

    let smoothed = builder.smooth(*bdd, num_vars);

    let unweighted_params: WmcParams<FiniteField<{ primes::U64_LARGEST }>> =
        WmcParams::new(HashMap::from_iter(
            (0..num_vars as u64)
                .map(|v| (VarLabel::new(v), (FiniteField::one(), FiniteField::one()))),
        ));

    let mc = smoothed
        .wmc(&VarOrder::linear_order(num_vars), &unweighted_params)
        .value();

    mc as u64
}
