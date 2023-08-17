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
pub struct RsddBddBuilder {
    _priv: [u8; 0],
}

unsafe fn robdd_builder_from_ptr<'_0>(
    ptr: *mut RsddBddBuilder,
) -> &'_0 mut RobddBuilder<'static, AllIteTable<BddPtr<'static>>> {
    if ptr.is_null() {
        eprintln!("Fatal error, got NULL `Context` pointer");
        ::std::process::abort();
    }
    &mut *(ptr.cast())
}

/// # Safety
///
/// Requires a valid VarOrder pointer; the constructor will copy the moved value.
#[no_mangle]
pub unsafe extern "C" fn robdd_builder_all_table(order: *mut VarOrder) -> *mut RsddBddBuilder {
    if order.is_null() {
        eprintln!("Fatal error, got NULL `order` pointer");
        std::process::abort();
    }

    let order = *Box::from_raw(order);
    Box::into_raw(Box::new(RobddBuilder::<AllIteTable<BddPtr>>::new(order))).cast()
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn robdd_builder_compile_cnf(
    builder: *mut RsddBddBuilder,
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
    builder: *mut RsddBddBuilder,
    bdd: *mut BddPtr<'static>,
) -> u64 {
    let builder = robdd_builder_from_ptr(builder);

    let num_vars = builder.num_vars();

    let smoothed = builder.smooth(*bdd, num_vars);

    let unweighted_params: WmcParams<FiniteField<{ primes::U64_LARGEST }>> =
        WmcParams::new(HashMap::from_iter(
            (0..num_vars as u64)
                .map(|v| (VarLabel::new(v), (FiniteField::one(), FiniteField::one()))),
        ));

    let mc = smoothed.unsmoothed_wmc(&unweighted_params).value();

    mc as u64
}

// implementing the disc interface

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn mk_bdd_manager_default_order(num_vars: u64) -> *mut RsddBddBuilder {
    Box::into_raw(Box::new(RobddBuilder::<AllIteTable<BddPtr>>::new(
        VarOrder::linear_order(num_vars as usize),
    )))
    .cast()
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn bdd_new_var(
    builder: *mut RsddBddBuilder,
    polarity: bool,
) -> *mut BddPtr<'static> {
    let builder = robdd_builder_from_ptr(builder);

    let (_, ptr) = builder.new_var(polarity);

    Box::into_raw(Box::new(ptr))
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn bdd_ite(
    builder: *mut RsddBddBuilder,
    f: *mut BddPtr<'static>,
    g: *mut BddPtr<'static>,
    h: *mut BddPtr<'static>,
) -> *mut BddPtr<'static> {
    let builder = robdd_builder_from_ptr(builder);

    let and = builder.ite(*f, *g, *h);
    Box::into_raw(Box::new(and))
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn bdd_and(
    builder: *mut RsddBddBuilder,
    left: *mut BddPtr<'static>,
    right: *mut BddPtr<'static>,
) -> *mut BddPtr<'static> {
    let builder = robdd_builder_from_ptr(builder);

    let and = builder.and(*left, *right);
    Box::into_raw(Box::new(and))
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn bdd_or(
    builder: *mut RsddBddBuilder,
    left: *mut BddPtr<'static>,
    right: *mut BddPtr<'static>,
) -> *mut BddPtr<'static> {
    let builder = robdd_builder_from_ptr(builder);

    let or = builder.and(*left, *right);
    Box::into_raw(Box::new(or))
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn bdd_negate(
    builder: *mut RsddBddBuilder,
    bdd: *mut BddPtr<'static>,
) -> *mut BddPtr<'static> {
    let builder = robdd_builder_from_ptr(builder);

    let negate = builder.negate(*bdd);
    Box::into_raw(Box::new(negate))
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn bdd_is_true(bdd: *mut BddPtr<'static>) -> bool {
    (*bdd).is_true()
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn bdd_is_false(bdd: *mut BddPtr<'static>) -> bool {
    (*bdd).is_false()
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn bdd_is_const(bdd: *mut BddPtr<'static>) -> bool {
    (*bdd).is_const()
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn bdd_true(builder: *mut RsddBddBuilder) -> *mut BddPtr<'static> {
    let builder = robdd_builder_from_ptr(builder);
    let bdd = builder.true_ptr();
    Box::into_raw(Box::new(bdd))
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn bdd_false(builder: *mut RsddBddBuilder) -> *mut BddPtr<'static> {
    let builder = robdd_builder_from_ptr(builder);
    let bdd = builder.false_ptr();
    Box::into_raw(Box::new(bdd))
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn bdd_eq(
    builder: *mut RsddBddBuilder,
    left: *mut BddPtr<'static>,
    right: *mut BddPtr<'static>,
) -> bool {
    let builder = robdd_builder_from_ptr(builder);
    // let left = bdd_ptr_from_ptr(left);
    // let right = bdd_ptr_from_ptr(right);

    builder.eq(*left, *right)
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn bdd_topvar(bdd: *mut BddPtr<'static>) -> u64 {
    // let bdd = bdd_ptr_from_ptr(bdd);

    match (*bdd).var_safe() {
        Some(x) => x.value(),
        None => 0, // TODO: fix this
    }
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn bdd_low(bdd: *mut BddPtr<'static>) -> *mut BddPtr<'static> {
    Box::into_raw(Box::new((*bdd).low()))
}

#[no_mangle]
#[allow(clippy::missing_safety_doc)]
pub unsafe extern "C" fn bdd_high(bdd: *mut BddPtr<'static>) -> *mut BddPtr<'static> {
    Box::into_raw(Box::new((*bdd).high()))
}
