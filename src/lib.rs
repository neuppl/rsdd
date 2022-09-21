//! Defines exports and the C api
extern crate dimacs;
extern crate fnv;
extern crate im;
extern crate libc;
extern crate maplit;
extern crate num;
extern crate pretty;
extern crate primal;
extern crate rand;
#[macro_use]
extern crate serde;
extern crate bit_set;
extern crate dot;
extern crate petgraph;
extern crate quickcheck;
extern crate segment_tree;
extern crate time_test;
extern crate twox_hash;

#[macro_use]
pub mod util;
mod backing_store;
pub mod builder;
pub mod repr;
pub mod sample;

use crate::builder::bdd_builder::BddManager;
use crate::builder::bdd_plan::BddPlan;
use crate::builder::repr::builder_bdd::BddPtr;
use crate::builder::repr::builder_sdd::{SddPtr, VTree};
use crate::builder::sdd_builder::SddManager;
use crate::builder::*;
use crate::repr::var_label::VarLabel;

/// Creates a vtree leaf
#[no_mangle]
pub extern "C" fn rsdd_vtree_leaf(var: *const u64, len: libc::size_t) -> *mut libc::c_void {
    let arg = unsafe {
        assert!(!var.is_null());
        let slice = std::slice::from_raw_parts(var, len as usize);
        slice.iter().map(|x| VarLabel::new(*x)).collect()
    };
    Box::into_raw(Box::new(VTree::Leaf(arg))) as *mut libc::c_void
}

/// Create a new VTree node.
/// Takes ownership of `l` and `r`.
#[no_mangle]
pub extern "C" fn rsdd_vtree_node(l: *mut VTree, r: *mut VTree) -> *mut libc::c_void {
    let lp = unsafe { Box::from_raw(l) };
    let rp = unsafe { Box::from_raw(r) };
    Box::into_raw(Box::new(VTree::Node((), lp, rp))) as *mut libc::c_void
}

#[no_mangle]
pub extern "C" fn rsdd_sdd_var(mgr: *mut SddManager, label: u64, is_true: bool) -> SddPtr {
    let mgr = unsafe { &mut *mgr };
    mgr.var(VarLabel::new(label), is_true)
}

/// Create a new SDD manager.
/// Takes ownership of `vtree`.
#[no_mangle]
pub extern "C" fn rsdd_mk_sdd_manager(vtree: *mut VTree) -> *mut libc::c_void {
    let vtree = unsafe { Box::from_raw(vtree) };
    let r = Box::new(SddManager::new(*vtree));
    Box::into_raw(r) as *mut libc::c_void
}

/// Create a new BDD manager with number of variables `numvars`
#[no_mangle]
pub extern "C" fn rsdd_mk_bdd_manager_default_order(numvars: usize) -> *mut libc::c_void {
    let r = Box::new(bdd_builder::BddManager::new_default_order(numvars));
    Box::into_raw(r) as *mut libc::c_void
}

#[no_mangle]
pub extern "C" fn rsdd_new_var(mgr: *mut BddManager, is_true: bool) -> u64 {
    let mgr = unsafe { &mut *mgr };
    let lbl = mgr.new_var();
    mgr.var(lbl, is_true).raw()
}

#[no_mangle]
pub extern "C" fn rsdd_var(mgr: *mut BddManager, lbl: u64, is_true: bool) -> u64 {
    let mgr = unsafe { &mut *mgr };
    mgr.var(VarLabel::new(lbl), is_true).raw()
}

#[no_mangle]
pub extern "C" fn rsdd_and(mgr: *mut BddManager, a: u64, b: u64) -> u64 {
    let mgr = unsafe { &mut *mgr };
    mgr.and(BddPtr::from_raw(a), BddPtr::from_raw(b)).raw()
}

#[no_mangle]
pub extern "C" fn rsdd_or(mgr: *mut BddManager, a: u64, b: u64) -> u64 {
    let mgr = unsafe { &mut *mgr };
    mgr.or(BddPtr::from_raw(a), BddPtr::from_raw(b)).raw()
}

#[no_mangle]
pub extern "C" fn rsdd_iff(mgr: *mut BddManager, a: u64, b: u64) -> u64 {
    let mgr = unsafe { &mut *mgr };
    mgr.iff(BddPtr::from_raw(a), BddPtr::from_raw(b)).raw()
}

#[no_mangle]
pub extern "C" fn rsdd_xor(mgr: *mut BddManager, a: u64, b: u64) -> u64 {
    let mgr = unsafe { &mut *mgr };
    mgr.xor(BddPtr::from_raw(a), BddPtr::from_raw(b)).raw()
}

#[no_mangle]
pub extern "C" fn rsdd_ite(mgr: *mut BddManager, a: u64, b: u64, c: u64) -> u64 {
    let mgr = unsafe { &mut *mgr };
    mgr.ite(
        BddPtr::from_raw(a),
        BddPtr::from_raw(b),
        BddPtr::from_raw(c),
    )
    .raw()
}

#[no_mangle]
pub extern "C" fn rsdd_true(mgr: *mut BddManager) -> u64 {
    let mgr = unsafe { &mut *mgr };
    mgr.true_ptr().raw()
}

#[no_mangle]
pub extern "C" fn rsdd_exists(mgr: *mut BddManager, bdd: u64, lbl: u64) -> u64 {
    let mgr = unsafe { &mut *mgr };
    mgr.exists(BddPtr::from_raw(bdd), repr::var_label::VarLabel::new(lbl))
        .raw()
}

#[no_mangle]
pub extern "C" fn rsdd_condition(mgr: *mut BddManager, bdd: u64, lbl: u64, value: bool) -> u64 {
    let mgr = unsafe { &mut *mgr };
    mgr.condition(
        BddPtr::from_raw(bdd),
        repr::var_label::VarLabel::new(lbl),
        value,
    )
    .raw()
}

#[no_mangle]
pub extern "C" fn rsdd_compose(mgr: *mut BddManager, a: u64, label: u64, b: u64) -> u64 {
    let mgr = unsafe { &mut *mgr };
    mgr.compose(
        BddPtr::from_raw(a),
        repr::var_label::VarLabel::new(label),
        BddPtr::from_raw(b),
    )
    .raw()
}

#[no_mangle]
pub extern "C" fn rsdd_size(mgr: *mut BddManager, bdd: u64) -> usize {
    let mgr = unsafe { &mut *mgr };
    mgr.count_nodes(BddPtr::from_raw(bdd))
}

#[no_mangle]
pub extern "C" fn rsdd_false(mgr: *mut BddManager) -> u64 {
    let mgr = unsafe { &mut *mgr };
    mgr.false_ptr().raw()
}

#[no_mangle]
pub extern "C" fn rsdd_is_false(mgr: *mut BddManager, ptr: u64) -> bool {
    let mgr = unsafe { &mut *mgr };
    mgr.is_false(BddPtr::from_raw(ptr))
}

#[no_mangle]
pub extern "C" fn rsdd_is_true(mgr: *mut BddManager, ptr: u64) -> bool {
    let mgr = unsafe { &mut *mgr };
    mgr.is_true(BddPtr::from_raw(ptr))
}

#[no_mangle]
pub extern "C" fn rsdd_is_var(_mgr: *mut BddManager, ptr: u64) -> bool {
    let ptr = BddPtr::from_raw(ptr);
    !ptr.is_const()
}

#[no_mangle]
pub extern "C" fn rsdd_topvar(mgr: *mut BddManager, ptr: u64) -> u64 {
    let mgr = unsafe { &mut *mgr };
    mgr.topvar(BddPtr::from_raw(ptr)).value()
}

#[no_mangle]
pub extern "C" fn rsdd_negate(mgr: *mut BddManager, ptr: u64) -> u64 {
    let mgr = unsafe { &mut *mgr };
    mgr.negate(BddPtr::from_raw(ptr)).raw()
}

#[no_mangle]
pub extern "C" fn rsdd_eq_bdd(mgr: *mut BddManager, a: u64, b: u64) -> bool {
    let mgr = unsafe { &mut *mgr };
    mgr.eq_bdd(BddPtr::from_raw(a), BddPtr::from_raw(b))
}

#[no_mangle]
pub extern "C" fn rsdd_low(mgr: *mut BddManager, a: u64) -> u64 {
    let mgr = unsafe { &mut *mgr };
    let bddptr = BddPtr::from_raw(a);
    let low = mgr.low(bddptr);
    if bddptr.is_compl() {
        low.neg().raw()
    } else {
        low.raw()
    }
}

#[no_mangle]
pub extern "C" fn rsdd_high(mgr: *mut BddManager, a: u64) -> u64 {
    let mgr = unsafe { &mut *mgr };
    let bddptr = BddPtr::from_raw(a);
    let high = mgr.high(bddptr);
    if bddptr.is_compl() {
        high.neg().raw()
    } else {
        high.raw()
    }
}

#[no_mangle]
pub extern "C" fn rsdd_print_stats(mgr: *mut BddManager) {
    let mgr = unsafe { &mut *mgr };
    mgr.print_stats();
}

#[no_mangle]
pub extern "C" fn rsdd_num_recursive_calls(mgr: *mut BddManager) -> u64 {
    let mgr = unsafe { &mut *mgr };
    mgr.num_recursive_calls() as u64
}

#[no_mangle]
pub extern "C" fn rsdd_plan_literal(label: u64, polarity: bool) -> *const BddPlan {
    Box::into_raw(Box::new(BddPlan::Literal(label, polarity)))
}

#[no_mangle]
pub extern "C" fn rsdd_plan_const_true() -> *const BddPlan {
    Box::into_raw(Box::new(BddPlan::ConstTrue))
}

#[no_mangle]
pub extern "C" fn rsdd_plan_const_false() -> *mut BddPlan {
    Box::into_raw(Box::new(BddPlan::ConstFalse))
}

/// Takes ownership of `a` and `b`, returns a `BddPlan` representing the conjunction
#[no_mangle]
pub extern "C" fn rsdd_plan_and(a: *mut BddPlan, b: *mut BddPlan) -> *mut BddPlan {
    unsafe {
        let a = Box::from_raw(a);
        let b = Box::from_raw(b);
        Box::into_raw(Box::new(BddPlan::And(a, b)))
    }
}

/// Takes ownership of `a` and `b`, returns a `BddPlan` representing the conjunction
#[no_mangle]
pub extern "C" fn rsdd_plan_or(a: *mut BddPlan, b: *mut BddPlan) -> *mut BddPlan {
    unsafe {
        let a = Box::from_raw(a);
        let b = Box::from_raw(b);
        Box::into_raw(Box::new(BddPlan::Or(a, b)))
    }
}

/// Takes ownership of `a` and `b`, returns a `BddPlan` representing the conjunction
#[no_mangle]
pub extern "C" fn rsdd_plan_iff(a: *mut BddPlan, b: *mut BddPlan) -> *mut BddPlan {
    unsafe {
        let a = Box::from_raw(a);
        let b = Box::from_raw(b);
        Box::into_raw(Box::new(BddPlan::Iff(a, b)))
    }
}

/// Takes ownership of `a` and `b`, returns a `BddPlan` representing the conjunction
#[no_mangle]
pub extern "C" fn rsdd_plan_ite(f: *mut BddPlan, g: *mut BddPlan, h: *mut BddPlan) -> *mut BddPlan {
    unsafe {
        let f = Box::from_raw(f);
        let g = Box::from_raw(g);
        let h = Box::from_raw(h);
        Box::into_raw(Box::new(BddPlan::Ite(f, g, h)))
    }
}

/// Frees a plan object
#[no_mangle]
pub extern "C" fn rsdd_plan_free(a: *mut BddPlan) {
    unsafe {
        let _a = Box::from_raw(a);
    }
}

#[no_mangle]
pub extern "C" fn rsdd_compile_plan(mgr: *mut BddManager, plan: *mut BddPlan) -> u64 {
    unsafe {
        let mat = &mut *mgr;
        mat.compile_plan(&*plan).raw()
    }
}
