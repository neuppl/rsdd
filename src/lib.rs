#[macro_use] mod util;
pub mod manager;
pub mod repr;
mod backing_store;

extern crate rand;
extern crate fnv;
extern crate twox_hash;
extern crate quickersort;
extern crate dimacs;
extern crate pretty;
extern crate num;
extern crate maplit;
extern crate libc;

use manager::*;
use manager::bdd_manager::BddManager;
use repr::bdd::BddPtr;
use repr::var_label::VarLabel;

// Define the public C API for rsdd
// Note: this API hides complemented edges and other BDD internals

#[no_mangle]
pub extern "C" fn rsdd_mk_bdd_manager_default_order(numvars: usize) -> *mut libc::c_void {
    let r = Box::new(bdd_manager::BddManager::new_default_order(numvars));
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
    mgr.ite(BddPtr::from_raw(a), BddPtr::from_raw(b), BddPtr::from_raw(c)).raw()
}

#[no_mangle]
pub extern "C" fn rsdd_true(mgr: *mut BddManager) -> u64 {
    let mgr = unsafe { &mut *mgr };
    mgr.true_ptr().raw()
}

#[no_mangle]
pub extern "C" fn rsdd_exists(mgr: *mut BddManager, bdd: u64, lbl: u64) -> u64 {
    let mgr = unsafe { &mut *mgr };
    mgr.exists(BddPtr::from_raw(bdd), repr::var_label::VarLabel::new(lbl)).raw()
}

#[no_mangle]
pub extern "C" fn rsdd_condition(mgr: *mut BddManager, bdd: u64, lbl: u64, value: bool) -> u64 {
    let mgr = unsafe { &mut *mgr };
    mgr.condition(BddPtr::from_raw(bdd), repr::var_label::VarLabel::new(lbl), value).raw()
}

#[no_mangle]
pub extern "C" fn rsdd_compose(mgr: *mut BddManager, a: u64, label: u64, b: u64) -> u64 {
    let mgr = unsafe { &mut *mgr };
    mgr.compose(BddPtr::from_raw(a), repr::var_label::VarLabel::new(label), 
                BddPtr::from_raw(b)).raw()
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
pub extern "C" fn rsdd_is_var(mgr: *mut BddManager, ptr: u64) -> bool {
    let mgr = unsafe { &mut *mgr };
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
pub extern "C" fn rsdd_print_stats(mgr: *mut BddManager) -> () {
    let mgr = unsafe { &mut *mgr };
    mgr.print_stats();
}
