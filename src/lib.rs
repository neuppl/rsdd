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


#[no_mangle]
pub extern "C" fn mk_bdd_manager_default_order(numvars: usize) -> *mut libc::c_void {
    let r = Box::new(manager::bdd_manager::BddManager::new_default_order(numvars));
    Box::into_raw(r) as *mut libc::c_void
}

#[no_mangle]
pub extern "C" fn test() -> () {
    println!("hello world");
}
