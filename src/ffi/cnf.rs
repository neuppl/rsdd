use crate::repr::{Cnf, Literal, VarOrder};
use core::slice;

#[repr(C)]
pub struct Clause {
    pub vars: *mut Literal,
    pub len: usize,
}

#[no_mangle]
pub unsafe extern "C" fn cnf_new(clauses: *const Clause, len: usize) -> *mut Cnf {
    let clauses = slice::from_raw_parts(clauses, len)
        .iter()
        .map(|c| slice::from_raw_parts(c.vars, c.len).to_vec())
        .collect::<Vec<_>>();
    Box::into_raw(Box::new(Cnf::new(&clauses)))
}

#[no_mangle]
pub unsafe extern "C" fn cnf_min_fill_order(cnf: *mut Cnf) -> *mut VarOrder {
    Box::into_raw(Box::new((*cnf).min_fill_order()))
}
