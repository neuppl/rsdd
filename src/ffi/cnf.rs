use crate::repr::{Cnf, Literal};
use core::slice;

#[repr(C)]
pub struct Clause {
    pub vars: *mut Literal,
    pub len: usize,
}

#[allow(clippy::missing_safety_doc)]
#[no_mangle]
pub unsafe extern "C" fn cnf_new(clauses: *const Clause, len: usize) -> *mut Cnf {
    let clauses = slice::from_raw_parts(clauses, len)
        .iter()
        .map(|c| slice::from_raw_parts(c.vars, c.len).to_vec())
        .collect::<Vec<_>>();
    Box::into_raw(Box::new(Cnf::new(&clauses)))
}
