use crate::repr::{Cnf, DTree, VarOrder};

#[no_mangle]
unsafe extern "C" fn dtree_from_cnf(cnf: *const Cnf, elim_order: *const VarOrder) -> *mut DTree {
    Box::into_raw(Box::new(DTree::from_cnf(&*cnf, &*elim_order)))
}
