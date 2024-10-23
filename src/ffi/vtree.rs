use crate::repr::{DTree, VTree};
use std::ptr;

#[no_mangle]
unsafe extern "C" fn vtree_from_dtree(dtree: *const DTree) -> *mut VTree {
    VTree::from_dtree(&*dtree).map_or(ptr::null_mut(), |v| Box::into_raw(Box::new(v)))
}
