use crate::repr::{VTree, VarLabel};
use core::slice;

#[allow(clippy::missing_safety_doc)]
#[no_mangle]
pub unsafe extern "C" fn vtree_left_linear(order: *const VarLabel, len: usize) -> *mut VTree {
    let order = slice::from_raw_parts(order, len);
    Box::into_raw(Box::new(VTree::left_linear(order)))
}
