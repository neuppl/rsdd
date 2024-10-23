use crate::repr::{Literal, VarLabel, VarOrder};
use core::slice;

#[no_mangle]
extern "C" fn literal_new(label: VarLabel, polarity: bool) -> Literal {
    Literal::new(label, polarity)
}

#[no_mangle]
unsafe extern "C" fn var_order_new(order: *const VarLabel, len: usize) -> *mut VarOrder {
    let order = slice::from_raw_parts(order, len);
    Box::into_raw(Box::new(VarOrder::new(order)))
}
