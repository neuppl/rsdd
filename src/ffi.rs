use crate::{builder::{bdd::RobddBuilder, cache::AllIteTable, BottomUpBuilder}, repr::{VarOrder, BddPtr, Cnf}};

#[no_mangle]
pub extern "C" fn var_order_linear(num_vars: usize) -> *const VarOrder  {
  Box::into_raw(Box::new(VarOrder::linear_order(num_vars)))
}

/// # Safety
///
/// Requires a valid VarOrder pointer; the constructor will copy the moved value.
#[no_mangle]
pub unsafe extern "C" fn robdd_builder_all_table(order: *mut VarOrder) -> *mut RobddBuilder<'static, AllIteTable<BddPtr<'static>>> {
  if order.is_null() {
    eprintln!("Fatal error, got NULL `order` pointer");
    std::process::abort();
  }

  let order = *Box::from_raw(order);
  Box::into_raw(Box::new(RobddBuilder::<AllIteTable<BddPtr>>::new(order)))
}

pub unsafe extern "C" fn robdd_builder_compile_cnf(builder: *mut RobddBuilder<'_, AllIteTable<BddPtr<'_>>>, cnf: *mut Cnf) -> *mut BddPtr<'_> {
  if builder.is_null() {
    eprintln!("Fatal error, got NULL `builder` pointer");
    std::process::abort();
  }

  if cnf.is_null() {
    eprintln!("Fatal error, got NULL `cnf` pointer");
    std::process::abort();
  }

  let builder = *Box::from_raw(builder);
  let cnf = *Box::from_raw(cnf);
  Box::into_raw(Box::new(builder.compile_cnf(&cnf)))
}
