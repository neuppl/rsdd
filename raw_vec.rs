//! A raw vector implementation

/// Proper heap allocation is still unstable, so we allocate a vector and then
/// forget it.
unsafe fn alloc<T>(count: usize, zero: bool) -> *mut T {
    let mut dummy: Vec<T> = Vec::with_capacity(count);
    let ptr = dummy.as_mut_ptr();
    if zero {
        ptr::write_bytes(ptr, 0, count);
    }
    mem::forget(dummy);
    return ptr;
}

/// Re-cast the pointer to a vector so that it can be properly deallocated
unsafe fn dealloc<T>(p: *mut T, count: usize) {
    let _dummy: Vec<T> = Vec::from_raw_parts(p, 0, count);
    // Dummy is dropped and the memory is freed
}
