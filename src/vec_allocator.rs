//! A specialized allocator for creating vectors of the same type. There will be
//! two types of vectors stored in a vecalloc:
//! 1. Short-lived vectors, which may be efficiently reclaimed and resized.
//! 2. Persistent vectors which cannot be reclaimed without remapping pointers.
//!    The size of these vectors must be known a-priori


/// Store an address into `long_store_sz` to know how long this array is
pub struct PersistentPtr(usize);

/// Store an address into `short_store`
pub struct ShortPtr(usize);

pub struct VecAlloc<T> {
    /// a long-lived storage area
    long_store: Vec<T>,
    /// identifies the end
    long_store_sz: Vec<usize>,
    short_store: Vec<Vec<T>>,
    /// maintains a stack of the current unused short store
    /// (e.g., the top element is the next position to use)
    short_store_free_stack: Vec<usize>
}

impl<T> VecAlloc<T> {
    fn new() -> VecAlloc<T> {
        let mut initial_short_store = Vec::new();
        VecAlloc {
            long_store: Vec::with_capacity(10000),
            long_store_sz: Vec::with_capacity(10000),
            short_store: initial_short_store,
            short_store_free_stack: Vec::new()
        }
    }

    pub fn alloc_short(&mut self) -> ShortPtr {
        if self.short_store_free_stack.is_empty() {
            self.short_store.push(Vec::with_capacity(25));
            ShortPtr(self.short_store.len() - 1)
        } else {
            ShortPtr(self.short_store_free_stack.pop().unwrap())
        }
    }
}

