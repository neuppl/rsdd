//! Base types for backing-store

/// Pointer into the backing store
#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub struct BackingPtr(pub u32);


#[derive(Clone, Debug)]
pub struct BackingCacheStats {
    pub lookup_count: usize,
    pub hit_count: usize,
    pub num_elements: usize,
}


impl BackingCacheStats {
    pub fn new() -> BackingCacheStats {
        BackingCacheStats {
            lookup_count: 0,
            hit_count: 0,
            num_elements: 0,
        }
    }
}
