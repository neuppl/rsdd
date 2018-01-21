//! Backing stores are unique tables which support a `get_or_insert` operation.
mod robin_hood;
mod cuckoo_store;
pub mod bdd_table;
pub mod sdd_table;

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
