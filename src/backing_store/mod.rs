//! Backing stores are unique tables which support a `get_or_insert` operation.
pub mod bdd_table_robinhood;
mod robin_hood;
pub mod sdd_table;

/// Pointer into the backing store
#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub struct BackingPtr(pub u32);

#[derive(Clone, Debug)]
pub struct BackingCacheStats {
    pub lookup_count: usize,
    pub hit_count: usize,
    pub num_elements: usize,
    pub avg_offset: f64,
}

impl BackingCacheStats {
    pub fn new() -> BackingCacheStats {
        BackingCacheStats {
            lookup_count: 0,
            hit_count: 0,
            num_elements: 0,
            avg_offset: 0.0,
        }
    }
}
