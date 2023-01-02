//! Backing stores are unique tables which support a `get_or_insert` operation.
pub mod bump_table;

use std::hash::Hash;

pub trait UniqueTable<T: Eq + PartialEq + Hash> {
    fn get_or_insert(&mut self, item: T) -> *mut T;
}
