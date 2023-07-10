//! Backing stores are unique tables which support a `get_or_insert` operation.
pub mod bump_table;

use std::hash::Hash;

pub trait UniqueTable<'a, T: Eq + PartialEq + Hash> {
    /// use a hash to allocate space in table, but use strict equality
    fn get_or_insert(&'a mut self, item: T) -> &'a T;
}
