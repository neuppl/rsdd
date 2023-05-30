//! Backing stores are unique tables which support a `get_or_insert` operation.
pub mod bump_table;

use std::hash::Hash;

pub trait UniqueTable<'a, T: Eq + PartialEq + Hash> {
    fn get_by_hash(&'a mut self, hash: u64) -> Option<&'a T>;
    /// use a hash to allocate space in table, but use strict equality
    fn get_or_insert(&'a mut self, item: T) -> &'a T;
    /// use a hash to both allocate space in table, *and* form equality
    fn get_or_insert_by_hash(&'a mut self, hash: u64, item: T, equality_by_hash: bool) -> &'a T;
}
