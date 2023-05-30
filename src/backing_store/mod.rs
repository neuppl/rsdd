//! Backing stores are unique tables which support a `get_or_insert` operation.
pub mod bump_table;

use std::hash::Hash;

pub trait UniqueTable<'a, T: Eq + PartialEq + Hash> {
    fn get_by_hash(&'a mut self, hash: u64) -> Option<&'a T>;
    fn get_or_insert(&'a mut self, item: T) -> &'a T;
    fn get_or_insert_by_hash(&'a mut self, hash: u64, item: T) -> &'a T;
}
