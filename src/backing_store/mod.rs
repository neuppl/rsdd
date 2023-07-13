//! Backing stores are unique tables which support a `get_or_insert` operation.
mod bump_table;

use std::hash::Hash;

pub use self::bump_table::*;

pub trait UniqueTable<'a, T: Eq + Hash> {
    /// use a hash to allocate space in table, but use strict equality for probing/checking
    fn get_or_insert(&'a mut self, item: T) -> &'a T;
}
