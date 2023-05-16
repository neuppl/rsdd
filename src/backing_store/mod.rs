//! Backing stores are unique tables which support a `get_or_insert` operation.
pub mod bump_table;

use std::hash::Hash;
use std::hash::Hasher;

use rustc_hash::FxHasher;

pub trait UniqueTable<'a, T: Eq + PartialEq + Hash, H: UniqueTableHasher<T>> {
    fn get_by_hash(&'a self, hash: u64) -> Option<&'a T>;
    fn get_or_insert_by_hash(&'a mut self, item: T, hash: u64) -> &'a T;
    fn get_or_insert(&'a mut self, item: T, hasher: &H) -> &'a T;
}

pub trait UniqueTableHasher<T> {
    fn u64hash(&self, elem: &T) -> u64;
}

pub struct DefaultUniqueTableHasher {}

impl<T: Hash> UniqueTableHasher<T> for DefaultUniqueTableHasher {
    fn u64hash(&self, elem: &T) -> u64 {
        let mut hasher = FxHasher::default();
        elem.hash(&mut hasher);
        hasher.finish()
    }
}

impl Default for DefaultUniqueTableHasher {
    fn default() -> Self {
        Self {}
    }
}
