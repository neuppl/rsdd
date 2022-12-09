//! A simple unique table based on a bump allocator and built-in rust 
//! hashmap for checking equality

use bumpalo::Bump;
use std::hash::{Hash, BuildHasherDefault};
use twox_hash::XxHash64;
use super::UniqueTable;
use std::collections::HashMap;

pub struct BumpTable<T: Eq + PartialEq + Hash> {
    table: Bump,
    unique_map: HashMap<T, *mut T, BuildHasherDefault<XxHash64>>
}

impl<T: Eq + PartialEq + Hash> BumpTable<T> {
    pub fn new() -> BumpTable<T> {
        BumpTable { table: Bump::new(), unique_map: Default::default() }
    }
}

impl<T: Eq + PartialEq + Hash> UniqueTable<T> for BumpTable<T> {
    fn get_or_insert(&mut self, item: T) -> *mut T {
        *self.unique_map.entry(item).or_insert_with(|| {
            // allocate a new item
             self.table.alloc(item) as *mut T
        })
    }
}