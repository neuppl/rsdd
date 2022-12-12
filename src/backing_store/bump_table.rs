//! A simple unique table based on a bump allocator and built-in rust
//! hashmap for checking equality

use super::UniqueTable;
use bumpalo::Bump;
use fnv::FnvHashMap;
use std::hash::Hash;

pub struct BumpTable<T: Eq + PartialEq + Hash> {
    table: Bump,
    unique_map: FnvHashMap<T, *mut T>,
}

impl<T: Eq + PartialEq + Hash + Clone> BumpTable<T> {
    pub fn new() -> BumpTable<T> {
        BumpTable {
            table: Bump::new(),
            unique_map: Default::default(),
        }
    }
}

impl<T: Eq + PartialEq + Hash + Clone> UniqueTable<T> for BumpTable<T> {
    fn get_or_insert(&mut self, item: T) -> *mut T {
        let i = self.unique_map.get(&item).cloned();
        match i {
            None => {
                let ptr = self.table.alloc(item.clone());
                self.unique_map.insert(item, ptr);
                return ptr;
            }
            Some(v) => v,
        }
    }
}
