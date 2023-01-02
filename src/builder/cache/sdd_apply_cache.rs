use rustc_hash::FxHashMap;

use crate::repr::sdd::{SddAnd, SddPtr};

pub struct SddApply {
    table: FxHashMap<SddAnd, SddPtr>,
}

impl SddApply {
    pub fn new() -> SddApply {
        SddApply {
            table: FxHashMap::default(),
        }
    }
    pub fn get(&self, and: SddAnd) -> Option<SddPtr> {
        self.table.get(&and).cloned()
    }
    pub fn insert(&mut self, and: SddAnd, ptr: SddPtr) -> () {
        self.table.insert(and, ptr);
    }
}
