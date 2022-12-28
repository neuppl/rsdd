use fnv::FnvHashMap;

use crate::repr::sdd::{SddAnd, SddPtr};

pub struct SddApply {
    table: FnvHashMap<SddAnd, SddPtr>,
}

impl SddApply {
    pub fn new() -> SddApply {
        SddApply {
            table: FnvHashMap::default(),
        }
    }
    pub fn get(&self, and: SddAnd) -> Option<SddPtr> {
        self.table.get(&and).cloned()
    }
    pub fn insert(&mut self, and: SddAnd, ptr: SddPtr) -> () {
        self.table.insert(and, ptr);
    }
}
