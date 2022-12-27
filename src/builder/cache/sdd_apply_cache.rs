use std::collections::HashMap;

use crate::repr::sdd::{SddAnd, SddPtr};

pub struct SddApply {
    table: HashMap<SddAnd, SddPtr>,
}

impl SddApply {
    pub fn new() -> SddApply {
        SddApply {
            table: HashMap::default(),
        }
    }
    pub fn get(&self, and: SddAnd) -> Option<SddPtr> {
        self.table.get(&and).cloned()
    }
    pub fn insert(&mut self, and: SddAnd, ptr: SddPtr) -> () {
        self.table.insert(and, ptr);
    }
}
