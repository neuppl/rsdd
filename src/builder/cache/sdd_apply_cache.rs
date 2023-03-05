use std::collections::HashMap;

use rustc_hash::FxHashMap;

use crate::repr::sdd::{SddAnd, SddPtr};

pub trait SddApply {
    // fn new() -> Self;
    fn get(&self, and: SddAnd) -> Option<SddPtr>;
    fn insert(&mut self, and: SddAnd, ptr: SddPtr);
}

pub struct SddApplyCompression {
    table: FxHashMap<SddAnd, SddPtr>,
}

impl SddApplyCompression {
    pub fn new() -> SddApplyCompression {
        SddApplyCompression {
            table: FxHashMap::default(),
        }
    }
}

impl SddApply for SddApplyCompression {
    fn get(&self, and: SddAnd) -> Option<SddPtr> {
        self.table.get(&and).cloned()
    }
    fn insert(&mut self, and: SddAnd, ptr: SddPtr) {
        self.table.insert(and, ptr);
    }
}

impl Default for SddApplyCompression {
    fn default() -> Self {
        Self::new()
    }
}

pub struct SddApplySemantic {
    table: FxHashMap<u128, SddPtr>,
    prime: u128,
    map: HashMap<usize, u128>,
}

impl SddApplySemantic {
    pub fn new(prime: u128, map: HashMap<usize, u128>) -> SddApplySemantic {
        SddApplySemantic {
            table: FxHashMap::default(),
            prime,
            map,
        }
    }
}

impl SddApply for SddApplySemantic {
    fn get(&self, and: SddAnd) -> Option<SddPtr> {
        let h = and.get_semantic_hash(&self.map, self.prime);
        self.table.get(&h).cloned()
    }
    fn insert(&mut self, and: SddAnd, ptr: SddPtr) {
        let h = and.get_semantic_hash(&self.map, self.prime);
        self.table.insert(h, ptr);
    }
}
