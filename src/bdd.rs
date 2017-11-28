use std::hash::{Hash, Hasher};
use twox_hash;
use std::io::prelude::*;

/// a label for each distinct variable in the BDD
pub type VarLabel = u16;
/// Index into BDD table
pub type TableIndex = u32;

/// A BDD pointer
#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct BddPtr {
    var: VarLabel,
    /// The index is a packed representation with the high 8-bits representing a
    /// sub-table, and the lower 24-bits representing an index into the sub-table
    idx: TableIndex
}

impl BddPtr {
    pub fn get_subtable(&self) -> TableIndex {
        self.idx >> 24
    }
    pub fn get_index(&self) -> TableIndex {
        self.idx & 0xffffff
    }
    pub fn get_var(&self) -> VarLabel {
        self.var
    }
    /// Generate a new BddPtr for a particular table at index idx
    pub fn new(var: VarLabel, table: TableIndex, idx: TableIndex) -> BddPtr {
        let new_idx = (table << 24) | (idx & 0xffffff);
        BddPtr {var: var, idx: new_idx}
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GcBits {

}

impl GcBits {
    pub fn new() -> GcBits {
        GcBits {}
    }
}

/// The primary BDD storage object
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToplessBdd {
    pub low: BddPtr,
    pub high: BddPtr,
    gc: GcBits,
}

impl ToplessBdd {
    pub fn new(low: BddPtr, high: BddPtr, gc: GcBits) -> ToplessBdd {
        ToplessBdd { 
            low: low, high:high, gc: gc
        }
    }
    /// compute a 64-bit hash of this value
    pub fn hash(&self) -> u64 {
        let mut hasher = twox_hash::XxHash::with_seed(0xdeadbeef);
        hasher.write_u32(self.low.idx);
        hasher.write_u16(self.low.var);
        hasher.write_u32(self.high.idx);
        hasher.write_u16(self.high.var);
        hasher.finish()
    }

}

#[derive(Debug)]
pub enum Op {
    BddOr,
    BddAnd
}

