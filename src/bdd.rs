use std::hash::{Hash, Hasher};
use twox_hash;
#[macro_use] use util::*;
use std::io::prelude::*;
use std::mem;

/// number of bits allocated for variable label (limit on total number of
/// variables)
const VAR_BITS: usize = 12;
/// number of bits allocated for a table index (limit on total BDDs of each
/// variable)
const INDEX_BITS: usize = 32 - VAR_BITS;

/// a label for each distinct variable in the BDD
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarLabel (u32);
impl VarLabel {
    #[inline]
    pub fn new(v: u32) -> VarLabel {
        assert!(v < 1 << VAR_BITS, "Variable identifier overflow");
        VarLabel(v)
    }
    #[inline]
    pub fn value(&self) -> u32 {
        self.0
    }
}

/// Index into BDD table
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TableIndex (u32);
impl TableIndex {
    #[inline]
    pub fn new(v: u32) -> TableIndex {
        assert!(v < 1 << INDEX_BITS, "Table index overflow; too many BDDs allocated");
        TableIndex(v)
    }
    #[inline]
    pub fn value(&self) -> u32 {
        self.0
    }
}

/// A BDD pointer
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BddPtr {
    data: u32
}

BITFIELD!(BddPtr data : u32 [
    var set_var[0..VAR_BITS],
    idx set_idx[VAR_BITS..32],
]);

impl BddPtr {
    /// Generate a new BddPtr for a particular table at index idx
    #[inline]
    pub fn new(var: VarLabel, idx: TableIndex) -> BddPtr {
        let mut v = BddPtr { data: 0 };
        v.set_idx(idx.value());
        v.set_var(var.value() as u32);
        v
    }
    /// fetch the raw underlying data of the pointer
    #[inline]
    pub fn raw(&self) -> u32 {
        self.data
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Bdd {
    low: BddPtr,
    high: BddPtr,
    var: VarLabel
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
}

#[derive(Debug)]
pub enum Op {
    BddOr,
    BddAnd
}

