use std::hash::{Hash, Hasher};
use twox_hash;
#[macro_use]
use util::*;
use std::io::prelude::*;
use std::mem;

/// number of bits allocated for variable label (limit on total number of
/// variables)
const VAR_BITS: usize = 11;
/// number of bits allocated for a table index (limit on total BDDs of each
/// variable)
const INDEX_BITS: usize = 32 - VAR_BITS - 1; // reserve 1 bit for special

const TRUE_VALUE: u32 = 1; // the variable ID corresponding with a true value
const FALSE_VALUE: u32 = 0; // the variable ID corresponding with a false value


/// a label for each distinct variable in the BDD
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarLabel(u32);
impl VarLabel {
    #[inline]
    pub fn new(v: u32) -> VarLabel {
        assert!(v < 1 << VAR_BITS - 1, "Variable identifier overflow");
        VarLabel(v)
    }
    #[inline]
    pub fn value(&self) -> u32 {
        self.0
    }
}

/// Index into BDD table
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TableIndex(u32);
impl TableIndex {
    #[inline]
    pub fn new(v: u32) -> TableIndex {
        assert!(
            v < 1 << INDEX_BITS + 1,
            "Table index overflow; too many BDDs allocated"
        );
        TableIndex(v)
    }
    #[inline]
    pub fn value(&self) -> u32 {
        self.0
    }
}

/// A BDD pointer
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct BddPtr {
    data: u32,
}

pub enum PointerType {
    PtrFalse,
    PtrTrue,
    PtrNode,
}

BITFIELD!(BddPtr data : u32 [
    var set_var[0..VAR_BITS],                  // the variable index
    special set_special[VAR_BITS..VAR_BITS+1], // a special bit of 1 indicates a special BDD node (like true or false)
    idx set_idx[(VAR_BITS+1)..32],
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

    #[inline]
    pub fn is_true(&self) -> bool {
        self.special() == 1 && self.var() == TRUE_VALUE
    }

    #[inline]
    pub fn is_const(&self) -> bool {
        self.special() == 1
    }

    #[inline]
    pub fn ptr_type(&self) -> PointerType {
        if self.is_const() {
            if self.is_true() {
                PointerType::PtrTrue
            } else {
                PointerType::PtrFalse
            }
        } else {
            PointerType::PtrNode
        }
    }

    #[inline]
    pub fn true_node() -> BddPtr {
        let mut v = BddPtr { data: 0 };
        v.set_special(1);
        v.set_var(TRUE_VALUE);
        v
    }

    #[inline]
    pub fn false_node() -> BddPtr {
        let mut v = BddPtr { data: 0 };
        v.set_special(1);
        v.set_var(FALSE_VALUE);
        v
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Bdd {
    BddNode {
        low: BddPtr,
        high: BddPtr,
        var: VarLabel,
    },
    BddTrue,
    BddFalse,
}

impl Bdd {
    pub fn new_node(low: BddPtr, high: BddPtr, var: VarLabel) -> Bdd {
        Bdd::BddNode {
            low: low,
            high: high,
            var: var,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GcBits {}

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
            low: low,
            high: high,
            gc: gc,
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum Op {
    BddOr,
    BddAnd,
}
