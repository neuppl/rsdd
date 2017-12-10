use std::hash::{Hash, Hasher};
use std::fmt;
#[macro_use]
use util::*;
use std::io::prelude::*;
use std::mem;

/// number of bits allocated for variable label (limit on total number of
/// variables)
const VAR_BITS: usize = 11;
/// number of bits allocated for a table index (limit on total BDDs of each
/// variable)
const INDEX_BITS: usize = 64 - VAR_BITS - 1; // reserve 1 bit for special

const TRUE_VALUE: u64 = 1; // the variable ID corresponding with a true value
const FALSE_VALUE: u64 = 0; // the variable ID corresponding with a false value


/// a label for each distinct variable in the BDD
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct VarLabel(u64);
impl VarLabel {
    #[inline]
    pub fn new(v: u64) -> VarLabel {
        assert!(v < 1 << VAR_BITS - 1, "Variable identifier overflow");
        VarLabel(v)
    }
    #[inline]
    pub fn value(&self) -> u64 {
        self.0
    }
}

/// Index into BDD table
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TableIndex(u64);
impl TableIndex {
    #[inline]
    pub fn new(v: u64) -> TableIndex {
        assert!(
            v < 1 << INDEX_BITS + 1,
            "Table index overflow; too many BDDs allocated"
        );
        TableIndex(v)
    }
    #[inline]
    pub fn value(&self) -> u64 {
        self.0
    }
}

/// A BDD pointer
#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub struct BddPtr {
    data: u64,
}

impl fmt::Debug for BddPtr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.ptr_type() {
            PointerType::PtrFalse => write!(f, "BddPtr(F)"),
            PointerType::PtrTrue => write!(f, "BddPtr(T)"),
            PointerType::PtrNode =>
                write!(f, "BddPtr(Cur var: {}, Index: {})", self.var(), self.idx())
        }
    }
}

pub enum PointerType {
    PtrFalse,
    PtrTrue,
    PtrNode,
}

BITFIELD!(BddPtr data : u64 [
    var set_var[0..VAR_BITS],                  // the variable index
    special set_special[VAR_BITS..VAR_BITS+1], // a special bit of 1 indicates a special BDD node (like true or false)
    idx set_idx[(VAR_BITS+1)..64],
]);

impl BddPtr {
    /// Generate a new BddPtr for a particular table at index idx
    #[inline]
    pub fn new(var: VarLabel, idx: TableIndex) -> BddPtr {
        let mut v = BddPtr { data: 0 };
        v.set_idx(idx.value());
        v.set_var(var.value());
        v
    }
    /// fetch the raw underlying data of the pointer
    #[inline]
    pub fn raw(&self) -> u64 {
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
pub struct BddNode {
    pub low: BddPtr,
    pub high: BddPtr,
    pub var: VarLabel,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Bdd {
    Node(BddNode),
    BddTrue,
    BddFalse,
}

impl Bdd {
    pub fn new_node(low: BddPtr, high: BddPtr, var: VarLabel) -> Bdd {
        let new_n = BddNode {
            low: low,
            high: high,
            var: var,
        };
        Bdd::Node(new_n)
    }

    pub fn into_node(&self) -> BddNode {
        match self {
            &Bdd::Node(ref n) => n.clone(),
            _ => panic!("called into-node on non-node BDD")
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

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
pub enum Op {
    BddOr,
    BddAnd,
}
