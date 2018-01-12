use std::hash::{Hash, Hasher};
use twox_hash;
use std::fmt;
#[macro_use]
use std::mem;

/// number of bits allocated for variable label (limit on total number of
/// variables)
const VAR_BITS: usize = 11;
/// number of bits allocated for a table index (limit on total BDDs of each
/// variable)
const INDEX_BITS: usize = 64 - VAR_BITS - 1; // reserve 1 bit for special

const TRUE_VALUE: u64 = 1; // the variable ID corresponding with a true value


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
        assert!(v < 1 << INDEX_BITS + 1, "overflow");
        TableIndex(v)
    }
    #[inline]
    pub fn value(&self) -> u64 {
        self.0
    }
}

/// A BDD pointer
#[derive(Clone, PartialEq, Eq, Hash, Copy, PartialOrd)]
pub struct BddPtr {
    data: u64,
}

impl fmt::Debug for BddPtr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.ptr_type() {
            PointerType::PtrFalse => write!(f, "BddPtr(F)"),
            PointerType::PtrTrue => write!(f, "BddPtr(T)"),
            PointerType::PtrNode => {
                write!(f, "BddPtr(Cur var: {}, Index: {})", self.var(), self.idx())
            }
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
    compl set_compl[VAR_BITS+1..VAR_BITS+2],
    idx set_idx[(VAR_BITS+2)..64],
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

    pub fn from_raw(raw: u64) -> BddPtr {
        BddPtr { data: raw }
    }
    /// fetch the raw underlying data of the pointer
    #[inline]
    pub fn raw(&self) -> u64 {
        self.data
    }

    #[inline]
    pub fn is_true(&self) -> bool {
        self.special() == 1 && self.var() == TRUE_VALUE && !self.is_compl()
    }

    #[inline]
    pub fn is_false(&self) -> bool {
        self.special() == 1 && self.var() == TRUE_VALUE && self.is_compl()
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
        let v = BddPtr::true_node();
        v.neg()
    }

    #[inline]
    pub fn is_compl(&self) -> bool {
        self.compl() == 1
    }

    /// gets a non-complemented version of self
    #[inline]
    pub fn regular(&self) -> BddPtr {
        let mut new = self.clone();
        new.set_compl(0);
        new
    }

    #[inline]
    pub fn neg(&self) -> BddPtr {
        let mut r = self.clone();
        if self.is_compl() {
            r.set_compl(0);
        } else {
            r.set_compl(1);
        }
        r
    }

    #[inline]
    pub fn label(&self) -> VarLabel {
        VarLabel::new(self.var() as u64)
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
            _ => panic!("called into-node on non-node BDD"),
        }
    }
}

/// The primary BDD storage object
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct ToplessBdd {
    pub low: BddPtr,
    pub high: BddPtr,
}

impl Hash for ToplessBdd {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.low.hash(state);
        self.high.hash(state);
    }
}

impl ToplessBdd {
    pub fn new(low: BddPtr, high: BddPtr) -> ToplessBdd {
        ToplessBdd {
            low: low,
            high: high,
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
pub enum Op {
    BddOr,
    BddAnd,
}
