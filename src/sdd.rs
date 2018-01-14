//! implements a trimmed SDD as a collection of BDDs

use std::collections::{HashSet, HashMap};
use std::rc::Rc;
use btree::*;
use bdd::*;
use std::mem;
use var_order::VarOrder;
#[macro_use]
use util::*;

/// holds metadata for an SDD pointer
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Copy)]
struct PackedInternalData {
    data: u32
}


BITFIELD!(PackedInternalData data : u32 [
    vtree set_vtree[0..16],
    is_bdd set_is_bdd[16..17],
    is_const set_is_const[17..18],
    compl set_compl[18..19],
]);

impl PackedInternalData {
    fn new(vtree: u16, is_bdd: u32, is_const: u32, compl: u32) -> PackedInternalData {
        let mut n = PackedInternalData { data: 0 };
        n.set_vtree(vtree as u32);
        n.set_is_bdd(is_bdd);
        n.set_is_const(is_const);
        n.set_compl(compl);
        n
    }
}

/// An SddPtr is either (1) a BDD pointer, or (2) a pointer to an SDD node.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Copy)]
pub struct SddPtr {
    /// the index into the table *or* a sub-BDD pointer, depending on the is_bdd
    /// flag
    idx: usize,
    pack: PackedInternalData
}


#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
struct ExternalSdd {
    idx: usize,
}


#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct SddOr {
    pub nodes: Vec<(SddPtr, SddPtr)>,
}

pub enum SddPtrType {
    True,
    False,
    Node
}

impl SddPtr {
    pub fn new_node(idx: usize, vtree: u16) -> SddPtr {
        SddPtr {
            idx: idx,
            pack: PackedInternalData::new(vtree, 0, 0, 0)
        }
    }

    /// negate an SDD pointer
    pub fn neg(&self) -> SddPtr {
        if self.is_bdd() {
            let v = self.as_bdd_ptr();
            SddPtr::new_bdd(v.neg(), self.pack.vtree() as u16)
        } else {
            let mut v = self.clone();
            v.pack.set_compl(if self.is_compl() { 0 } else { 1 });
            v
        }
    }

    pub fn is_compl(&self) -> bool {
        self.pack.compl() == 1
    }

    pub fn new_const(v: bool) -> SddPtr {
        SddPtr {
            idx: 0,
            pack: PackedInternalData::new(0, 0, 1, if v {0} else {1})
        }
    }

    pub fn new_bdd(ptr: BddPtr, vtree: u16) -> SddPtr {
        SddPtr {
            idx: ptr.raw() as usize,
            pack: PackedInternalData::new(vtree, 1, 0, 0)
        }
    }

    /// produce an uncomplemented version of an SDD
    pub fn regular(&self) -> SddPtr {
        let mut v = self.clone();
        v.pack.set_compl(0);
        v
    }

    pub fn is_const(&self) -> bool {
        self.pack.is_const() == 1
    }

    pub fn is_true(&self) -> bool {
        self.is_const() && !self.is_compl()
    }

    pub fn is_false(&self) -> bool {
        self.is_const() && self.is_compl()
    }

    pub fn is_bdd(&self) -> bool {
        self.pack.is_bdd() == 1
    }

    pub fn as_bdd_ptr(&self) -> BddPtr {
        assert!(self.is_bdd());
        if self.is_true() {
            BddPtr::true_node()
        } else if self.is_false() {
            BddPtr::false_node()
        } else {
            BddPtr::from_raw(self.idx as u64)
        }
    }

    pub fn idx(&self) -> usize {
        self.idx
    }

    pub fn vtree(&self) -> usize {
        self.pack.vtree() as usize
    }

    pub fn ptr_type(&self) -> SddPtrType {
        if self.is_false() {
            SddPtrType::False
        } else if self.is_true() {
            SddPtrType::True
        } else {
            SddPtrType::Node
        }
    }
}

/// Represent an SDD as a list of BDD pointers. The ordering of the BDDs
/// corresponds with the depth-first left-first traversal of the vtree
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Sdd {
    Or(SddOr),
    Bdd(BddPtr),
}

pub type VTree = BTree<(), Vec<VarLabel>>;
