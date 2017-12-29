//! implements a trimmed SDD as a collection of BDDs

use std::collections::{HashSet, HashMap};
use std::rc::Rc;
use btree::*;
use bdd::*;
use var_order::VarOrder;
#[macro_use]
use util::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Copy)]
pub struct SddPtr {
    /// the index into the table
    idx: usize,
    /// the vtree index for this node
    vtree: u16,
    is_const: bool,
    const_val: bool,
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
            vtree: vtree,
            is_const: false,
            const_val: false,
        }
    }

    pub fn new_const(v: bool, vtree: u16) -> SddPtr {
        SddPtr {
            idx: 0,
            vtree: vtree,
            is_const: true,
            const_val: v,
        }
    }

    pub fn new_bdd(ptr: BddPtr, vtree: u16) -> SddPtr {
        SddPtr {
            idx: ptr.raw() as usize,
            vtree: vtree,
            is_const: false,
            const_val: false,
        }
    }

    pub fn is_const(&self) -> bool {
        self.is_const
    }

    pub fn is_true(&self) -> bool {
        self.const_val && self.is_const
    }

    pub fn is_false(&self) -> bool {
        !self.const_val && self.is_const
    }

    pub fn as_bdd_ptr(&self) -> BddPtr {
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
        self.vtree as usize
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
