//! implements an SDD as a collection of BDDs

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
    pub idx: usize,
    pub tbl: u16,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
struct ExternalSdd {
    idx: usize,
}


#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct SddOr {
    pub nodes: Vec<(SddPtr, SddPtr)>
}


impl SddPtr {
    pub fn new(idx: usize, tbl: u16) -> SddPtr {
        SddPtr { idx: idx, tbl: tbl }
    }
    pub fn new_bdd(ptr: BddPtr, tbl: u16) -> SddPtr {
        SddPtr {idx: ptr.raw() as usize, tbl: tbl}
    }
    pub fn as_bdd_ptr(&self) -> BddPtr {
        BddPtr::from_raw(self.idx as u64)
    }
    pub fn idx(&self) -> usize {
        self.idx
    }
    pub fn tbl(&self) -> usize {
        self.tbl as usize
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
