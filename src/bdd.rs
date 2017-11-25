use std::hash::{Hash, Hasher};
use std::io::prelude::*;


pub type VarLabel = u16;
/// Index into BDD table
pub type TableIndex = u16;

pub type RefCount = u8;

/// A BDD pointer
#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct BddPtr {
    pub var: VarLabel,
    pub idx: TableIndex
}



/// Raw BDD data
#[derive(Debug, Hash)]
pub struct Bdd {
    pub var: VarLabel,
    pub low: BddPtr,
    pub high: BddPtr,
}

/// A reference-counted BDD
#[derive(Debug, Hash)]
pub struct BddRc {
    pub var: VarLabel,
    pub low: BddPtr,
    pub high: BddPtr,
    pub rc: RefCount
}

#[derive(Debug)]
pub enum Op {
    BddOr,
    BddAnd
}

