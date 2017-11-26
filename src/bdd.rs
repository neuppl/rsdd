use std::hash::{Hash, Hasher};
use std::io::prelude::*;


pub type VarLabel = u16;
/// Index into BDD table
pub type TableIndex = u32;

pub type RefCount = u16;

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



#[derive(Debug)]
pub enum Op {
    BddOr,
    BddAnd
}

