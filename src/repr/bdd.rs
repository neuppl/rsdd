//! Primary binary decision diagram representation
use std::marker::PhantomData;

use crate::builder::var_order::VarOrder;

#[derive(Debug, Clone)]
pub struct BddPtr {
    idx: usize,
}

impl BddPtr {
    pub fn new(idx: usize) -> Self { 
        BddPtr { idx }
    }
}

#[derive(Debug, Clone)]
pub enum Bdd {
    True,
    False,
    Node { low: BddPtr, high: BddPtr }
}

pub struct FinalizedBDD {
    bdd: BddPtr,
    allocator: Vec<Bdd>,
    order: VarOrder
}

impl FinalizedBDD {
    pub fn new(bdd: BddPtr, allocator: Vec<Bdd>, order: VarOrder) -> Self {
        FinalizedBDD { bdd, allocator, order }
    }
}