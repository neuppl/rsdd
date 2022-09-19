//! Public-facing binary decision diagram representation
use crate::builder::var_order::VarOrder;

use super::var_label::VarLabel;

// TODO: resolve unused
#[allow(unused)]
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
    Node {
        var: VarLabel,
        low: BddPtr,
        high: BddPtr,
    },
}

// TODO: resolve unused
#[allow(unused)]
pub struct FinalizedBDD {
    bdd: BddPtr,
    allocator: Vec<Bdd>,
    order: VarOrder,
}

impl FinalizedBDD {
    pub fn new(bdd: BddPtr, allocator: Vec<Bdd>, order: VarOrder) -> Self {
        FinalizedBDD {
            bdd,
            allocator,
            order,
        }
    }
}
