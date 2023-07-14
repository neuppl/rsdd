use crate::repr::{bdd::BddPtr, var_label::VarLabel};
use std::cmp::Ordering;

mod builder;
mod robdd;
mod stats;

pub use self::builder::*;
pub use self::robdd::*;
pub use self::stats::*;

// TODO: move this to a compile module

#[derive(Eq, PartialEq, Debug)]
struct CompiledCNF<'a> {
    ptr: BddPtr<'a>,
    sz: usize,
}

// The priority queue depends on `Ord`.
// Explicitly implement the trait so the queue becomes a min-heap
// instead of a max-heap.
impl<'a> Ord for CompiledCNF<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        // Notice that the we flip the ordering on costs.
        // In case of a tie we compare positions - this step is necessary
        // to make implementations of `PartialEq` and `Ord` consistent.
        other.sz.cmp(&self.sz)
    }
}

// `PartialOrd` needs to be implemented as well.
impl<'a> PartialOrd for CompiledCNF<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
pub struct Assignment {
    assignments: Vec<bool>,
}

impl Assignment {
    pub fn new(assignments: Vec<bool>) -> Assignment {
        Assignment { assignments }
    }

    pub fn get_assignment(&self, var: VarLabel) -> bool {
        self.assignments[var.value() as usize]
    }
}
