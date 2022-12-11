//! Defines the internal representations for a trimmed and compressed SDD with
//! complemented edges.

use crate::repr::var_label::VarLabel;
use crate::repr::vtree::VTree;
use crate::util::btree::*;
use std::mem;
use num::Num;
use tinyvec::TinyVec;

/// An SddPtr is either (1) a BDD pointer, or (2) a pointer to an SDD node.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Copy)]
pub enum SddPtr {
    PtrTrue,
    PtrFalse,
    Var(VarLabel, bool),
    Compl(*mut SddOr),
    Reg(*mut SddOr)
}

use SddPtr::*;

/// An SddOr node is a vector of (prime, sub) pairs.
#[derive(Debug, Clone, Eq, Ord, PartialOrd)]
pub struct SddOr {
    index: VTreeIndex,
    pub nodes: Vec<(SddPtr, SddPtr)>,
    pub scratch: Option<usize>
}

impl SddOr {
    pub fn new(nodes: Vec<(SddPtr, SddPtr)>, index: VTreeIndex) -> SddOr {
        SddOr { nodes, index, scratch: None}
    }
}

impl PartialEq for SddOr {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index && self.nodes == other.nodes
    }
}

use std::{hash::{Hash, Hasher}, collections::HashMap};

use super::{wmc::WmcParams, vtree::VTreeIndex};
impl Hash for SddOr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state);
        self.nodes.hash(state);
    }
}

impl SddPtr {
    pub fn or(ptr: *mut SddOr) -> SddPtr {
        Reg(ptr)
    }

    /// negate an SDD pointer
    pub fn neg(&self) -> SddPtr {
        match &self {
            TruePtr => PtrFalse,
            FalsePtr => PtrTrue,
            Var(x, p) => Var(*x, !p),
            Compl(x) => Reg(*x),
            Reg(x) => Compl(*x)
        }
    }

    /// true if the node is complemented
    pub fn is_compl(&self) -> bool {
        match &self {
            PtrTrue | Reg(_) => false, 
            Var(_, _) => false,
            PtrFalse | Compl(_) => true
        }
    }

    pub fn false_ptr() -> SddPtr {
        PtrFalse
    }

    pub fn true_ptr() -> SddPtr {
        PtrTrue
    }

    pub fn var(lbl: VarLabel, polarity: bool) -> SddPtr {
        Var(lbl, polarity)
    }

    /// convert a BddPtr into a regular (non-complemented) pointer
    pub fn to_reg(&self) -> SddPtr {
        match &self {
            Compl(x) => Reg(*x),
            Reg(x) => Reg(*x),
            Var(x, p) => Var(*x, true),
            PtrTrue => PtrTrue,
            PtrFalse => PtrTrue
        }
    }

    pub fn is_const(&self) -> bool {
        match &self {
            Compl(x) => false,
            Reg(x) => false,
            Var(_, _) => false,
            PtrTrue => true,
            PtrFalse => true 
        }
    }

     pub fn is_true(&self) -> bool {
        match &self {
            Compl(_) | Reg(_) | PtrFalse | Var(_, _) => false,
            PtrTrue => true,
        }
    }

    pub fn is_false(&self) -> bool {
        match &self {
            Compl(_) | Reg(_) | PtrTrue | Var(_,_) => false,
            PtrFalse => true,
        }
    }

    /// Get a mutable reference to the node that &self points to
    /// 
    /// Panics if not a node pointer
    pub fn mut_node_ref(&mut self) -> &mut SddOr {
        unsafe {
            match &self {
                Reg(x) => {
                    &mut (**x)
                },
                Compl(x) => {
                    &mut (**x)
                },
                _ => panic!("Dereferencing constant in deref_or_panic")
            }
        }       
    }

    /// Get an immutable reference to the node that &self points to
    /// 
    /// Panics if not a node pointer
    pub fn node_ref(&self) -> &SddOr {
        unsafe {
            match &self {
                Reg(x) => {
                    &(**x)
                },
                Compl(x) => {
                    &(**x)
                },
                _ => panic!("Dereferencing constant in deref_or_panic")
            }
        }       
    }

    /// Get a reference to the slice &[(prime, sub)] that &self points to
    /// 
    /// Panics if not a node pointer
    pub fn or_ref(&self) -> &[(SddPtr, SddPtr)] {
        &self.node_ref().nodes
    }

    // // Walks the Sdd, caching results of previously computed values
    // fn unsmoothed_wmc_h<T: Num + Clone + Debug + Copy>(
    //     &mut self,
    //     ptr: SddPtr,
    //     weights: &SddWmc<T>,
    //     tbl: &mut HashMap<SddPtr, T>,
    // ) -> T {
    //     match tbl.get(&ptr.regular()) {
    //         Some(v) => *v,
    //         None => {
    //             if ptr.is_false() {
    //                 return weights.zero;
    //             }
    //             if ptr.is_true() {
    //                 return weights.one;
    //             }
    //             if ptr.is_bdd() {
    //                 let mgr = self.get_bdd_mgr_mut(ptr);
    //                 let bdd_ptr = ptr.as_bdd_ptr();
    //                 let pot_wmc = &weights.wmc_structs[ptr.vtree().value()];
    //                 let bdd_wmc = match pot_wmc {
    //                     WmcStruct::Bdd(wmc) => wmc,
    //                     WmcStruct::Dummy(_) => panic!("Oh the humanity!"),
    //                 };
    //                 let wmc_val = mgr.wmc(bdd_ptr, bdd_wmc);
    //                 tbl.insert(ptr.regular(), wmc_val);
    //                 return wmc_val;
    //             }
    //             // TODO remove this allocation
    //             let or = Vec::from(self.tbl.sdd_get_or(ptr));
    //             or.iter().fold(weights.zero, |acc, (p, s)| {
    //                 let s = if ptr.is_compl() { s.neg() } else { *s };
    //                 acc + (self.unsmoothed_wmc_h(*p, weights, tbl)
    //                     * self.unsmoothed_wmc_h(s, weights, tbl))
    //             })
    //         }
    //     }
    // }

    pub fn unsmoothed_wmc<T: Num + Clone + std::fmt::Debug + Copy>(
        &mut self,
        ptr: SddPtr,
        weights: &WmcParams<T>,
    ) -> T {
        panic!("not implementd")
        // self.unsmoothed_wmc_h(ptr, weights, &mut HashMap::new())
    }
   
    /// retrieve the vtree index (as its index in a left-first depth-first traversal)
    /// 
    /// panics if this is a constant
    pub fn vtree(&self) -> VTreeIndex {
        self.node_ref().index
    }
}