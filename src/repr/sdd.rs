//! Defines the internal representations for a trimmed and compressed SDD with
//! complemented edges.

use crate::repr::{var_label::VarLabel, ddnnf::DDNNF};
use std::fmt::Debug;
use bumpalo::Bump;
use SddPtr::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Copy)]
pub enum SddPtr {
    PtrTrue,
    PtrFalse,
    Var(VarLabel, bool),
    Compl(*mut SddOr),
    Reg(*mut SddOr),
}


#[derive(Debug, Clone, Hash, PartialEq, Eq, Ord, PartialOrd, Copy)]
pub struct SddAnd {
    prime: SddPtr,
    sub: SddPtr
}

impl SddAnd {
    pub fn prime(&self) -> SddPtr {
        self.prime
    }
    pub fn sub(&self) -> SddPtr {
        self.sub
    }
    pub fn new(prime: SddPtr, sub: SddPtr) -> SddAnd {
        SddAnd { prime, sub }
    }
}

/// An SddOr node is a vector of (prime, sub) pairs.
#[derive(Debug, Clone, Eq, Ord, PartialOrd)]
pub struct SddOr {
    index: VTreeIndex,
    pub nodes: Vec<SddAnd>,
    pub scratch: usize,
}

impl SddOr {
    pub fn new(nodes: Vec<SddAnd>, index: VTreeIndex) -> SddOr {
        SddOr {
            nodes,
            index,
            scratch: 0,
        }
    }
}


impl PartialEq for SddOr {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index && self.nodes == other.nodes
    }
}

use std::{
    hash::{Hash, Hasher},
};

use super::{vtree::VTreeIndex, ddnnf::DDNNFPtr, var_label::Literal};
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
            PtrTrue => PtrFalse,
            PtrFalse => PtrTrue,
            Var(x, p) => Var(*x, !p),
            Compl(x) => Reg(*x),
            Reg(x) => Compl(*x),
        }
    }

    /// Gets the scratch value stored in `&self`
    /// 
    /// Panics if not node.
    pub fn get_scratch<T>(&self) -> Option<&T> {
        unsafe {
            let ptr = self.node_ref().scratch;
            if ptr == 0 {
                return None
            } else {
                return Some(&*(self.node_ref().scratch as *const T))
            }
        }
    }

    /// Set the scratch in this node to the value `v`. 
    /// 
    /// Panics if not a node.
    /// 
    /// Invariant: values stored in `set_scratch` must not outlive 
    /// the provided allocator `alloc` (i.e., calling `get_scratch` 
    /// involves dereferencing a pointer stored in `alloc`)
    pub fn set_scratch<T>(&self, alloc: &mut Bump, v: T) -> () {
        self.mut_node_ref().scratch = (alloc.alloc(v) as *const T) as usize;
    }

    /// recursively traverses the SDD and clears all scratch
    pub fn clear_scratch(&self) -> () {
        if self.is_const() || self.is_var() {
            return;
        }
        let n = self.node_ref_mut();
        if n.scratch == 0 {
            return;
        } else {
            n.scratch = 0;
            for a in &n.nodes {
                a.prime().clear_scratch();
                a.sub().clear_scratch();
            }
        }
    }

    /// true if the node is complemented
    pub fn is_compl(&self) -> bool {
        match &self {
            Compl(_) => true,
            _ => false
        }
    }


    pub fn is_or(&self) -> bool {
        match &self {
            Compl(_) | Reg(_) => true,
            _ => false
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

    /// uncomplement a pointer
    pub fn to_reg(&self) -> SddPtr {
        match &self {
            Compl(x) => Reg(*x),
            _ => *self
        }
    }

    pub fn is_const(&self) -> bool {
        match &self {
            Compl(_) => false,
            Reg(_) => false,
            Var(_, _) => false,
            PtrTrue => true,
            PtrFalse => true,
        }
    }

    pub fn is_var(&self) -> bool {
        match &self {
            Var(_, _) => true,
            _ => false
        }
    }

    pub fn is_neg_var(&self) -> bool {
        match &self {
            Var(_, false) => true,
            _ => false
        }
    }


    pub fn get_var(&self) -> Literal {
        match &self {
            Var(v, b) => Literal::new(*v, *b),
            _ => panic!("called get_var on non var")
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
            Compl(_) | Reg(_) | PtrTrue | Var(_, _) => false,
            PtrFalse => true,
        }
    }

    /// Get a mutable reference to the node that &self points to
    ///
    /// Panics if not a node pointer
    pub fn mut_node_ref(&self) -> &mut SddOr {
        unsafe {
            match &self {
                Reg(x) => &mut (**x),
                Compl(x) => &mut (**x),
                _ => panic!("Dereferencing constant in deref_or_panic"),
            }
        }
    }

    /// get an iterator to all the (prime, sub) pairs this node points to
    /// panics if not an or-node
    pub fn node_iter<'a>(&'a self) -> impl Iterator<Item = &'a SddAnd> {
        self.node_ref().nodes.iter()
    }

    /// returns number of (prime, sub) pairs this node points to
    /// panics if not an or-node
    pub fn num_nodes<'a>(&'a self) -> usize {
        self.node_ref().nodes.len()
    }

    /// Get an immutable reference to the node that &self points to
    ///
    /// Panics if not a node pointer
    pub fn node_ref(&self) -> &SddOr {
        unsafe {
            match &self {
                Reg(x) => &(**x),
                Compl(x) => &(**x),
                _ => panic!("Called node_ref on non-node {:?}", self),
            }
        }
    }

    /// Get a mutable reference to the node that &self points to
    ///
    /// Panics if not a node pointer
    pub fn node_ref_mut(&self) -> &mut SddOr {
        unsafe {
            match &self {
                Reg(x) => &mut (**x),
                Compl(x) => &mut (**x),
                _ => panic!("Called node_ref on non-node {:?}", self),
            }
        }
    }

    /// retrieve the vtree index (as its index in a left-first depth-first traversal)
    ///
    /// panics if this is not a node
    pub fn vtree(&self) -> VTreeIndex {
        self.node_ref().index
    }
}

type DDNNFCache<T> = (Option<T>, Option<T>);

impl DDNNFPtr for SddPtr {
    fn fold<T: Clone + Copy + std::fmt::Debug, F: Fn(super::ddnnf::DDNNF<T>) -> T>(&self, f: F) -> T {
        fn bottomup_pass_h<T: Clone + Copy + Debug, F: Fn(DDNNF<T>) -> T>(ptr: SddPtr, f: &F, alloc: &mut Bump) -> T {
            match ptr {
                PtrTrue => f(DDNNF::True),
                PtrFalse => f(DDNNF::False),
                Var(v, polarity) => f(DDNNF::Lit(v, polarity)),
                Compl(_) | Reg(_) => {
                    // inside the cache, store a (compl, non_compl) pair corresponding to the 
                    // complemented and uncomplemented pass over this node
                    if ptr.get_scratch::<DDNNFCache<T>>().is_none() {
                        ptr.set_scratch::<DDNNFCache<T>>(alloc, (None, None));
                    }
                    match ptr.get_scratch::<DDNNFCache<T>>() {
                        Some((Some(v), _)) if ptr.is_compl() => return v.clone(),
                        Some((_, Some(v))) if !ptr.is_compl() => return v.clone(),
                        Some((None, cached)) | Some((cached, None)) => {
                            // no cached value found, compute it
                            let mut or_v = f(DDNNF::False);
                            for and in ptr.node_iter() {
                                let s = if ptr.is_compl() { and.sub().neg() } else { and.sub() };
                                let p_sub = bottomup_pass_h(and.prime(), f, alloc);
                                let s_sub = bottomup_pass_h(s, f, alloc);
                                let a = f(DDNNF::And(p_sub, s_sub));
                                or_v = f(DDNNF::Or(or_v, a));
                            }

                            // cache and return or_v
                            if ptr.is_compl() { 
                                ptr.set_scratch::<DDNNFCache<T>>(alloc, (Some(or_v), *cached));
                            } else {
                                ptr.set_scratch::<DDNNFCache<T>>(alloc, (*cached, Some(or_v)));
                            }
                            return or_v
                        },
                        _ => panic!("unreachable")
                    }
                }
            }
        }

        let mut alloc = Bump::new();
        let r = bottomup_pass_h(*self, &f, &mut alloc);
        self.clear_scratch();
        r
    }

    fn count_nodes(&self) -> usize {
        todo!()
    }
}