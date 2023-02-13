//! Defines the internal representations for a trimmed and compressed SDD with
//! complemented edges.

use crate::repr::{
    ddnnf::DDNNF,
    var_label::{VarLabel, VarSet},
};
use bumpalo::Bump;
use std::fmt::Debug;
use SddPtr::*;

// This type is used a lot. Make sure it doesn't unintentionally get bigger.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Copy)]
pub enum SddPtr {
    PtrTrue,
    PtrFalse,
    BDD(*mut BinarySDD),
    ComplBDD(*mut BinarySDD),
    Var(VarLabel, bool),
    Compl(*mut SddOr),
    Reg(*mut SddOr),
}

/// Specialized SDD node for a right-linear sub-vtree
/// SDDs for these fragments are binary decisions
#[derive(Debug, Clone, Eq, Ord, PartialOrd, Copy)]
pub struct BinarySDD {
    label: VarLabel,
    vtree: VTreeIndex,
    low: SddPtr,
    high: SddPtr,
    scratch: usize,
}

impl BinarySDD {
    pub fn new(label: VarLabel, low: SddPtr, high: SddPtr, vtree: VTreeIndex) -> BinarySDD {
        BinarySDD {
            label,
            low,
            high,
            vtree,
            scratch: 0,
        }
    }

    pub fn vtree(&self) -> VTreeIndex {
        self.vtree
    }

    pub fn low(&self) -> SddPtr {
        self.low
    }

    pub fn high(&self) -> SddPtr {
        self.high
    }

    pub fn label(&self) -> VarLabel {
        self.label
    }
}

impl Hash for BinarySDD {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.vtree.hash(state);
        self.label.hash(state);
        self.low.hash(state);
        self.high.hash(state);
    }
}

impl PartialEq for BinarySDD {
    fn eq(&self, other: &Self) -> bool {
        self.vtree == other.vtree
            && self.low == other.low
            && self.high == other.high
            && self.label == other.label
    }
}

/// Produces a node iterator for SDD or-nodes from an SDD pointer
struct SddNodeIter {
    sdd: SddPtr,
    count: usize,
}

impl SddNodeIter {
    pub fn new(sdd: SddPtr) -> SddNodeIter {
        // println!("new iter, len: {}", if sdd.is_bdd() { 2 } else { sdd.num_nodes() });
        SddNodeIter { sdd, count: 0 }
    }
}

impl Iterator for SddNodeIter {
    type Item = SddAnd;

    fn next(&mut self) -> Option<Self::Item> {
        if self.sdd.is_bdd() {
            // if this is a binary SDD, produce the appropriate nodes
            if self.count == 0 {
                self.count += 1;
                return Some(SddAnd::new(
                    SddPtr::var(self.sdd.topvar(), true),
                    self.sdd.high_raw(),
                ));
            } else if self.count == 1 {
                self.count += 1;
                return Some(SddAnd::new(
                    SddPtr::var(self.sdd.topvar(), false),
                    self.sdd.low_raw(),
                ));
            } else {
                return None;
            }
        } else {
            let sdd = self.sdd.node_ref();
            if self.count >= sdd.nodes.len() {
                return None;
            } else {
                self.count += 1;
                return Some(sdd.nodes[self.count - 1]);
            }
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Ord, PartialOrd, Copy)]
pub struct SddAnd {
    prime: SddPtr,
    sub: SddPtr,
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

use std::hash::{Hash, Hasher};

use super::{
    ddnnf::DDNNFPtr,
    var_label::Literal,
    vtree::{VTreeIndex, VTreeManager},
};
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

    pub fn bdd(ptr: *mut BinarySDD) -> SddPtr {
        Self::BDD(ptr)
    }

    /// Gets the scratch value stored in `&self`
    ///
    /// Panics if not node.
    pub fn get_scratch<T>(&self) -> Option<&T> {
        unsafe {
            let ptr = if self.is_bdd() {
                self.mut_bdd_ref().scratch
            } else {
                self.node_ref().scratch
            };
            if ptr == 0 {
                return None;
            } else {
                return Some(&*(ptr as *const T));
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
        if self.is_bdd() {
            self.mut_bdd_ref().scratch = (alloc.alloc(v) as *const T) as usize;
        } else {
            self.node_ref_mut().scratch = (alloc.alloc(v) as *const T) as usize;
        }
    }

    /// recursively traverses the SDD and clears all scratch
    pub fn clear_scratch(&self) -> () {
        if self.is_const() || self.is_var() {
            return;
        }
        if self.is_bdd() {
            if self.mut_bdd_ref().scratch == 0 {
                return;
            } else {
                // clear children and return
                self.mut_bdd_ref().scratch = 0;
                self.high().clear_scratch();
                self.low().clear_scratch();
                return;
            }
        }

        // node is an sdd
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

    pub fn is_or(&self) -> bool {
        match &self {
            Compl(_) | Reg(_) => true,
            _ => false,
        }
    }

    pub fn var(lbl: VarLabel, polarity: bool) -> SddPtr {
        Var(lbl, polarity)
    }

    /// uncomplement a pointer
    pub fn to_reg(&self) -> SddPtr {
        match &self {
            Compl(x) => Reg(*x),
            ComplBDD(x) => BDD(*x),
            _ => *self,
        }
    }

    pub fn is_const(&self) -> bool {
        match &self {
            PtrTrue => true,
            PtrFalse => true,
            _ => false,
        }
    }

    pub fn is_var(&self) -> bool {
        match &self {
            Var(_, _) => true,
            _ => false,
        }
    }

    pub fn is_neg_var(&self) -> bool {
        match &self {
            Var(_, false) => true,
            _ => false,
        }
    }

    pub fn is_pos_var(&self) -> bool {
        match &self {
            Var(_, true) => true,
            _ => false,
        }
    }

    pub fn get_var_label(&self) -> VarLabel {
        match &self {
            Var(v, _b) => *v,
            _ => panic!("called get_var on non var"),
        }
    }

    pub fn get_var(&self) -> Literal {
        match &self {
            Var(v, b) => Literal::new(*v, *b),
            _ => panic!("called get_var on non var"),
        }
    }

    pub fn is_bdd(&self) -> bool {
        match &self {
            BDD(_) => true,
            ComplBDD(_) => true,
            _ => false,
        }
    }

    /// Get a mutable reference to the node that &self points to
    ///
    /// Panics if not a bdd pointer
    pub fn mut_bdd_ref(&self) -> &mut BinarySDD {
        unsafe {
            match &self {
                BDD(x) => &mut (**x),
                ComplBDD(x) => &mut (**x),
                _ => panic!("Dereferencing constant in deref_or_panic"),
            }
        }
    }

    /// gets the top variable of a BDD
    ///
    /// panics if not a bdd pointer
    pub fn topvar(&self) -> VarLabel {
        self.mut_bdd_ref().label
    }

    /// gets the low pointer of a BDD
    /// negates the returned pointer if the root is negated
    ///
    /// panics if not a bdd pointer
    pub fn low(&self) -> SddPtr {
        if self.is_neg() {
            self.mut_bdd_ref().low.neg()
        } else {
            self.mut_bdd_ref().low
        }
    }

    /// gets the low pointer of a BDD
    ///
    /// panics if not a bdd pointer
    pub fn low_raw(&self) -> SddPtr {
        self.mut_bdd_ref().low
    }

    /// gets the high pointer of a BDD
    /// negates the returned pointer if the root is negated
    ///
    /// panics if not a bdd pointer
    pub fn high(&self) -> SddPtr {
        if self.is_neg() {
            self.mut_bdd_ref().high.neg()
        } else {
            self.mut_bdd_ref().high
        }
    }

    /// gets the high pointer of a BDD
    ///
    /// panics if not a bdd pointer
    pub fn high_raw(&self) -> SddPtr {
        self.mut_bdd_ref().high
    }

    /// get an iterator to all the (prime, sub) pairs this node points to
    /// panics if not an or-node
    pub fn node_iter(&self) -> impl Iterator<Item = SddAnd> {
        SddNodeIter::new(*self)
    }

    /// returns number of (prime, sub) pairs this node points to
    /// panics if not an or-node
    pub fn num_nodes<'a>(&'a self) -> usize {
        if self.is_bdd() {
            return 2;
        } else {
            self.node_ref().nodes.len()
        }
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
        if self.is_bdd() {
            self.mut_bdd_ref().vtree()
        } else {
            self.node_ref().index
        }
    }
}

type DDNNFCache<T> = (Option<T>, Option<T>);

impl DDNNFPtr for SddPtr {
    type Order = VTreeManager;

    fn fold<T: Clone + Copy + std::fmt::Debug, F: Fn(super::ddnnf::DDNNF<T>) -> T>(
        &self,
        _v: &VTreeManager,
        f: F,
    ) -> T {
        fn bottomup_pass_h<T: Clone + Copy + Debug, F: Fn(DDNNF<T>) -> T>(
            ptr: SddPtr,
            f: &F,
            alloc: &mut Bump,
        ) -> T {
            match ptr {
                PtrTrue => f(DDNNF::True),
                PtrFalse => f(DDNNF::False),
                Var(v, polarity) => f(DDNNF::Lit(v, polarity)),
                Compl(_) | Reg(_) | ComplBDD(_) | BDD(_) => {
                    // inside the cache, store a (compl, non_compl) pair corresponding to the
                    // complemented and uncomplemented pass over this node
                    if ptr.get_scratch::<DDNNFCache<T>>().is_none() {
                        ptr.set_scratch::<DDNNFCache<T>>(alloc, (None, None));
                    }
                    match ptr.get_scratch::<DDNNFCache<T>>() {
                        Some((Some(v), _)) if ptr.is_neg() => return v.clone(),
                        Some((_, Some(v))) if !ptr.is_neg() => return v.clone(),
                        Some((None, cached)) | Some((cached, None)) => {
                            // no cached value found, compute it
                            let mut or_v = f(DDNNF::False);
                            for and in ptr.node_iter() {
                                let s = if ptr.is_neg() {
                                    and.sub().neg()
                                } else {
                                    and.sub()
                                };
                                let p_sub = bottomup_pass_h(and.prime(), f, alloc);
                                let s_sub = bottomup_pass_h(s, f, alloc);
                                let a = f(DDNNF::And(p_sub, s_sub));
                                let v = VarSet::new();
                                or_v = f(DDNNF::Or(or_v, a, v));
                            }

                            // cache and return or_v
                            if ptr.is_neg() {
                                ptr.set_scratch::<DDNNFCache<T>>(alloc, (Some(or_v), *cached));
                            } else {
                                ptr.set_scratch::<DDNNFCache<T>>(alloc, (*cached, Some(or_v)));
                            }
                            return or_v;
                        }
                        _ => panic!("unreachable"),
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
        fn count_h(ptr: SddPtr, alloc: &mut Bump) -> usize {
            if ptr.is_const() || ptr.is_var() {
                return 0;
            } else {
                match ptr.get_scratch::<usize>() {
                    Some(_) => 0,
                    None => {
                        // found a new node
                        ptr.set_scratch::<usize>(alloc, 0);
                        let mut c = 0;
                        for a in ptr.node_iter() {
                            c += count_h(a.sub(), alloc);
                            c += count_h(a.prime(), alloc);
                            c += 1;
                        }
                        return c;
                    }
                }
            }
        }
        count_h(*self, &mut Bump::new())
    }

    fn false_ptr() -> SddPtr {
        PtrFalse
    }

    fn true_ptr() -> SddPtr {
        PtrTrue
    }

    /// true if the node is complemented
    fn is_neg(&self) -> bool {
        match &self {
            Compl(_) => true,
            ComplBDD(_) => true,
            _ => false,
        }
    }

    fn is_true(&self) -> bool {
        match &self {
            PtrTrue => true,
            _ => false,
        }
    }

    fn is_false(&self) -> bool {
        match &self {
            PtrFalse => true,
            _ => false,
        }
    }

    fn neg(&self) -> Self {
        match &self {
            PtrTrue => PtrFalse,
            PtrFalse => PtrTrue,
            Var(x, p) => Var(*x, !p),
            Compl(x) => Reg(*x),
            Reg(x) => Compl(*x),
            BDD(x) => ComplBDD(*x),
            ComplBDD(x) => BDD(*x),
        }
    }
}
