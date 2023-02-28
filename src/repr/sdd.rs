//! Defines the internal representations for a trimmed and compressed SDD with
//! complemented edges.

use crate::repr::{
    ddnnf::DDNNF,
    var_label::{VarLabel, VarSet},
};
use bumpalo::Bump;
use std::collections::{HashMap, HashSet};
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
                Some(SddAnd::new(
                    SddPtr::var(self.sdd.topvar(), true),
                    self.sdd.high_raw(),
                ))
            } else if self.count == 1 {
                self.count += 1;
                Some(SddAnd::new(
                    SddPtr::var(self.sdd.topvar(), false),
                    self.sdd.low_raw(),
                ))
            } else {
                None
            }
        } else {
            let sdd = self.sdd.node_ref();
            if self.count >= sdd.nodes.len() {
                None
            } else {
                self.count += 1;
                Some(sdd.nodes[self.count - 1])
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
                None
            } else {
                Some(&*(ptr as *const T))
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
    pub fn set_scratch<T>(&self, alloc: &mut Bump, v: T) {
        if self.is_bdd() {
            self.mut_bdd_ref().scratch = (alloc.alloc(v) as *const T) as usize;
        } else {
            self.node_ref_mut().scratch = (alloc.alloc(v) as *const T) as usize;
        }
    }

    /// recursively traverses the SDD and clears all scratch
    pub fn clear_scratch(&self) {
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
        if n.scratch != 0 {
            n.scratch = 0;
            for a in &n.nodes {
                a.prime().clear_scratch();
                a.sub().clear_scratch();
            }
        }
    }

    pub fn is_or(&self) -> bool {
        matches!(self, Compl(_) | Reg(_))
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
        matches!(self, PtrTrue | PtrFalse)
    }

    pub fn is_var(&self) -> bool {
        matches!(self, Var(_, _))
    }

    pub fn is_neg_var(&self) -> bool {
        matches!(self, Var(_, false))
    }

    pub fn is_pos_var(&self) -> bool {
        matches!(self, Var(_, true))
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
        matches!(self, BDD(_) | ComplBDD(_))
    }

    /// Get a mutable reference to the node that &self points to
    ///
    /// Panics if not a bdd pointer
    #[allow(clippy::mut_from_ref)]
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
    pub fn num_nodes(&self) -> usize {
        if self.is_bdd() {
            2
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
    #[allow(clippy::mut_from_ref)]
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

    fn is_canonical_h(&self) -> bool {
        self.is_compressed() && self.is_trimmed()
    }

    pub fn is_canonical(&self) -> bool {
        self.is_canonical_h()
    }

    // predicate that returns if an SDD is compressed;
    // see https://www.ijcai.org/Proceedings/11/Papers/143.pdf
    // definition 8
    pub fn is_compressed(&self) -> bool {
        self.is_compressed_h()
    }

    fn is_compressed_h(&self) -> bool {
        match &self {
            PtrTrue => true,
            PtrFalse => true,
            Var(_, _) => true,
            BDD(_) | ComplBDD(_) => {
                let low = self.low();
                let high = self.high();

                (low != high) && low.is_compressed() && high.is_compressed()
            }
            Reg(_) | Compl(_) => {
                let mut visited_sdds: HashSet<SddPtr> = HashSet::new();
                for and in self.node_iter() {
                    if visited_sdds.contains(&and.sub) {
                        return false;
                    }
                    visited_sdds.insert(and.sub);
                }

                self.node_iter().all(|and| and.prime.is_compressed())
            }
        }
    }

    pub fn is_trimmed(&self) -> bool {
        self.is_trimmed_h()
    }

    fn is_trimmed_h(&self) -> bool {
        match &self {
            PtrTrue => true,
            PtrFalse => true,
            Var(_, _) => true,
            BDD(_) => {
                // core assumption: in binary SDD, the prime is always x and not x
                // so, we only check low/high being flipped versions
                if !self.low().is_const() || !self.high().is_const() {
                    return self.low().is_trimmed() && self.high().is_trimmed();
                }

                // both low and high are constants; need to check for (a,T) and (~a, F) case
                self.low() != self.high()
            }
            ComplBDD(_) => self.neg().is_trimmed(),
            Reg(_) | Compl(_) => {
                // TODO(mattxwang): significantly optimize this
                // this next part is an O(n^2) (i.e., pairwise) comparison of each SDD
                // and an arbitrary prime. we are looking for untrimmed decomposition pairs of the form (a, T) and (~a, F)
                let mut visited_primes: HashSet<SddPtr> = HashSet::new();

                for and in self.node_iter() {
                    let prime = and.prime;

                    // decomposition of the form (T, a)
                    if prime.is_true() {
                        return false;
                    }

                    if !and.sub.is_const() {
                        continue;
                    }

                    // have seen (a, T) and (~a, F)
                    if visited_primes.contains(&prime) {
                        return false;
                    }

                    // add (~a, _) to seen nodes
                    visited_primes.insert(prime.neg());
                }

                self.node_iter().all(|s| s.prime.is_trimmed())
            }
        }
    }
    // heavy lifting for getting the semantic hash of SDD; assumes current is the root
    // this function doesn't allocate anything!
    // for more info, see https://tr.inf.unibe.ch/pdf/iam-06-001.pdf
    pub fn get_semantic_hash(&self, map: &HashMap<usize, u128>, prime: u128) -> u128 {
        let negate_hash = |val: u128| -> u128 { (prime - val + 1) % prime };

        let get_var_weight = |v: &VarLabel, polarity: bool| -> u128 {
            match map.get(&v.value_usize()) {
                None => panic!("error - variable weight not defined in map"),
                Some(&val) => {
                    if polarity {
                        return val;
                    }
                    negate_hash(val)
                }
            }
        };

        match &self {
            PtrTrue => 1,
            PtrFalse => 0,
            Var(v, polarity) => get_var_weight(v, *polarity),
            BDD(_) => {
                let label_weight = get_var_weight(&self.topvar(), true);

                let low_weight = self.low().get_semantic_hash(map, prime);
                let high_weight = self.high().get_semantic_hash(map, prime);

                (negate_hash(label_weight) * low_weight + label_weight * high_weight) % prime
            }
            Reg(_) => {
                let raw_hash: u128 = self
                    .node_iter()
                    .map(|and| {
                        and.prime().get_semantic_hash(map, prime)
                            * and.sub().get_semantic_hash(map, prime)
                    })
                    .sum();
                raw_hash % prime
            }

            ComplBDD(_) | Compl(_) => negate_hash(self.to_reg().get_semantic_hash(map, prime)),
        }
    }
}

type DDNNFCache<T> = (Option<T>, Option<T>);

impl DDNNFPtr for SddPtr {
    type Order = VTreeManager;

    // TODO: we should be able to remove this; e.g. replace v.clone() with *v
    #[allow(clippy::clone_on_copy)]
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
                        Some((Some(v), _)) if ptr.is_neg() => v.clone(),
                        Some((_, Some(v))) if !ptr.is_neg() => v.clone(),
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
                            or_v
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
            }
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
                    c
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
        matches!(self, Compl(_) | ComplBDD(_))
    }

    fn is_true(&self) -> bool {
        matches!(self, PtrTrue)
    }

    fn is_false(&self) -> bool {
        matches!(self, PtrFalse)
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

#[test]
fn is_compressed_trivial() {
    assert!(PtrTrue.is_compressed());
    assert!(PtrFalse.is_compressed());
    assert!(Var(VarLabel::new(0), true).is_compressed());
    assert!(Var(VarLabel::new(1), false).is_compressed());
}

#[test]
fn is_compressed_simple_bdd() {
    let vtree = crate::repr::vtree::VTree::even_split(
        &[VarLabel::new(0), VarLabel::new(1), VarLabel::new(2)],
        1,
    );
    let vtree_manager = VTreeManager::new(vtree);
    let a = SddPtr::var(VarLabel::new(0), true);
    let b = SddPtr::var(VarLabel::new(1), false);
    let mut binary_sdd = BinarySDD::new(
        VarLabel::new(2),
        a,
        b,
        vtree_manager.get_varlabel_idx(VarLabel::new(2)),
    );
    let binary_sdd_ptr = &mut binary_sdd;
    let bdd_ptr = SddPtr::bdd(binary_sdd_ptr);
    assert_ne!(a, b);
    assert!(bdd_ptr.is_compressed());
}

#[test]
fn is_compressed_simple_bdd_duplicate() {
    let vtree = crate::repr::vtree::VTree::even_split(
        &[VarLabel::new(0), VarLabel::new(1), VarLabel::new(2)],
        1,
    );
    let vtree_manager = VTreeManager::new(vtree);
    let a = SddPtr::var(VarLabel::new(0), true);
    let mut binary_sdd = BinarySDD::new(
        VarLabel::new(2),
        a,
        a, // duplicate with low - not compressed!
        vtree_manager.get_varlabel_idx(VarLabel::new(2)),
    );
    let binary_sdd_ptr = &mut binary_sdd;
    let bdd_ptr = SddPtr::bdd(binary_sdd_ptr);

    assert!(!bdd_ptr.is_compressed())
}

#[test]
fn is_trimmed_trivial() {
    assert!(PtrTrue.is_trimmed());
    assert!(PtrFalse.is_trimmed());
    assert!(Var(VarLabel::new(0), true).is_trimmed());
    assert!(Var(VarLabel::new(1), false).is_trimmed());
}

#[test]
fn is_trimmed_simple_demorgan() {
    let mut man =
        crate::builder::sdd_builder::SddManager::new(crate::repr::vtree::VTree::even_split(
            &[
                VarLabel::new(0),
                VarLabel::new(1),
                VarLabel::new(2),
                VarLabel::new(3),
                VarLabel::new(4),
            ],
            1,
        ));

    let x = SddPtr::var(VarLabel::new(0), true);
    let y = SddPtr::var(VarLabel::new(3), true);
    let res = man.or(x, y).neg();
    let expected = man.and(x.neg(), y.neg());

    assert!(expected.is_trimmed());
    assert!(res.is_trimmed());
}
#[test]
fn is_canonical_trivial() {
    assert!(PtrTrue.is_canonical());
    assert!(PtrFalse.is_canonical());
    assert!(Var(VarLabel::new(0), true).is_canonical());
    assert!(Var(VarLabel::new(1), false).is_canonical());
}

#[test]
fn is_canonical_simple_demorgan() {
    let mut man =
        crate::builder::sdd_builder::SddManager::new(crate::repr::vtree::VTree::even_split(
            &[
                VarLabel::new(0),
                VarLabel::new(1),
                VarLabel::new(2),
                VarLabel::new(3),
                VarLabel::new(4),
            ],
            1,
        ));
    let x = SddPtr::var(VarLabel::new(0), true);
    let y = SddPtr::var(VarLabel::new(3), true);
    let res = man.or(x, y).neg();
    let expected = man.and(x.neg(), y.neg());
    assert!(expected.is_canonical());
    assert!(res.is_canonical());
}
