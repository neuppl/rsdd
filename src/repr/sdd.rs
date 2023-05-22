//! Defines the internal representations for a trimmed and compressed SDD with
//! complemented edges.

pub mod binary_sdd;
pub mod sdd_or;

use crate::{
    repr::{
        ddnnf::DDNNF,
        var_label::{VarLabel, VarSet},
    },
    util::semiring::FiniteField,
};
use bumpalo::Bump;
use std::collections::HashSet;
use std::fmt::Debug;
use SddPtr::*;

use std::hash::Hash;

use self::binary_sdd::BinarySDD;
use self::sdd_or::{SddAnd, SddNodeIter, SddOr};

// This type is used a lot. Make sure it doesn't unintentionally get bigger.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Copy)]
pub enum SddPtr<'a> {
    PtrTrue,
    PtrFalse,
    BDD(&'a BinarySDD<'a>),
    ComplBDD(&'a BinarySDD<'a>),
    Var(VarLabel, bool),
    Compl(&'a SddOr<'a>),
    Reg(&'a SddOr<'a>),
}

use super::{
    ddnnf::DDNNFPtr,
    robdd::WmcParams,
    var_label::Literal,
    vtree::{VTreeIndex, VTreeManager},
};

impl<'a> SddPtr<'a> {
    pub fn or(ptr: &'a SddOr) -> SddPtr<'a> {
        Reg(ptr)
    }

    pub fn bdd(ptr: &'a BinarySDD) -> SddPtr<'a> {
        Self::BDD(ptr)
    }

    /// performs a semantic hash and caches the result on the node
    pub fn cached_semantic_hash<const P: u128>(
        &self,
        vtree: &VTreeManager,
        map: &WmcParams<FiniteField<P>>,
    ) -> FiniteField<P> {
        match self {
            PtrTrue => FiniteField::new(1),
            PtrFalse => FiniteField::new(0),
            Var(label, polarity) => {
                let (l_w, h_w) = map.get_var_weight(*label);
                return if *polarity { *h_w } else { *l_w };
            }
            BDD(bdd) => bdd.cached_semantic_hash(vtree, map),
            Reg(or) => or.cached_semantic_hash(vtree, map),
            ComplBDD(_) | Compl(_) => self.neg().cached_semantic_hash(vtree, map).negate(),
        }
    }

    /// Gets the scratch value stored in `&self`
    pub fn get_scratch<T: ?Sized + Clone + 'static>(&self) -> Option<T> {
        match self {
            PtrTrue | PtrFalse | Var(_, _) => None,
            BDD(bdd) | ComplBDD(bdd) => bdd.get_scratch(),
            Reg(or) | Compl(or) => or.get_scratch(),
        }
    }

    /// Set the scratch in this node to the value `v`.
    ///
    /// Panics if not a node.
    ///
    /// Invariant: values stored in `set_scratch` must not outlive
    /// the provided allocator `alloc` (i.e., calling `get_scratch`
    /// involves dereferencing a pointer stored in `alloc`)
    pub fn set_scratch<T: 'static>(&self, v: T) {
        match self {
            PtrTrue | PtrFalse | Var(_, _) => panic!("attempting to store scratch on constant"),
            BDD(bdd) | ComplBDD(bdd) => bdd.set_scratch(v),
            Reg(or) | Compl(or) => or.set_scratch(v),
        };
    }

    pub fn is_scratch_cleared(&self) -> bool {
        match self {
            PtrTrue | PtrFalse | Var(_, _) => true,
            BDD(bdd) | ComplBDD(bdd) => bdd.is_scratch_cleared(),
            Reg(or) | Compl(or) => or.scratch.borrow().is_none(),
        }
    }

    /// recursively traverses the SDD and clears all scratch
    pub fn clear_scratch(&self) {
        match self {
            PtrTrue | PtrFalse | Var(_, _) => return,
            BDD(bdd) | ComplBDD(bdd) => bdd.clear_scratch(),
            Reg(or) | Compl(or) => or.clear_scratch(),
        }
    }

    pub fn is_or(&self) -> bool {
        matches!(self, Compl(_) | Reg(_))
    }

    pub fn var(lbl: VarLabel, polarity: bool) -> SddPtr<'a> {
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

    pub fn is_node(&self) -> bool {
        matches!(self, Compl(_) | Reg(_))
    }

    /// get a reference to a binary SDD node
    ///
    /// panics if not a binary SDD
    pub fn bdd_ref(&self) -> &'a BinarySDD<'a> {
        match self {
            BDD(n) => n,
            ComplBDD(n) => n,
            _ => panic!(),
        }
    }

    /// gets the top variable of a BDD
    ///
    /// panics if not a bdd pointer
    pub fn topvar(&self) -> VarLabel {
        self.bdd_ref().label()
    }

    /// gets the low pointer of a BDD
    /// negates the returned pointer if the root is negated
    ///
    /// panics if not a bdd pointer
    pub fn low(&self) -> SddPtr<'a> {
        if self.is_neg() {
            self.bdd_ref().low().neg()
        } else {
            self.bdd_ref().low()
        }
    }

    /// gets the low pointer of a BDD
    ///
    /// panics if not a bdd pointer
    pub fn low_raw(&self) -> SddPtr<'a> {
        self.bdd_ref().low()
    }

    /// gets the high pointer of a BDD
    /// negates the returned pointer if the root is negated
    ///
    /// panics if not a bdd pointer
    pub fn high(&self) -> SddPtr<'a> {
        if self.is_neg() {
            self.bdd_ref().high().neg()
        } else {
            self.bdd_ref().high()
        }
    }

    /// gets the high pointer of a BDD
    ///
    /// panics if not a bdd pointer
    pub fn high_raw(&self) -> SddPtr<'a> {
        self.bdd_ref().high()
    }

    /// get an iterator to all the (prime, sub) pairs this node points to
    /// panics if not an or-node
    pub fn node_iter(&self) -> impl Iterator<Item = SddAnd<'a>> {
        SddNodeIter::new(*self)
    }

    /// returns number of (prime, sub) pairs this node points to
    /// panics if not an or-node or const
    pub fn num_nodes(&self) -> usize {
        if self.is_const() {
            return 1;
        }
        if self.is_bdd() {
            2
        } else {
            self.node_ref().nodes.len()
        }
    }

    /// gets the total number of nodes that are a child to this SDD
    pub fn num_child_nodes(&self) -> usize {
        match &self {
            PtrTrue | PtrFalse | Var(_, _) => 1,
            BDD(_) | ComplBDD(_) => {
                1 + self.low().num_child_nodes() + self.high().num_child_nodes()
            }
            Compl(_) | Reg(_) => {
                1 + self
                    .node_ref()
                    .nodes
                    .iter()
                    .map(|n| return 1 + n.prime.num_child_nodes() + n.sub.num_child_nodes())
                    .sum::<usize>()
            }
        }
    }

    /// Get an immutable reference to the node that &self points to
    ///
    /// Panics if not a node pointer
    pub fn node_ref(&self) -> &SddOr {
        match &self {
            Reg(x) => x,
            Compl(x) => x,
            _ => panic!("Called node_ref on non-node {:?}", self),
        }
    }

    /// retrieve the vtree index (as its index in a left-first depth-first traversal)
    ///
    /// panics if this is not a node
    pub fn vtree(&self) -> VTreeIndex {
        if self.is_bdd() {
            self.bdd_ref().vtree()
        } else {
            self.node_ref().index()
        }
    }

    pub fn is_canonical(&self) -> bool {
        self.is_compressed() && self.is_trimmed()
    }

    // predicate that returns if an SDD is compressed;
    // see https://www.ijcai.org/Proceedings/11/Papers/143.pdf
    // definition 8
    pub fn is_compressed(&self) -> bool {
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
}

type DDNNFCache<T> = (Option<T>, Option<T>);

impl<'a> DDNNFPtr<'a> for SddPtr<'a> {
    type Order = VTreeManager;

    fn fold<T: 'static + Clone + Copy + std::fmt::Debug, F: Fn(super::ddnnf::DDNNF<T>) -> T>(
        &self,
        _v: &VTreeManager,
        f: F,
    ) -> T {
        debug_assert!(self.is_scratch_cleared());
        fn bottomup_pass_h<T: 'static + Clone + Copy + Debug, F: Fn(DDNNF<T>) -> T>(
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

                    // helper performs actual fold-and-cache work
                    let mut bottomup_helper = |cached| {
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
                            ptr.set_scratch::<DDNNFCache<T>>((Some(or_v), cached));
                        } else {
                            ptr.set_scratch::<DDNNFCache<T>>((cached, Some(or_v)));
                        }
                        or_v
                    };

                    match ptr.get_scratch::<DDNNFCache<T>>() {
                        // first, check if cached; explicit arms here for clarity
                        Some((Some(l), Some(h))) => {
                            if ptr.is_neg() {
                                l
                            } else {
                                h
                            }
                        }
                        Some((Some(v), None)) if ptr.is_neg() => v,
                        Some((None, Some(v))) if !ptr.is_neg() => v,
                        // no cached value found, compute it
                        Some((None, cached)) | Some((cached, None)) => bottomup_helper(cached),
                        None => bottomup_helper(None),
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
        debug_assert!(self.is_scratch_cleared());
        fn count_h(ptr: SddPtr, alloc: &mut Bump) -> usize {
            if ptr.is_const() || ptr.is_var() {
                return 0;
            }
            match ptr.get_scratch::<usize>() {
                Some(_) => 0,
                None => {
                    // found a new node
                    ptr.set_scratch::<usize>(0);
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
        let r = count_h(*self, &mut Bump::new());
        self.clear_scratch();
        return r;
    }

    fn false_ptr() -> SddPtr<'a> {
        PtrFalse
    }

    fn true_ptr() -> SddPtr<'a> {
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
    let man = crate::builder::sdd_builder::SddManager::new(crate::repr::vtree::VTree::even_split(
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
    let man = crate::builder::sdd_builder::SddManager::new(crate::repr::vtree::VTree::even_split(
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
