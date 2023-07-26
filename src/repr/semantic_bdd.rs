use std::{
    any::Any,
    cell::RefCell,
    fmt::Debug,
    hash::{Hash, Hasher},
    ptr,
};

use crate::{
    builder::parallel::SemanticBddBuilder,
    repr::var_label::VarSet,
    util::semirings::{FiniteField, Semiring},
};

use super::{
    ddnnf::{DDNNFPtr, DDNNF},
    var_label::VarLabel,
    var_order::VarOrder,
    wmc::WmcParams,
};

use SemanticBddPtr::*;

#[derive(Debug, Clone, Copy, Eq, PartialOrd, Ord)]
pub enum SemanticBddPtr<'a, const P: u128> {
    PtrTrue,
    PtrFalse,
    Reg(&'a SemanticBddNode<'a, P>),
    Compl(&'a SemanticBddNode<'a, P>),
}

impl<'a, const P: u128> SemanticBddPtr<'a, P> {
    /// Gets the scratch value stored in `&self`
    ///
    /// Panics if not node.
    pub fn scratch<T: ?Sized + Clone + 'static>(&self) -> Option<T> {
        match self {
            Compl(n) | Reg(n) => {
                if self.is_scratch_cleared() {
                    return None;
                }
                // println!("dereferencing {:?}", n.data.as_ptr());
                n.data
                    .borrow()
                    .as_ref()
                    .unwrap()
                    .as_ref()
                    .downcast_ref::<T>()
                    .cloned()
            }
            PtrTrue => None,
            PtrFalse => None,
        }
    }

    /// Set the scratch in this node to the value `v`.
    ///
    /// Panics if not a node.
    ///
    /// Invariant: values stored in `set_scratch` must not outlive
    /// the provided allocator `alloc` (i.e., calling `scratch`
    /// involves dereferencing a pointer stored in `alloc`)
    pub fn set_scratch<T: 'static>(&self, v: T) {
        match self {
            Compl(n) | Reg(n) => {
                *n.data.borrow_mut() = Some(Box::new(v));
            }
            _ => panic!("attempting to store scratch on constant"),
        }
    }

    /// Traverses the BDD and clears all scratch memory (sets it equal to 0)
    pub fn clear_scratch(&self) {
        match &self {
            Compl(x) | Reg(x) => {
                if x.data.borrow().is_some() {
                    x.data.take();
                    x.low().clear_scratch();
                    x.high().clear_scratch();
                }
            }
            PtrTrue | PtrFalse => (),
        }
    }

    /// true if the scratch is current cleared
    pub fn is_scratch_cleared(&self) -> bool {
        match self {
            Compl(n) | Reg(n) => n.data.borrow().is_none(),
            PtrTrue => true,
            PtrFalse => true,
        }
    }
}

type DDNNFCache<T> = (Option<T>, Option<T>);

impl<'a, const P: u128> DDNNFPtr<'a> for SemanticBddPtr<'a, P> {
    type Order = VarOrder;

    fn fold<T: Clone + Copy + Debug, F: Fn(DDNNF<T>) -> T>(&self, _o: &VarOrder, f: F) -> T
    where
        T: 'static,
    {
        debug_assert!(self.is_scratch_cleared());
        fn bottomup_pass_h<T: Clone + Copy + Debug, F: Fn(DDNNF<T>) -> T, const P: u128>(
            ptr: SemanticBddPtr<P>,
            f: &F,
        ) -> T
        where
            T: 'static,
        {
            match ptr {
                PtrTrue => f(DDNNF::True),
                PtrFalse => f(DDNNF::False),
                Compl(node) | Reg(node) => {
                    // inside the cache, store a (compl, non_compl) pair corresponding to the
                    // complemented and uncomplemented pass over this node

                    // helper performs actual fold-and-cache work
                    let bottomup_helper = |cached| {
                        let (l, h) = if ptr.is_neg() {
                            (node.low().neg(), node.high().neg())
                        } else {
                            (node.low(), node.high())
                        };

                        let low_v = bottomup_pass_h(l, f);
                        let high_v = bottomup_pass_h(h, f);
                        let top = node.var;

                        let lit_high = f(DDNNF::Lit(top, true));
                        let lit_low = f(DDNNF::Lit(top, false));

                        let and_low = f(DDNNF::And(lit_low, low_v));
                        let and_high = f(DDNNF::And(lit_high, high_v));

                        // in a BDD, each decision only depends on the topvar
                        let mut varset = VarSet::new();
                        varset.insert(top);

                        let or_v = f(DDNNF::Or(and_low, and_high, varset));

                        // cache and return or_v
                        if ptr.is_neg() {
                            ptr.set_scratch::<DDNNFCache<T>>((Some(or_v), cached));
                        } else {
                            ptr.set_scratch::<DDNNFCache<T>>((cached, Some(or_v)));
                        }
                        or_v
                    };

                    match ptr.scratch::<DDNNFCache<T>>() {
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

        let r = bottomup_pass_h(*self, &f);
        self.clear_scratch();
        r
    }

    fn neg(&self) -> Self {
        match self {
            PtrTrue => PtrFalse,
            PtrFalse => PtrTrue,
            Reg(n) => Compl(n),
            Compl(n) => Reg(n),
        }
    }

    fn false_ptr() -> Self {
        PtrFalse
    }

    fn true_ptr() -> Self {
        PtrTrue
    }

    fn is_neg(&self) -> bool {
        match &self {
            Compl(_) => true,
            Reg(_) | PtrTrue | PtrFalse => false,
        }
    }

    fn is_true(&self) -> bool {
        match &self {
            Compl(_) | Reg(_) | PtrFalse => false,
            PtrTrue => true,
        }
    }

    fn is_false(&self) -> bool {
        match &self {
            Compl(_) | Reg(_) | PtrTrue => false,
            PtrFalse => true,
        }
    }

    fn count_nodes(&self) -> usize {
        debug_assert!(self.is_scratch_cleared());

        let mut count = 0;
        self.count_h(&mut count);
        self.clear_scratch();
        count
    }
}

impl<'a, const P: u128> SemanticBddPtr<'a, P> {
    pub fn semantic_hash(&self) -> FiniteField<P> {
        match self {
            PtrTrue => FiniteField::one(),
            PtrFalse => FiniteField::zero(),
            Reg(node) => node.hash,
            Compl(node) => node.hash.negate(),
        }
    }

    fn count_h(self, count: &mut usize) {
        match self {
            PtrTrue | PtrFalse => (),
            Reg(n) | Compl(n) => {
                match self.scratch::<usize>() {
                    Some(_) => (),
                    None => {
                        // found a new node
                        *count += 1;
                        self.set_scratch::<usize>(0);
                        n.low().count_h(count);
                        n.high().count_h(count);
                    }
                }
            }
        }
    }
}

impl<'a, const P: u128> PartialEq for SemanticBddPtr<'a, P> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Compl(l0), Self::Compl(r0)) => std::ptr::eq(*l0, *r0),
            (Self::Reg(l0), Self::Reg(r0)) => std::ptr::eq(*l0, *r0),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl<'a, const P: u128> Hash for SemanticBddPtr<'a, P> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Compl(n) | Reg(n) => ptr::hash(*n, state),
            _ => (),
        }
    }
}

#[derive(Debug)]
pub struct SemanticBddNode<'a, const P: u128> {
    var: VarLabel,
    hash: FiniteField<P>,
    low_hash: FiniteField<P>,
    high_hash: FiniteField<P>,
    /// builder ref is needed for dereferencing semantic hashes, to get to low/highs
    builder: &'a SemanticBddBuilder<'a, P>,
    /// scratch space used for caching data during traversals; ignored during
    /// equality checking and hashing
    data: RefCell<Option<Box<dyn Any>>>,
}

impl<'a, const P: u128> SemanticBddNode<'a, P> {
    pub fn new(
        var: VarLabel,
        low_hash: FiniteField<P>,
        high_hash: FiniteField<P>,
        builder: &'a SemanticBddBuilder<'a, P>,
        map: &WmcParams<FiniteField<P>>,
    ) -> SemanticBddNode<'a, P> {
        let (low_w, high_w) = map.var_weight(var);
        let hash = low_hash * (*low_w) + high_hash * (*high_w);

        SemanticBddNode {
            var,
            hash,
            low_hash,
            high_hash,
            builder,
            data: RefCell::new(None),
        }
    }

    pub fn low(&self) -> SemanticBddPtr<'a, P> {
        self.builder.deref_semantic_hash(self.low_hash)
    }

    pub fn high(&self) -> SemanticBddPtr<'a, P> {
        self.builder.deref_semantic_hash(self.high_hash)
    }
}

impl<'a, const P: u128> PartialEq for SemanticBddNode<'a, P> {
    fn eq(&self, other: &Self) -> bool {
        self.var == other.var
            && self.low_hash == other.low_hash
            && self.high_hash == other.high_hash
    }
}

impl<'a, const P: u128> Hash for SemanticBddNode<'a, P> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.var.hash(state);
        self.hash.hash(state);
        self.low_hash.hash(state);
        self.high_hash.hash(state);
    }
}

impl<'a, const P: u128> Eq for SemanticBddNode<'a, P> {}

impl<'a, const P: u128> PartialOrd for SemanticBddNode<'a, P> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.var.partial_cmp(&other.var) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        match self.low_hash.partial_cmp(&other.low_hash) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        match self.high_hash.partial_cmp(&other.high_hash) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        Some(core::cmp::Ordering::Equal)
    }
}

impl<'a, const P: u128> Clone for SemanticBddNode<'a, P> {
    fn clone(&self) -> Self {
        Self {
            var: self.var,
            hash: self.hash,
            low_hash: self.low_hash,
            high_hash: self.high_hash,
            builder: self.builder,
            data: RefCell::new(None),
        }
    }
}

impl<'a, const P: u128> Ord for SemanticBddNode<'a, P> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}
