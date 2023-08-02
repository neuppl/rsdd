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
    var_order::{PartialVariableOrder, VarOrder},
};

use SemanticBddPtr::*;

#[derive(Clone, Copy)]
pub enum SemanticBddPtr<'a, const P: u128> {
    PtrTrue,
    PtrFalse,
    Reg(&'a SemanticBddNode<P>, &'a SemanticBddBuilder<'a, P>),
    Compl(&'a SemanticBddNode<P>, &'a SemanticBddBuilder<'a, P>),
}

impl<'a, const P: u128> SemanticBddPtr<'a, P> {
    /// Gets the scratch value stored in `&self`
    ///
    /// Panics if not node.
    pub fn scratch<T: ?Sized + Clone + 'static>(&self) -> Option<T> {
        match self {
            Compl(n, _) | Reg(n, _) => {
                if self.is_scratch_cleared() {
                    return None;
                }
                println!("dereferencing {:?}", n.data.as_ptr());
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
            Compl(n, _) | Reg(n, _) => {
                *n.data.borrow_mut() = Some(Box::new(v));
            }
            _ => panic!("attempting to store scratch on constant"),
        }
    }

    /// Traverses the BDD and clears all scratch memory (sets it equal to 0)
    pub fn clear_scratch(&self) {
        match &self {
            Compl(x, builder) | Reg(x, builder) => {
                if x.data.borrow().is_some() {
                    x.data.take();
                    x.low(builder).clear_scratch();
                    x.high(builder).clear_scratch();
                }
            }
            PtrTrue | PtrFalse => (),
        }
    }

    /// true if the scratch is current cleared
    pub fn is_scratch_cleared(&self) -> bool {
        match self {
            Compl(n, _) | Reg(n, _) => n.data.borrow().is_none(),
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
                Compl(node, builder) | Reg(node, builder) => {
                    // inside the cache, store a (compl, non_compl) pair corresponding to the
                    // complemented and uncomplemented pass over this node

                    // helper performs actual fold-and-cache work
                    let bottomup_helper = |cached| {
                        let (l, h) = if ptr.is_neg() {
                            (node.low(builder).neg(), node.high(builder).neg())
                        } else {
                            (node.low(builder), node.high(builder))
                        };

                        let low_v = bottomup_pass_h(l, f);
                        let high_v = bottomup_pass_h(h, f);
                        let top = node.var();

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
            Reg(n, b) => Compl(n, b),
            Compl(n, b) => Reg(n, b),
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
            Compl(_, _) => true,
            Reg(_, _) | PtrTrue | PtrFalse => false,
        }
    }

    fn is_true(&self) -> bool {
        match &self {
            Compl(_, _) | Reg(_, _) | PtrFalse => false,
            PtrTrue => true,
        }
    }

    fn is_false(&self) -> bool {
        match &self {
            Compl(_, _) | Reg(_, _) | PtrTrue => false,
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
            Reg(node, _) => node.semantic_hash(),
            Compl(node, _) => node.semantic_hash().negate(),
        }
    }

    fn count_h(self, count: &mut usize) {
        match self {
            PtrTrue | PtrFalse => (),
            Reg(n, builder) | Compl(n, builder) => {
                match self.scratch::<usize>() {
                    Some(_) => (),
                    None => {
                        // found a new node
                        *count += 1;
                        self.set_scratch::<usize>(0);
                        n.low(builder).count_h(count);
                        n.high(builder).count_h(count);
                    }
                }
            }
        }
    }
}

impl<'a, const P: u128> PartialEq for SemanticBddPtr<'a, P> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Compl(l0, _), Self::Compl(r0, _)) => std::ptr::eq(*l0, *r0),
            (Self::Reg(l0, _), Self::Reg(r0, _)) => std::ptr::eq(*l0, *r0),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl<'a, const P: u128> Eq for SemanticBddPtr<'a, P> {}

impl<'a, const P: u128> Hash for SemanticBddPtr<'a, P> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Compl(n, _) | Reg(n, _) => ptr::hash(*n, state),
            _ => (),
        }
    }
}

impl<'a, const P: u128> Debug for SemanticBddPtr<'a, P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PtrTrue => write!(f, "PtrTrue"),
            Self::PtrFalse => write!(f, "PtrFalse"),
            Self::Reg(arg0, _) => f.debug_tuple("Reg").field(arg0).finish(),
            Self::Compl(arg0, _) => f.debug_tuple("Compl").field(arg0).finish(),
        }
    }
}

impl<'a, const P: u128> PartialVariableOrder for SemanticBddPtr<'a, P> {
    fn var(&self) -> Option<VarLabel> {
        match self {
            PtrTrue | PtrFalse => None,
            Reg(n, _) | Compl(n, _) => Some(n.var()),
        }
    }
}

#[derive(Debug)]
pub struct SemanticBddNode<const P: u128> {
    var: VarLabel,
    hash: FiniteField<P>,
    low_hash: FiniteField<P>,
    high_hash: FiniteField<P>,
    /// scratch space used for caching data during traversals; ignored during
    /// equality checking and hashing
    data: RefCell<Option<Box<dyn Any>>>,
}

impl<const P: u128> SemanticBddNode<P> {
    pub fn new(
        var: VarLabel,
        hash: FiniteField<P>,
        low_hash: FiniteField<P>,
        high_hash: FiniteField<P>,
    ) -> SemanticBddNode<P> {
        println!("n w/ hash: {}", hash);
        println!("lh: {}, hh: {}", low_hash, high_hash);
        SemanticBddNode {
            var,
            hash,
            low_hash,
            high_hash,
            data: RefCell::new(None),
        }
    }

    pub fn new_from_builder<'a>(
        var: VarLabel,
        low_hash: FiniteField<P>,
        high_hash: FiniteField<P>,
        builder: &'a SemanticBddBuilder<'a, P>,
    ) -> SemanticBddNode<P> {
        let (low_w, high_w) = builder.map().var_weight(var);
        let hash = low_hash * (*low_w) + high_hash * (*high_w);

        println!("nfb w/ hash: {}", hash);
        println!("lh: {}, hh: {}", low_hash, high_hash);

        SemanticBddNode {
            var,
            hash,
            low_hash,
            high_hash,
            data: RefCell::new(None),
        }
    }

    pub fn semantic_hash(&self) -> FiniteField<P> {
        self.hash
    }

    pub fn low<'a>(&self, builder: &'a SemanticBddBuilder<'a, P>) -> SemanticBddPtr<'a, P> {
        builder.deref_semantic_hash(self.low_hash)
    }

    pub fn high<'a>(&self, builder: &'a SemanticBddBuilder<'a, P>) -> SemanticBddPtr<'a, P> {
        builder.deref_semantic_hash(self.high_hash)
    }

    pub fn var(&self) -> VarLabel {
        self.var
    }
}

impl<const P: u128> PartialEq for SemanticBddNode<P> {
    fn eq(&self, other: &Self) -> bool {
        self.var == other.var
            && self.hash == other.hash
            && self.low_hash == other.low_hash
            && self.high_hash == other.high_hash
    }
}

impl<const P: u128> Hash for SemanticBddNode<P> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.var.hash(state);
        self.hash.hash(state);
        self.low_hash.hash(state);
        self.high_hash.hash(state);
    }
}

impl<const P: u128> Eq for SemanticBddNode<P> {}

impl<const P: u128> PartialOrd for SemanticBddNode<P> {
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

impl<const P: u128> Clone for SemanticBddNode<P> {
    fn clone(&self) -> Self {
        println!(
            "cloning... var: {:?}, hash: {}, lh: {}, hh: {}",
            self.var, self.hash, self.low_hash, self.high_hash
        );
        Self {
            var: self.var,
            hash: self.hash,
            low_hash: self.low_hash,
            high_hash: self.high_hash,
            data: RefCell::new(None),
        }
    }
}

impl<const P: u128> Ord for SemanticBddNode<P> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}
