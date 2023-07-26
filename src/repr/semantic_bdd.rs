use std::{
    any::Any,
    cell::RefCell,
    fmt::Debug,
    hash::{Hash, Hasher},
    ptr,
};

use crate::{
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
    Reg(&'a SemanticBddNode<P>),
    Compl(&'a SemanticBddNode<P>),
}

impl<'a, const P: u128> SemanticBddPtr<'a, P> {
    pub fn is_const(&self) -> bool {
        match &self {
            Reg(_) | Compl(_) => false,
            PtrTrue | PtrFalse => true,
        }
    }

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
                    x.low.clear_scratch();
                    x.high.clear_scratch();
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
                            (ptr.low_raw().neg(), ptr.high_raw().neg())
                        } else {
                            (ptr.low_raw(), ptr.high_raw())
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
        // debug_assert!(self.is_scratch_cleared());
        // fn count_h(ptr: SemanticBddPtr<P>, count: &mut usize) {
        //     if ptr.is_const() {
        //         return;
        //     }
        //     match ptr.scratch::<usize>() {
        //         Some(_) => (),
        //         None => {
        //             // found a new node
        //             *count += 1;
        //             ptr.set_scratch::<usize>(0);
        //             count_h(ptr.low_raw(), count);
        //             count_h(ptr.high_raw(), count);
        //         }
        //     }
        // }
        // let mut count = 0;
        // count_h(*self, &mut count);
        // self.clear_scratch();
        // count
        todo!()
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
pub struct SemanticBddNode<const P: u128> {
    var: VarLabel,
    hash: FiniteField<P>,
    low: FiniteField<P>,
    high: FiniteField<P>,
    /// scratch space used for caching data during traversals; ignored during
    /// equality checking and hashing
    data: RefCell<Option<Box<dyn Any>>>,
}

impl<const P: u128> SemanticBddNode<P> {
    pub fn new(
        var: VarLabel,
        low: FiniteField<P>,
        high: FiniteField<P>,
        map: &WmcParams<FiniteField<P>>,
    ) -> SemanticBddNode<P> {
        let (low_w, high_w) = map.var_weight(var);
        let hash = low * (*low_w) + high * (*high_w);

        SemanticBddNode {
            var,
            hash,
            low,
            high,
            data: RefCell::new(None),
        }
    }
}

impl<const P: u128> PartialEq for SemanticBddNode<P> {
    fn eq(&self, other: &Self) -> bool {
        self.var == other.var && self.low == other.low && self.high == other.high
    }
}

impl<const P: u128> Hash for SemanticBddNode<P> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.var.hash(state);
        self.hash.hash(state);
        self.low.hash(state);
        self.high.hash(state);
    }
}

impl<const P: u128> Eq for SemanticBddNode<P> {}

impl<const P: u128> PartialOrd for SemanticBddNode<P> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.var.partial_cmp(&other.var) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        match self.low.partial_cmp(&other.low) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        match self.high.partial_cmp(&other.high) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        Some(core::cmp::Ordering::Equal)
    }
}

impl<const P: u128> Clone for SemanticBddNode<P> {
    fn clone(&self) -> Self {
        Self {
            var: self.var,
            hash: self.hash,
            low: self.low,
            high: self.high,
            data: RefCell::new(None),
        }
    }
}

impl<const P: u128> Ord for SemanticBddNode<P> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}
