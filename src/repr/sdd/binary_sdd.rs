use crate::{
    repr::{
        sdd::SddPtr,
        var_label::VarLabel,
        vtree::{VTreeIndex, VTreeManager},
        wmc::WmcParams,
    },
    util::semirings::FiniteField,
};
use std::{
    any::Any,
    cell::RefCell,
    hash::{Hash, Hasher},
};

/// Specialized SDD node for a right-linear sub-vtree
/// SDDs for these fragments are binary decisions
#[derive(Debug)]
pub struct BinarySDD<'a> {
    label: VarLabel,
    vtree: VTreeIndex,
    low: SddPtr<'a>,
    high: SddPtr<'a>,

    /// scratch data types
    scratch: RefCell<Option<Box<dyn Any>>>,
    semantic_hash: RefCell<Option<u128>>,
}

impl<'a> BinarySDD<'a> {
    pub fn new(
        label: VarLabel,
        low: SddPtr<'a>,
        high: SddPtr<'a>,
        vtree: VTreeIndex,
    ) -> BinarySDD<'a> {
        BinarySDD {
            label,
            low,
            high,
            vtree,
            semantic_hash: RefCell::new(None),
            scratch: RefCell::new(None),
        }
    }

    #[inline]
    pub fn vtree(&self) -> VTreeIndex {
        self.vtree
    }

    #[inline]
    pub fn low(&self) -> SddPtr<'a> {
        self.low
    }

    #[inline]
    pub fn high(&self) -> SddPtr<'a> {
        self.high
    }

    #[inline]
    pub fn label(&self) -> VarLabel {
        self.label
    }

    pub fn semantic_hash<const P: u128>(
        &self,
        vtree: &VTreeManager,
        map: &WmcParams<FiniteField<P>>,
    ) -> FiniteField<P> {
        let (low_w, high_w) = map.get_var_weight(self.label());
        self.low().cached_semantic_hash(vtree, map) * (*low_w)
            + self.high().cached_semantic_hash(vtree, map) * (*high_w)
    }

    pub fn cached_semantic_hash<const P: u128>(
        &self,
        vtree: &VTreeManager,
        map: &WmcParams<FiniteField<P>>,
    ) -> FiniteField<P> {
        if let Some(h) = *(self.semantic_hash.borrow()) {
            return FiniteField::new(h);
        }

        let h = self.semantic_hash(vtree, map);
        *(self.semantic_hash.borrow_mut()) = Some(h.value());

        h
    }

    pub fn get_scratch<T: ?Sized + Clone + 'static>(&self) -> Option<T>
    where
        T: Clone,
    {
        if self.scratch.borrow().is_none() {
            return None;
        }
        self.scratch
            .borrow()
            .as_ref()
            .unwrap()
            .as_ref()
            .downcast_ref::<T>()
            .cloned()
    }

    pub fn set_scratch<T: 'static>(&self, v: T) {
        *self.scratch.borrow_mut() = Some(Box::new(v));
    }

    pub fn clear_scratch(&self) {
        *(self.scratch.borrow_mut()) = None;

        self.low.clear_scratch();
        self.high.clear_scratch();
    }

    #[inline]
    pub fn is_scratch_cleared(&self) -> bool {
        self.scratch.borrow().is_none()
    }
}

impl<'a> Hash for BinarySDD<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.vtree.hash(state);
        self.label.hash(state);
        self.low.hash(state);
        self.high.hash(state);
    }
}

impl<'a> PartialEq for BinarySDD<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.vtree == other.vtree
            && self.low == other.low
            && self.high == other.high
            && self.label == other.label
    }
}

impl<'a> Eq for BinarySDD<'a> {}

impl<'a> PartialOrd for BinarySDD<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Ord for BinarySDD<'a> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.label.cmp(&other.label) {
            core::cmp::Ordering::Equal => {}
            ord => return ord,
        }
        match self.vtree.cmp(&other.vtree) {
            core::cmp::Ordering::Equal => {}
            ord => return ord,
        }
        match self.low.cmp(&other.low) {
            core::cmp::Ordering::Equal => {}
            ord => return ord,
        }
        match self.high.cmp(&other.high) {
            core::cmp::Ordering::Equal => {}
            ord => return ord,
        }
        core::cmp::Ordering::Equal
    }
}

impl<'a> Clone for BinarySDD<'a> {
    fn clone(&self) -> Self {
        Self {
            label: self.label,
            vtree: self.vtree,
            low: self.low,
            high: self.high,
            scratch: RefCell::new(None),
            semantic_hash: RefCell::new(None),
        }
    }
}
