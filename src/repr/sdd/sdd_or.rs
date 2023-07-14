use crate::{
    repr::{
        sdd::SddPtr::{self, Compl, ComplBDD, PtrFalse, PtrTrue, Reg, Var, BDD},
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

/// An SddOr node is a vector of (prime, sub) pairs.
#[derive(Debug)]
pub struct SddOr<'a> {
    index: VTreeIndex,
    pub nodes: Vec<SddAnd<'a>>,

    // scratch
    pub scratch: RefCell<Option<Box<dyn Any>>>,
    pub semantic_hash: RefCell<Option<u128>>,
}

impl<'a> SddOr<'a> {
    pub fn new(nodes: Vec<SddAnd>, index: VTreeIndex) -> SddOr {
        SddOr {
            nodes,
            index,
            scratch: RefCell::new(None),
            semantic_hash: RefCell::new(None),
        }
    }

    #[inline]
    pub fn index(&self) -> VTreeIndex {
        self.index
    }

    pub fn semantic_hash<const P: u128>(
        &self,
        vtree: &VTreeManager,
        map: &WmcParams<FiniteField<P>>,
    ) -> FiniteField<P> {
        FiniteField::new(
            self.nodes
                .iter()
                .map(|and| and.semantic_hash(vtree, map).value())
                .sum(),
        )
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

        for n in &self.nodes {
            n.prime.clear_scratch();
            n.sub.clear_scratch();
        }
    }

    #[inline]
    pub fn is_scratch_cleared(&self) -> bool {
        self.scratch.borrow().is_none()
    }
}

impl<'a> PartialEq for SddOr<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index && self.nodes == other.nodes
    }
}

impl<'a> Eq for SddOr<'a> {}

impl<'a> PartialOrd for SddOr<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Ord for SddOr<'a> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.index.cmp(&other.index) {
            core::cmp::Ordering::Equal => {}
            ord => return ord,
        }
        match self.nodes.cmp(&other.nodes) {
            core::cmp::Ordering::Equal => {}
            ord => return ord,
        }
        core::cmp::Ordering::Equal
    }
}

impl<'a> Clone for SddOr<'a> {
    fn clone(&self) -> Self {
        Self {
            index: self.index,
            nodes: self.nodes.clone(),

            scratch: RefCell::new(None),
            semantic_hash: RefCell::new(None),
        }
    }
}

impl<'a> Hash for SddOr<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state);
        self.nodes.hash(state);
    }
}

/// Produces a node iterator for SDD or-nodes from an SDD pointer
pub struct SddNodeIter<'a> {
    sdd: SddPtr<'a>,
    count: usize,
}

impl<'a> SddNodeIter<'a> {
    pub fn new(sdd: SddPtr<'a>) -> SddNodeIter<'a> {
        SddNodeIter { sdd, count: 0 }
    }
}

impl<'a> Iterator for SddNodeIter<'a> {
    type Item = SddAnd<'a> where Self: 'a;

    fn next(&mut self) -> Option<Self::Item> {
        match self.sdd {
            PtrTrue | PtrFalse | Var(_, _) => panic!("called iterator on constant"),
            BDD(bdd) | ComplBDD(bdd) => match self.count {
                0 => {
                    self.count += 1;
                    Some(SddAnd::new(SddPtr::Var(bdd.label(), true), bdd.high()))
                }
                1 => {
                    self.count += 1;
                    Some(SddAnd::new(SddPtr::Var(bdd.label(), false), bdd.low()))
                }
                _ => None,
            },
            Reg(or) | Compl(or) => {
                if self.count >= or.nodes.len() {
                    None
                } else {
                    self.count += 1;
                    Some(or.nodes[self.count - 1])
                }
            }
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Ord, PartialOrd, Copy)]
pub struct SddAnd<'a> {
    pub prime: SddPtr<'a>,
    pub sub: SddPtr<'a>,
}

impl<'a> SddAnd<'a> {
    pub fn prime(&self) -> SddPtr<'a> {
        self.prime
    }
    pub fn sub(&self) -> SddPtr<'a> {
        self.sub
    }
    pub fn new(prime: SddPtr<'a>, sub: SddPtr<'a>) -> SddAnd<'a> {
        SddAnd { prime, sub }
    }
    pub fn semantic_hash<const P: u128>(
        &self,
        vtree: &VTreeManager,
        map: &WmcParams<FiniteField<P>>,
    ) -> FiniteField<P> {
        self.prime.cached_semantic_hash(vtree, map) * self.sub.cached_semantic_hash(vtree, map)
    }
}
