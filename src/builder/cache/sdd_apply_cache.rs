use rustc_hash::FxHashMap;

use crate::{
    repr::{
        robdd::WmcParams,
        sdd::{sdd_or::SddAnd, SddPtr},
        vtree::VTreeManager,
    },
    util::semiring::FiniteField,
};

pub trait SddApply<'a> {
    fn get(&'a self, and: SddAnd<'a>) -> Option<SddPtr<'a>>;
    fn insert(&'a mut self, and: SddAnd<'a>, ptr: SddPtr<'a>);
}

pub struct SddApplyCompression<'a> {
    table: FxHashMap<SddAnd<'a>, SddPtr<'a>>,
}

impl<'a> SddApplyCompression<'a> {
    pub fn new() -> SddApplyCompression<'a> {
        SddApplyCompression {
            table: FxHashMap::default(),
        }
    }
}

impl<'a> SddApply<'a> for SddApplyCompression<'a> {
    fn get(&'a self, and: SddAnd) -> Option<SddPtr> {
        self.table.get(&and).cloned()
    }
    fn insert(&mut self, and: SddAnd<'a>, ptr: SddPtr<'a>) {
        self.table.insert(and, ptr);
    }
}

impl<'a> Default for SddApplyCompression<'a> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct SddApplySemantic<'a, const P: u128> {
    table: FxHashMap<u128, SddPtr<'a>>,
    map: WmcParams<FiniteField<P>>,
    vtree: VTreeManager,
}

impl<'a, const P: u128> SddApplySemantic<'a, P> {
    pub fn new(map: WmcParams<FiniteField<P>>, vtree: VTreeManager) -> SddApplySemantic<'a, P> {
        SddApplySemantic {
            table: FxHashMap::default(),
            map,
            vtree,
        }
    }
}

impl<'a, const P: u128> SddApply<'a> for SddApplySemantic<'a, P> {
    fn get(&self, and: SddAnd) -> Option<SddPtr> {
        let h = and.semantic_hash(&self.vtree, &self.map);
        match h.value() {
            0 => Some(SddPtr::PtrFalse),
            1 => Some(SddPtr::PtrTrue),
            _ => self.table.get(&h.value()).copied(),
        }
    }
    /// assumption: we've already checked if element is in cache
    fn insert(&'a mut self, and: SddAnd<'a>, ptr: SddPtr<'a>) {
        let h = and.semantic_hash(&self.vtree, &self.map);
        if h.value() > 1 {
            self.table.insert(h.value(), ptr);
        }
    }
}
