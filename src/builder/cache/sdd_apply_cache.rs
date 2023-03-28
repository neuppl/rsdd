use rustc_hash::FxHashMap;

use crate::{
    repr::{
        bdd::WmcParams,
        sdd::{SddAnd, SddPtr},
        vtree::VTreeManager,
    },
    util::semiring::FiniteField,
};

pub trait SddApply {
    fn get(&self, and: SddAnd) -> Option<SddPtr>;
    fn insert(&mut self, and: SddAnd, ptr: SddPtr);
}

pub struct SddApplyCompression {
    table: FxHashMap<SddAnd, SddPtr>,
}

impl SddApplyCompression {
    pub fn new() -> SddApplyCompression {
        SddApplyCompression {
            table: FxHashMap::default(),
        }
    }
}

impl SddApply for SddApplyCompression {
    fn get(&self, and: SddAnd) -> Option<SddPtr> {
        self.table.get(&and).cloned()
    }
    fn insert(&mut self, and: SddAnd, ptr: SddPtr) {
        self.table.insert(and, ptr);
    }
}

impl Default for SddApplyCompression {
    fn default() -> Self {
        Self::new()
    }
}

pub struct SddApplySemantic<const P: u128> {
    table: FxHashMap<u128, SddPtr>,
    map: WmcParams<FiniteField<P>>,
    vtree: VTreeManager,
}

impl<const P: u128> SddApplySemantic<P> {
    pub fn new<'a>(map: WmcParams<FiniteField<P>>, vtree: VTreeManager) -> SddApplySemantic<P> {
        SddApplySemantic {
            table: FxHashMap::default(),
            map,
            vtree,
        }
    }
}

impl<const P: u128> SddApply for SddApplySemantic<P> {
    fn get(&self, and: SddAnd) -> Option<SddPtr> {
        let h = and.semantic_hash(&self.vtree, &self.map);
        match h.value() {
            0 => Some(SddPtr::PtrFalse),
            1 => Some(SddPtr::PtrTrue),
            _ => self.table.get(&h.value()).copied(),
        }
    }
    fn insert(&mut self, and: SddAnd, ptr: SddPtr) {
        let h = and.semantic_hash(&self.vtree, &self.map);
        if h.value() > 1 {
            self.table.insert(h.value(), ptr);
        }
    }
}
