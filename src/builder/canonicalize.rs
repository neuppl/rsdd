use std::hash::{Hash, Hasher};

use rustc_hash::FxHasher;

use super::bdd_builder::DDNNFPtr;
use super::cache::sdd_apply_cache::{SddApply, SddApplyCompression, SddApplySemantic};
use crate::backing_store::bump_table::BackedRobinhoodTable;
use crate::backing_store::{DefaultUniqueTableHasher, UniqueTable, UniqueTableHasher};
use crate::repr::bdd::{create_semantic_hash_map, WmcParams};
use crate::repr::sdd::{BinarySDD, SddOr, SddPtr};
use crate::repr::vtree::VTreeManager;
use crate::util::semiring::FiniteField;

pub trait SddCanonicalizationScheme {
    type ApplyCacheMethod: SddApply;
    type BddHasher: UniqueTableHasher<BinarySDD>;
    type SddOrHasher: UniqueTableHasher<SddOr>;

    fn new(vtree: &VTreeManager) -> Self;
    fn set_compress(&mut self, b: bool);
    fn should_compress(&self) -> bool;

    /// this is mutable because we may update an internal cache
    fn sdd_eq(&mut self, a: SddPtr, b: SddPtr) -> bool;
    fn app_cache(&mut self) -> &mut Self::ApplyCacheMethod;

    // BackedRobinhoodTable-related methods
    fn bdd_tbl(&self) -> &BackedRobinhoodTable<BinarySDD>;
    fn sdd_tbl(&self) -> &BackedRobinhoodTable<SddOr>;
    fn bdd_get_or_insert(&mut self, item: BinarySDD) -> *mut BinarySDD;
    fn sdd_get_or_insert(&mut self, item: SddOr) -> *mut SddOr;

    // debugging util
    fn on_sdd_print_dump_state(&self, ptr: SddPtr);
}

pub struct CompressionCanonicalizer {
    use_compression: bool,
    app_cache: SddApplyCompression,
    bdd_tbl: BackedRobinhoodTable<BinarySDD>,
    sdd_tbl: BackedRobinhoodTable<SddOr>,
    hasher: DefaultUniqueTableHasher,
}

impl SddCanonicalizationScheme for CompressionCanonicalizer {
    type ApplyCacheMethod = SddApplyCompression;
    type BddHasher = DefaultUniqueTableHasher;
    type SddOrHasher = DefaultUniqueTableHasher;

    fn new(_vtree: &VTreeManager) -> Self {
        CompressionCanonicalizer {
            use_compression: true,
            app_cache: SddApplyCompression::new(),
            bdd_tbl: BackedRobinhoodTable::new(),
            sdd_tbl: BackedRobinhoodTable::new(),
            hasher: DefaultUniqueTableHasher::default(),
        }
    }

    fn sdd_eq(&mut self, a: SddPtr, b: SddPtr) -> bool {
        a == b
    }

    fn set_compress(&mut self, b: bool) {
        self.use_compression = b
    }

    fn should_compress(&self) -> bool {
        self.use_compression
    }

    fn app_cache(&mut self) -> &mut Self::ApplyCacheMethod {
        &mut self.app_cache
    }

    fn bdd_tbl(&self) -> &BackedRobinhoodTable<BinarySDD> {
        &self.bdd_tbl
    }

    fn sdd_tbl(&self) -> &BackedRobinhoodTable<SddOr> {
        &self.sdd_tbl
    }

    fn bdd_get_or_insert(&mut self, item: BinarySDD) -> *mut BinarySDD {
        self.bdd_tbl.get_or_insert(item, &self.hasher)
    }

    fn sdd_get_or_insert(&mut self, item: SddOr) -> *mut SddOr {
        self.sdd_tbl.get_or_insert(item, &self.hasher)
    }

    fn on_sdd_print_dump_state(&self, _ptr: SddPtr) {}
}

pub struct SemanticUniqueTableHasher<const P: u128> {
    map: WmcParams<FiniteField<P>>,
    vtree: VTreeManager,
}

impl<const P: u128> SemanticUniqueTableHasher<P> {
    pub fn new(vtree: VTreeManager, map: WmcParams<FiniteField<P>>) -> Self {
        Self { vtree, map }
    }
}

impl<const P: u128> UniqueTableHasher<BinarySDD> for SemanticUniqueTableHasher<P> {
    // TODO(matt): we should be able to de-duplicate this with fold/wmc
    fn u64hash(&self, elem: &BinarySDD) -> u64 {
        let mut hasher = FxHasher::default();

        let (low_w, high_w) = self.map.get_var_weight(elem.label());

        (elem
            .low()
            .cached_semantic_hash(&self.vtree, &self.map)
            .value()
            * low_w.value()
            + elem
                .high()
                .cached_semantic_hash(&self.vtree, &self.map)
                .value()
                * high_w.value())
        .hash(&mut hasher);
        hasher.finish()
    }
}

impl<const P: u128> UniqueTableHasher<SddOr> for SemanticUniqueTableHasher<P> {
    fn u64hash(&self, elem: &SddOr) -> u64 {
        let mut hasher = FxHasher::default();
        elem.nodes
            .iter()
            .map(|and| and.semantic_hash(&self.vtree, &self.map).value())
            .fold(0, |accum, elem| accum + elem)
            .hash(&mut hasher);
        hasher.finish()
    }
}

pub struct SemanticCanonicalizer<const P: u128> {
    map: WmcParams<FiniteField<P>>,
    app_cache: SddApplySemantic<P>,
    use_compression: bool,
    vtree: VTreeManager,
    bdd_tbl: BackedRobinhoodTable<BinarySDD>,
    sdd_tbl: BackedRobinhoodTable<SddOr>,
    hasher: SemanticUniqueTableHasher<P>,
}

impl<const P: u128> SddCanonicalizationScheme for SemanticCanonicalizer<P> {
    type ApplyCacheMethod = SddApplySemantic<P>;
    type BddHasher = SemanticUniqueTableHasher<P>;
    type SddOrHasher = SemanticUniqueTableHasher<P>;

    fn new(vtree: &VTreeManager) -> Self {
        let map = create_semantic_hash_map(vtree.vtree_root().num_vars());
        let app_cache = SddApplySemantic::new(map.clone(), vtree.clone());
        SemanticCanonicalizer {
            app_cache,
            use_compression: false,
            vtree: vtree.clone(),
            bdd_tbl: BackedRobinhoodTable::new(),
            sdd_tbl: BackedRobinhoodTable::new(),
            hasher: SemanticUniqueTableHasher::new(vtree.clone(), map.clone()),
            map,
        }
    }

    fn sdd_eq(&mut self, a: SddPtr, b: SddPtr) -> bool {
        let h1 = a.cached_semantic_hash(&self.vtree, &self.map);
        let h2 = b.cached_semantic_hash(&self.vtree, &self.map);
        h1 == h2
    }

    fn set_compress(&mut self, b: bool) {
        self.use_compression = b
    }

    fn should_compress(&self) -> bool {
        self.use_compression
    }

    fn app_cache(&mut self) -> &mut Self::ApplyCacheMethod {
        &mut self.app_cache
    }

    fn bdd_tbl(&self) -> &BackedRobinhoodTable<BinarySDD> {
        &self.bdd_tbl
    }

    fn sdd_tbl(&self) -> &BackedRobinhoodTable<SddOr> {
        &self.sdd_tbl
    }

    fn bdd_get_or_insert(&mut self, item: BinarySDD) -> *mut BinarySDD {
        self.bdd_tbl.get_or_insert(item, &self.hasher)
    }

    fn sdd_get_or_insert(&mut self, item: SddOr) -> *mut SddOr {
        self.sdd_tbl.get_or_insert(item, &self.hasher)
    }

    fn on_sdd_print_dump_state(&self, ptr: SddPtr) {
        println!("h: {}", ptr.semantic_hash(&self.vtree, &self.map));
    }
}
