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
    fn sdd_eq(&self, s1: SddPtr, s2: SddPtr) -> bool;
    fn app_cache(&mut self) -> &mut Self::ApplyCacheMethod;

    // BackedRobinhoodTable-related methods
    fn bdd_tbl(&self) -> &BackedRobinhoodTable<BinarySDD, Self::BddHasher>;
    fn sdd_tbl(&self) -> &BackedRobinhoodTable<SddOr, Self::SddOrHasher>;
    fn bdd_hasher(&self) -> &Self::BddHasher;
    fn sdd_hasher(&self) -> &Self::SddOrHasher;
    fn bdd_get_or_insert(&mut self, item: BinarySDD) -> *mut BinarySDD;
    fn sdd_get_or_insert(&mut self, item: SddOr) -> *mut SddOr;
}

pub struct CompressionCanonicalizer {
    use_compression: bool,
    app_cache: SddApplyCompression,
    bdd_tbl: BackedRobinhoodTable<BinarySDD, DefaultUniqueTableHasher>,
    sdd_tbl: BackedRobinhoodTable<SddOr, DefaultUniqueTableHasher>,
    hasher: DefaultUniqueTableHasher,
}

impl CompressionCanonicalizer {}

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

    fn sdd_eq(&self, s1: SddPtr, s2: SddPtr) -> bool {
        s1 == s2
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

    fn bdd_tbl(&self) -> &BackedRobinhoodTable<BinarySDD, Self::BddHasher> {
        &self.bdd_tbl
    }

    fn sdd_tbl(&self) -> &BackedRobinhoodTable<SddOr, Self::SddOrHasher> {
        &self.sdd_tbl
    }

    fn bdd_hasher(&self) -> &Self::BddHasher {
        &self.hasher
    }

    fn sdd_hasher(&self) -> &Self::SddOrHasher {
        &self.hasher
    }

    fn bdd_get_or_insert(&mut self, item: BinarySDD) -> *mut BinarySDD {
        self.bdd_tbl.get_or_insert(item, &self.hasher)
    }

    fn sdd_get_or_insert(&mut self, item: SddOr) -> *mut SddOr {
        self.sdd_tbl.get_or_insert(item, &self.hasher)
    }
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
    fn u64hash(&self, elem: &BinarySDD) -> u64 {
        (elem.low().semantic_hash(&self.vtree, &self.map)
            * elem.high().semantic_hash(&self.vtree, &self.map))
        .value() as u64
    }
}

impl<const P: u128> UniqueTableHasher<SddOr> for SemanticUniqueTableHasher<P> {
    fn u64hash(&self, elem: &SddOr) -> u64 {
        elem.nodes
            .clone()
            .into_iter()
            .map(|and| {
                and.prime().semantic_hash(&self.vtree, &self.map)
                    * and.sub().semantic_hash(&self.vtree, &self.map)
            })
            .fold(FiniteField::new(0), |accum, elem| accum + elem)
            .value() as u64
    }
}

pub struct SemanticCanonicalizer<const P: u128> {
    map: WmcParams<FiniteField<P>>,
    app_cache: SddApplySemantic<P>,
    use_compression: bool,
    vtree: VTreeManager,
    bdd_tbl: BackedRobinhoodTable<BinarySDD, SemanticUniqueTableHasher<P>>,
    sdd_tbl: BackedRobinhoodTable<SddOr, SemanticUniqueTableHasher<P>>,
    hasher: SemanticUniqueTableHasher<P>,
}

impl<const P: u128> SddCanonicalizationScheme for SemanticCanonicalizer<P> {
    type ApplyCacheMethod = SddApplySemantic<P>;
    type BddHasher = SemanticUniqueTableHasher<P>;
    type SddOrHasher = SemanticUniqueTableHasher<P>;

    fn new(vtree: &VTreeManager) -> Self {
        let map = create_semantic_hash_map(vtree.num_vars());
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

    fn sdd_eq(&self, s1: SddPtr, s2: SddPtr) -> bool {
        let h1 = s1.semantic_hash(&self.vtree, &self.map);
        let h2 = s2.semantic_hash(&self.vtree, &self.map);
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

    fn bdd_tbl(&self) -> &BackedRobinhoodTable<BinarySDD, Self::BddHasher> {
        &self.bdd_tbl
    }

    fn sdd_tbl(&self) -> &BackedRobinhoodTable<SddOr, Self::SddOrHasher> {
        &self.sdd_tbl
    }

    fn bdd_hasher(&self) -> &Self::BddHasher {
        &self.hasher
    }

    fn sdd_hasher(&self) -> &Self::SddOrHasher {
        &self.hasher
    }

    fn bdd_get_or_insert(&mut self, item: BinarySDD) -> *mut BinarySDD {
        self.bdd_tbl.get_or_insert(item, &self.hasher)
    }

    fn sdd_get_or_insert(&mut self, item: SddOr) -> *mut SddOr {
        self.sdd_tbl.get_or_insert(item, &self.hasher)
    }
}
