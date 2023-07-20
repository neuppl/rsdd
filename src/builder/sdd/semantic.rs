use crate::{
    backing_store::BackedRobinhoodTable,
    builder::{
        cache::ite::Ite,
        sdd::{SddBuilder, SddBuilderStats},
    },
    repr::{
        ddnnf::{create_semantic_hash_map, DDNNFPtr},
        sdd::{BinarySDD, SddAnd, SddOr, SddPtr},
        vtree::{VTree, VTreeIndex, VTreeManager},
        wmc::WmcParams,
    },
    util::semirings::FiniteField,
};
use rustc_hash::FxHasher;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    hash::{Hash, Hasher},
};

pub struct SemanticSddBuilder<'a, const P: u128> {
    vtree: VTreeManager,
    should_compress: bool,
    // tables
    bdd_tbl: RefCell<BackedRobinhoodTable<'a, BinarySDD<'a>>>,
    sdd_tbl: RefCell<BackedRobinhoodTable<'a, SddOr<'a>>>,
    // caches
    // ite_cache: RefCell<AllTable<SddPtr<'a>>>,
    app_cache: RefCell<HashMap<u128, SddPtr<'a>>>,
    // semantic hashing
    map: WmcParams<FiniteField<P>>,
    // stats
    num_recursive_calls: RefCell<usize>,
    num_get_or_insert_bdd: RefCell<usize>,
    num_get_or_insert_sdd: RefCell<usize>,
}

impl<'a, const P: u128> SddBuilder<'a> for SemanticSddBuilder<'a, P> {
    #[inline]
    fn vtree_manager(&self) -> &VTreeManager {
        &self.vtree
    }

    #[inline]
    fn set_compression(&mut self, b: bool) {
        self.should_compress = b
    }

    fn node_iter(&self) -> Vec<SddPtr> {
        let binding = self.bdd_tbl.borrow_mut();
        let bdds = binding.iter().map(SddPtr::BDD);
        let binding = self.sdd_tbl.borrow_mut();
        let sdds = binding.iter().map(SddPtr::Reg);
        bdds.chain(sdds).collect()
    }

    fn app_cache_get(&self, and: &SddAnd<'a>) -> Option<SddPtr<'a>> {
        let h = and.semantic_hash(&self.vtree, &self.map);
        match h.value() {
            0 => Some(SddPtr::PtrFalse),
            1 => Some(SddPtr::PtrTrue),
            _ => self.app_cache.borrow().get(&h.value()).copied(),
        }
    }

    fn app_cache_insert(&self, and: SddAnd<'a>, ptr: SddPtr<'a>) {
        let h = and.semantic_hash(&self.vtree, &self.map);
        if h.value() > 1 {
            self.app_cache.borrow_mut().insert(h.value(), ptr);
        }
    }

    fn ite_cache_hash(&self, _ite: &Ite<SddPtr>) -> u64 {
        todo!()
    }

    fn ite_cache_get(&self, _ite: Ite<SddPtr<'a>>, _hash: u64) -> Option<SddPtr> {
        todo!()
    }

    fn ite_cache_insert(&self, _ite: Ite<SddPtr<'a>>, _res: SddPtr<'a>, _hash: u64) {
        todo!()
    }

    fn get_or_insert_bdd(&'a self, bdd: BinarySDD<'a>) -> SddPtr<'a> {
        *self.num_get_or_insert_bdd.borrow_mut() += 1;
        let semantic_hash = bdd.semantic_hash(&self.vtree, &self.map);

        if let Some(sdd) = self.check_cached_hash_and_neg(semantic_hash) {
            return sdd;
        }

        let hash = self.hash_bdd(&bdd);
        unsafe {
            let tbl = &mut *self.bdd_tbl.as_ptr();
            SddPtr::BDD(tbl.get_or_insert_by_hash(hash, bdd, true))
        }
    }

    fn get_or_insert_sdd(&'a self, or: SddOr<'a>) -> SddPtr<'a> {
        *self.num_get_or_insert_sdd.borrow_mut() += 1;
        let semantic_hash = or.semantic_hash(&self.vtree, &self.map);
        if let Some(sdd) = self.check_cached_hash_and_neg(semantic_hash) {
            return sdd;
        }

        let hash = self.hash_sdd(&or);
        unsafe {
            let tbl = &mut *self.sdd_tbl.as_ptr();
            SddPtr::Reg(tbl.get_or_insert_by_hash(hash, or, true))
        }
    }

    fn sdd_eq(&'a self, a: SddPtr<'a>, b: SddPtr<'a>) -> bool {
        let h1 = a.cached_semantic_hash(&self.vtree, &self.map);
        let h2 = b.cached_semantic_hash(&self.vtree, &self.map);
        h1 == h2
    }

    fn compress(&'a self, _node: &mut Vec<SddAnd<'a>>) {}

    fn canonicalize(&'a self, node: Vec<SddAnd<'a>>, table: VTreeIndex) -> SddPtr<'a> {
        self.unique_or(node, table)
    }

    fn stats(&self) -> SddBuilderStats {
        let mut s: HashSet<u128> = HashSet::new();
        let mut num_collisions = 0;
        for n in self.node_iter() {
            let h = n.cached_semantic_hash(&self.vtree, &self.map);
            if s.contains(&h.value()) {
                num_collisions += 1;
            }
            s.insert(h.value());
        }

        SddBuilderStats {
            app_cache_hits: self.bdd_tbl.borrow().hits() + self.sdd_tbl.borrow().hits(),
            app_cache_size: self.bdd_tbl.borrow().num_nodes() + self.sdd_tbl.borrow().num_nodes(),
            num_logically_redundant: num_collisions,
            num_recursive_calls: *self.num_recursive_calls.borrow(),
            num_compressions: 0,
            num_get_or_insert_bdd: *self.num_get_or_insert_bdd.borrow(),
            num_get_or_insert_sdd: *self.num_get_or_insert_sdd.borrow(),
        }
    }

    fn log_recursive_call(&self) {
        *self.num_recursive_calls.borrow_mut() += 1
    }
}

impl<'a, const P: u128> SemanticSddBuilder<'a, P> {
    pub fn new(vtree: VTree) -> Self {
        let vtree_man = VTreeManager::new(vtree.clone());
        let map = create_semantic_hash_map(vtree.num_vars());
        SemanticSddBuilder {
            should_compress: false,
            vtree: vtree_man,
            // ite_cache: RefCell::new(AllTable::new()),
            app_cache: RefCell::new(HashMap::new()),
            bdd_tbl: RefCell::new(BackedRobinhoodTable::new()),
            sdd_tbl: RefCell::new(BackedRobinhoodTable::new()),
            map,
            num_recursive_calls: RefCell::new(0),
            num_get_or_insert_bdd: RefCell::new(0),
            num_get_or_insert_sdd: RefCell::new(0),
        }
    }

    pub fn cached_semantic_hash(&self, sdd: SddPtr) -> FiniteField<P> {
        sdd.cached_semantic_hash(&self.vtree, &self.map)
    }

    pub fn map(&self) -> &WmcParams<FiniteField<P>> {
        &self.map
    }

    fn hash_bdd(&self, elem: &BinarySDD) -> u64 {
        let mut hasher = FxHasher::default();
        elem.semantic_hash(&self.vtree, &self.map)
            .value()
            .hash(&mut hasher);
        hasher.finish()
    }

    fn hash_sdd(&self, elem: &SddOr) -> u64 {
        let mut hasher = FxHasher::default();
        elem.semantic_hash(&self.vtree, &self.map)
            .value()
            .hash(&mut hasher);
        hasher.finish()
    }

    fn get_shared_sdd_ptr(&self, semantic_hash: FiniteField<P>, hash: u64) -> Option<SddPtr> {
        match semantic_hash.value() {
            0 => Some(SddPtr::PtrFalse),
            1 => Some(SddPtr::PtrTrue),
            _ => {
                unsafe {
                    let tbl = &mut *self.bdd_tbl.as_ptr();
                    if let Some(sdd) = tbl.get_by_hash(hash) {
                        return Some(SddPtr::BDD(sdd));
                    }
                    let tbl = &mut *self.sdd_tbl.as_ptr();
                    if let Some(sdd) = tbl.get_by_hash(hash) {
                        return Some(SddPtr::Reg(sdd));
                    }
                }
                None
            }
        }
    }

    fn check_cached_hash_and_neg(&self, semantic_hash: FiniteField<P>) -> Option<SddPtr> {
        // check regular hash
        let mut hasher = FxHasher::default();
        semantic_hash.value().hash(&mut hasher);
        let hash = hasher.finish();
        if let Some(sdd) = self.get_shared_sdd_ptr(semantic_hash, hash) {
            return Some(sdd);
        }

        // check negated hash
        let semantic_hash = semantic_hash.negate();
        let mut hasher = FxHasher::default();
        semantic_hash.value().hash(&mut hasher);
        let hash = hasher.finish();
        if let Some(sdd) = self.get_shared_sdd_ptr(semantic_hash, hash) {
            return Some(sdd.neg());
        }
        None
    }
}

#[test]
fn prob_equiv_sdd_demorgan() {
    use crate::builder::BottomUpBuilder;
    use crate::constants::primes;
    use crate::repr::var_label::VarLabel;
    use crate::util::semirings::FiniteField;

    let mut builder = SemanticSddBuilder::<{ primes::U32_SMALL }>::new(VTree::even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        1,
    ));
    builder.set_compression(false);
    let x = SddPtr::Var(VarLabel::new(0), true);
    let y = SddPtr::Var(VarLabel::new(3), true);
    let res = builder.or(x, y).neg();
    let expected = builder.and(x.neg(), y.neg());

    let map: WmcParams<FiniteField<{ primes::U32_SMALL }>> =
        create_semantic_hash_map(builder.num_vars());

    let sh1 = res.cached_semantic_hash(builder.vtree_manager(), &map);
    let sh2 = expected.cached_semantic_hash(builder.vtree_manager(), &map);

    assert!(sh1 == sh2, "Not eq:\nGot: {:?}\nExpected: {:?}", sh1, sh2);
}
