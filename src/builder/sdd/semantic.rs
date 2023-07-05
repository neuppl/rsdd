use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};

use rustc_hash::FxHasher;

use crate::backing_store::bump_table::BackedRobinhoodTable;
use crate::backing_store::UniqueTable;
use crate::builder::cache::ite::Ite;
use crate::repr::ddnnf::DDNNFPtr;
use crate::repr::robdd::create_semantic_hash_map;
use crate::repr::sdd::binary_sdd::BinarySDD;
use crate::repr::sdd::sdd_or::{SddAnd, SddOr};
use crate::repr::sdd::SddPtr;
use crate::repr::vtree::{VTree, VTreeIndex, VTreeManager};
use crate::repr::wmc::WmcParams;
use crate::util::semiring::FiniteField;

use super::builder::{SddBuilder, SddBuilderStats};

pub struct SemanticSddManager<'a, const P: u128> {
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
}

impl<'a, const P: u128> SddBuilder<'a> for SemanticSddManager<'a, P> {
    #[inline]
    fn get_vtree_manager(&self) -> &VTreeManager {
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
            num_logically_redundant: num_collisions,
        }
    }
}

impl<'a, const P: u128> SemanticSddManager<'a, P> {
    pub fn new(vtree: VTree) -> Self {
        let vtree_man = VTreeManager::new(vtree.clone());
        let map = create_semantic_hash_map(vtree.num_vars());
        SemanticSddManager {
            should_compress: false,
            vtree: vtree_man,
            // ite_cache: RefCell::new(AllTable::new()),
            app_cache: RefCell::new(HashMap::new()),
            bdd_tbl: RefCell::new(BackedRobinhoodTable::new()),
            sdd_tbl: RefCell::new(BackedRobinhoodTable::new()),
            map,
        }
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
    use crate::repr::robdd::create_semantic_hash_map;
    use crate::repr::robdd::WmcParams;
    use crate::repr::var_label::VarLabel;
    use crate::util::semiring::FiniteField;

    let mut man = SemanticSddManager::<100000049>::new(VTree::even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        1,
    ));
    man.set_compression(false);
    let x = SddPtr::Var(VarLabel::new(0), true);
    let y = SddPtr::Var(VarLabel::new(3), true);
    let res = man.or(x, y).neg();
    let expected = man.and(x.neg(), y.neg());

    let map: WmcParams<FiniteField<100000049>> = create_semantic_hash_map(man.num_vars());

    let sh1 = res.cached_semantic_hash(man.get_vtree_manager(), &map);
    let sh2 = expected.cached_semantic_hash(man.get_vtree_manager(), &map);

    assert!(sh1 == sh2, "Not eq:\nGot: {:?}\nExpected: {:?}", sh1, sh2);
}
