use crate::{
    backing_store::BackedRobinhoodTable,
    builder::decision_nnf::builder::{DecisionNNFBuilder, DecisionNNFBuilderStats},
    repr::{create_semantic_hash_map, BddNode, BddPtr, DDNNFPtr, VarOrder, WmcParams},
    util::semirings::FiniteField,
};
use rustc_hash::FxHasher;
use std::{
    cell::RefCell,
    collections::HashSet,
    hash::{Hash, Hasher},
};

pub struct SemanticDecisionNNFBuilder<'a, const P: u128> {
    compute_table: RefCell<BackedRobinhoodTable<'a, BddNode<'a>>>,
    order: VarOrder,
    // semantic hashing
    map: WmcParams<FiniteField<P>>,
}

impl<'a, const P: u128> DecisionNNFBuilder<'a> for SemanticDecisionNNFBuilder<'a, P> {
    fn order(&'a self) -> &'a VarOrder {
        &self.order
    }

    fn get_or_insert(&'a self, bdd: BddNode<'a>) -> BddPtr<'a> {
        let semantic_hash = bdd.semantic_hash(&self.order, &self.map);

        if let Some(bdd) = self.check_cached_hash_and_neg(semantic_hash) {
            return bdd;
        }

        let hash = {
            let mut hasher = FxHasher::default();
            bdd.semantic_hash(&self.order, &self.map)
                .value()
                .hash(&mut hasher);
            hasher.finish()
        };

        unsafe {
            let tbl = &mut *self.compute_table.as_ptr();
            BddPtr::Reg(tbl.get_or_insert_by_hash(hash, bdd, true))
        }
    }

    fn num_logically_redundant(&self) -> usize {
        let mut num_collisions = 0;
        let mut seen_hashes = HashSet::new();
        let map = create_semantic_hash_map::<P>(self.order.num_vars());
        for bdd in self.compute_table.borrow().iter() {
            let h = BddPtr::Reg(bdd).semantic_hash(&map);
            if seen_hashes.contains(&(h.value())) {
                num_collisions += 1;
            } else {
                seen_hashes.insert(h.value());
            }
        }
        num_collisions
    }

    fn stats(&self) -> DecisionNNFBuilderStats {
        DecisionNNFBuilderStats {
            num_nodes_alloc: self.compute_table.borrow().num_nodes(),
        }
    }
}

impl<'a, const P: u128> SemanticDecisionNNFBuilder<'a, P> {
    pub fn new(order: VarOrder) -> SemanticDecisionNNFBuilder<'a, P> {
        SemanticDecisionNNFBuilder {
            map: create_semantic_hash_map(order.num_vars()),
            order,
            compute_table: RefCell::new(BackedRobinhoodTable::new()),
        }
    }

    fn check_cached_hash_and_neg(&self, semantic_hash: FiniteField<P>) -> Option<BddPtr> {
        // check regular hash
        let mut hasher = FxHasher::default();
        semantic_hash.value().hash(&mut hasher);
        let hash = hasher.finish();
        unsafe {
            let tbl = &mut *self.compute_table.as_ptr();
            if let Some(bdd) = tbl.get_by_hash(hash) {
                return Some(BddPtr::Reg(bdd));
            }
        }

        // check negated hash
        let semantic_hash = semantic_hash.negate();
        let mut hasher = FxHasher::default();
        semantic_hash.value().hash(&mut hasher);
        let hash = hasher.finish();
        unsafe {
            let tbl = &mut *self.compute_table.as_ptr();
            if let Some(bdd) = tbl.get_by_hash(hash) {
                return Some(BddPtr::Compl(bdd));
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        builder::decision_nnf::{
            builder::DecisionNNFBuilder, semantic::SemanticDecisionNNFBuilder,
        },
        constants::primes,
        repr::{Cnf, DDNNFPtr, VarOrder},
    };

    #[test]
    fn trivial_evaluation_test() {
        static CNF: &str = "
        p cnf 2 1
        1 2 0
        ";

        let cnf = Cnf::from_dimacs(CNF);

        let linear_order = VarOrder::linear_order(cnf.num_vars());

        let builder = SemanticDecisionNNFBuilder::<{ primes::U32_SMALL }>::new(linear_order);
        let dnnf = builder.compile_cnf_topdown(&cnf);

        assert!(dnnf.evaluate(&[true, true]));
        assert!(dnnf.evaluate(&[false, true]));
        assert!(dnnf.evaluate(&[true, false]));
        assert!(!dnnf.evaluate(&[false, false]));
    }
}
