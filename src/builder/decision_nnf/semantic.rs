use crate::{
    backing_store::BackedRobinhoodTable,
    builder::decision_nnf::builder::{DecisionNNFBuilder, DecisionNNFBuilderStats},
    repr::{
        bdd::{BddNode, BddPtr},
        ddnnf::{create_semantic_hash_map, DDNNFPtr},
        var_order::VarOrder,
        wmc::WmcParams,
    },
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

        if let Some(bdd) = self.check_cached_hash_and_neg(semantic_hash, bdd.clone()) {
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
            BddPtr::Reg(tbl.sized_get_or_insert_by_hash(hash, bdd, true))
        }
    }

    fn num_logically_redundant(&self) -> usize {
        let mut num_collisions = 0;
        let mut seen_hashes = HashSet::new();
        let map = create_semantic_hash_map::<P>(self.order.num_vars());
        for bdd in self.compute_table.borrow().iter() {
            let h = BddPtr::Reg(bdd).semantic_hash(&self.order, &map);
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

    pub fn rebuild(&'a self, bdd: BddPtr) -> BddPtr {
        match bdd {
            BddPtr::PtrTrue => BddPtr::PtrTrue,
            BddPtr::PtrFalse => BddPtr::PtrFalse,
            BddPtr::Reg(node) => {
                let new_node =
                    BddNode::new(node.var, self.rebuild(node.low), self.rebuild(node.high));
                self.get_or_insert(new_node)
            }
            BddPtr::Compl(node) => self.rebuild(BddPtr::Reg(node)).neg(),
        }
    }

    fn check_cached_hash_and_neg(
        &'a self,
        semantic_hash: FiniteField<P>,
        bdd: BddNode<'a>,
    ) -> Option<BddPtr> {
        // check regular hash
        let mut hasher = FxHasher::default();
        semantic_hash.value().hash(&mut hasher);
        let hash = hasher.finish();
        unsafe {
            let tbl = &mut *self.compute_table.as_ptr();
            if let Some(bdd) = tbl.sized_get_by_hash(hash, bdd.clone()) {
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
            if let Some(bdd) = tbl.sized_get_by_hash(hash, bdd) {
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
        repr::{cnf::Cnf, ddnnf::DDNNFPtr, var_order::VarOrder},
    };

    #[test]
    fn trivial_evaluation_test() {
        static CNF: &str = "
        p cnf 2 1
        1 2 0
        ";

        let cnf = Cnf::from_file(String::from(CNF));

        let linear_order = VarOrder::linear_order(cnf.num_vars());

        let builder =
            SemanticDecisionNNFBuilder::<{ primes::U32_SMALL }>::new(linear_order.clone());
        let dnnf = builder.compile_cnf_topdown(&cnf);

        assert!(dnnf.evaluate(&linear_order, &[true, true]));
        assert!(dnnf.evaluate(&linear_order, &[false, true]));
        assert!(dnnf.evaluate(&linear_order, &[true, false]));
        assert!(!dnnf.evaluate(&linear_order, &[false, false]));
    }
}
