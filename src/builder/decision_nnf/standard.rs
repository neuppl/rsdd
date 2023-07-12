use std::{cell::RefCell, collections::HashSet};

use crate::{
    backing_store::bump_table::BackedRobinhoodTable,
    builder::{
        bdd::robdd::{BddPtr, DDNNFPtr},
        decision_nnf::builder::{DecisionNNFBuilder, DecisionNNFBuilderStats},
    },
    constants::primes,
    repr::bdd::{create_semantic_hash_map, BddNode, VarOrder},
};

use crate::backing_store::UniqueTable;

pub struct StandardDecisionNNFBuilder<'a> {
    compute_table: RefCell<BackedRobinhoodTable<'a, BddNode<'a>>>,
    order: VarOrder,
}

impl<'a> DecisionNNFBuilder<'a> for StandardDecisionNNFBuilder<'a> {
    fn order(&'a self) -> &'a VarOrder {
        &self.order
    }

    fn get_or_insert(&'a self, bdd: BddNode<'a>) -> BddPtr<'a> {
        // TODO make this safe
        unsafe {
            let tbl = &mut *self.compute_table.as_ptr();
            if bdd.high.is_neg() {
                let bdd = BddNode::new(bdd.var, bdd.low.neg(), bdd.high.neg());
                BddPtr::new_compl(tbl.get_or_insert(bdd))
            } else {
                let bdd = BddNode::new(bdd.var, bdd.low, bdd.high);
                BddPtr::new_reg(tbl.get_or_insert(bdd))
            }
        }
    }

    fn num_logically_redundant(&self) -> usize {
        let mut num_collisions = 0;
        let mut seen_hashes = HashSet::new();
        let map = create_semantic_hash_map::<{ primes::U32_SMALL }>(self.order.num_vars());
        for bdd in self.compute_table.borrow().iter() {
            let h = BddPtr::new_reg(bdd).semantic_hash(&self.order, &map);
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

impl<'a> StandardDecisionNNFBuilder<'a> {
    pub fn new(order: VarOrder) -> StandardDecisionNNFBuilder<'a> {
        StandardDecisionNNFBuilder {
            order,
            compute_table: RefCell::new(BackedRobinhoodTable::new()),
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        builder::{
            bdd::robdd::DDNNFPtr,
            decision_nnf::{builder::DecisionNNFBuilder, standard::StandardDecisionNNFBuilder},
        },
        repr::{bdd::VarOrder, cnf::Cnf},
    };

    #[test]
    fn trivial_evaluation_test() {
        static CNF: &str = "
        p cnf 2 1
        1 2 0
        ";

        let cnf = Cnf::from_file(String::from(CNF));

        let linear_order = VarOrder::linear_order(cnf.num_vars());

        let builder = StandardDecisionNNFBuilder::new(linear_order.clone());
        let dnnf = builder.compile_cnf_topdown(&cnf);

        assert!(dnnf.evaluate(&linear_order, &[true, true]));
        assert!(dnnf.evaluate(&linear_order, &[false, true]));
        assert!(dnnf.evaluate(&linear_order, &[true, false]));
        assert!(!dnnf.evaluate(&linear_order, &[false, false]));
    }
}
