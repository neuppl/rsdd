use std::{cell::RefCell, collections::HashSet};

use crate::{
    backing_store::bump_table::BackedRobinhoodTable,
    builder::{
        bdd::robdd::{BddPtr, DDNNFPtr},
        decision_nnf::builder::DecisionNNFBuilder,
    },
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
        let map = create_semantic_hash_map::<10000000049>(self.order.num_vars());
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
    use std::collections::HashMap;

    use crate::{
        builder::{
            bdd::robdd::{BddPtr, DDNNFPtr, VarLabel},
            decision_nnf::{builder::DecisionNNFBuilder, standard::StandardDecisionNNFBuilder},
        },
        repr::{
            bdd::{VarOrder, WmcParams},
            cnf::Cnf,
        },
        util::semirings::{boolean::BooleanSemiring, semiring_traits::Semiring},
    };

    fn gen_bs_mappings(
        instantations: &[bool],
    ) -> HashMap<VarLabel, (BooleanSemiring, BooleanSemiring)> {
        HashMap::from_iter(instantations.iter().enumerate().map(|(index, polarity)| {
            (
                VarLabel::new(index as u64),
                (BooleanSemiring(!polarity), BooleanSemiring(*polarity)),
            )
        }))
    }

    fn bs_wmc(dnnf: &BddPtr, instantations: &[bool], order: &VarOrder) -> bool {
        dnnf.evaluate(
            order,
            &WmcParams::new_with_default(
                BooleanSemiring::zero(),
                BooleanSemiring::one(),
                gen_bs_mappings(instantations),
            ),
        )
        .0
    }

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

        assert!(bs_wmc(&dnnf, &[true, true], &linear_order));
        assert!(bs_wmc(&dnnf, &[true, false], &linear_order));
        assert!(bs_wmc(&dnnf, &[false, true], &linear_order));
        assert!(!bs_wmc(&dnnf, &[false, false], &linear_order));
    }
}
