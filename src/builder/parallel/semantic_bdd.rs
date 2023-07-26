use crate::{
    backing_store::{BackedRobinhoodTable, UniqueTable},
    builder::{
        bdd::{BddBuilder, BddBuilderStats},
        cache::{AllIteTable, Ite, IteTable},
        BottomUpBuilder,
    },
    repr::{
        ddnnf::DDNNFPtr,
        model::PartialModel,
        semantic_bdd::{SemanticBddNode, SemanticBddPtr},
        var_label::VarLabel,
        var_order::VarOrder,
    },
    util::semirings::FiniteField,
};
use std::cell::RefCell;

#[derive(Debug)]
pub struct SemanticBddBuilder<'a, const P: u128> {
    compute_table: RefCell<BackedRobinhoodTable<'a, SemanticBddNode<'a, P>>>,
    apply_table: RefCell<AllIteTable<SemanticBddPtr<'a, P>>>,
    stats: RefCell<BddBuilderStats>,
    order: RefCell<VarOrder>,
}

impl<'a, const P: u128> BottomUpBuilder<'a, SemanticBddPtr<'a, P>> for SemanticBddBuilder<'a, P> {
    fn true_ptr(&self) -> SemanticBddPtr<'a, P> {
        SemanticBddPtr::PtrTrue
    }

    fn false_ptr(&self) -> SemanticBddPtr<'a, P> {
        SemanticBddPtr::PtrFalse
    }

    fn var(&'a self, label: VarLabel, polarity: bool) -> SemanticBddPtr<'a, P> {
        todo!()
    }

    fn eq(&'a self, a: SemanticBddPtr<'a, P>, b: SemanticBddPtr<'a, P>) -> bool {
        a.semantic_hash() == b.semantic_hash()
    }

    fn and(&'a self, a: SemanticBddPtr<'a, P>, b: SemanticBddPtr<'a, P>) -> SemanticBddPtr<'a, P> {
        todo!()
    }

    fn negate(&'a self, f: SemanticBddPtr<'a, P>) -> SemanticBddPtr<'a, P> {
        todo!()
    }

    fn ite(
        &'a self,
        f: SemanticBddPtr<'a, P>,
        g: SemanticBddPtr<'a, P>,
        h: SemanticBddPtr<'a, P>,
    ) -> SemanticBddPtr<'a, P> {
        todo!()
    }

    fn iff(&'a self, a: SemanticBddPtr<'a, P>, b: SemanticBddPtr<'a, P>) -> SemanticBddPtr<'a, P> {
        todo!()
    }

    fn xor(&'a self, a: SemanticBddPtr<'a, P>, b: SemanticBddPtr<'a, P>) -> SemanticBddPtr<'a, P> {
        todo!()
    }

    fn exists(&'a self, f: SemanticBddPtr<'a, P>, v: VarLabel) -> SemanticBddPtr<'a, P> {
        todo!()
    }

    fn condition(
        &'a self,
        a: SemanticBddPtr<'a, P>,
        v: VarLabel,
        value: bool,
    ) -> SemanticBddPtr<'a, P> {
        todo!()
    }

    fn compose(
        &'a self,
        f: SemanticBddPtr<'a, P>,
        lbl: VarLabel,
        g: SemanticBddPtr<'a, P>,
    ) -> SemanticBddPtr<'a, P> {
        todo!()
    }

    fn compile_cnf(&'a self, cnf: &crate::repr::cnf::Cnf) -> SemanticBddPtr<'a, P> {
        todo!()
    }
}

impl<'a, const P: u128> SemanticBddBuilder<'a, P> {
    pub fn deref_semantic_hash(&'a self, hash: FiniteField<P>) -> SemanticBddPtr<'a, P> {
        todo!()
    }
}
