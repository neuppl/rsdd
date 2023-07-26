use rustc_hash::FxHasher;

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
        wmc::WmcParams,
    },
    util::semirings::{FiniteField, Semiring},
};
use std::{
    cell::RefCell,
    hash::{Hash, Hasher},
};

#[derive(Debug)]
pub struct SemanticBddBuilder<'a, const P: u128> {
    compute_table: RefCell<BackedRobinhoodTable<'a, SemanticBddNode<'a, P>>>,
    apply_table: RefCell<AllIteTable<SemanticBddPtr<'a, P>>>,
    stats: RefCell<BddBuilderStats>,
    order: RefCell<VarOrder>,
    map: WmcParams<FiniteField<P>>,
}

impl<'a, const P: u128> BottomUpBuilder<'a, SemanticBddPtr<'a, P>> for SemanticBddBuilder<'a, P> {
    fn true_ptr(&self) -> SemanticBddPtr<'a, P> {
        SemanticBddPtr::PtrTrue
    }

    fn false_ptr(&self) -> SemanticBddPtr<'a, P> {
        SemanticBddPtr::PtrFalse
    }

    fn var(&'a self, label: VarLabel, polarity: bool) -> SemanticBddPtr<'a, P> {
        let bdd = SemanticBddNode::new(label, FiniteField::zero(), FiniteField::one(), self);
        let r = self.get_or_insert(bdd);
        if polarity {
            r
        } else {
            r.neg()
        }
    }

    fn eq(&'a self, a: SemanticBddPtr<'a, P>, b: SemanticBddPtr<'a, P>) -> bool {
        a.semantic_hash() == b.semantic_hash()
    }

    fn and(&'a self, a: SemanticBddPtr<'a, P>, b: SemanticBddPtr<'a, P>) -> SemanticBddPtr<'a, P> {
        self.ite(a, b, SemanticBddPtr::false_ptr())
    }

    fn negate(&'a self, f: SemanticBddPtr<'a, P>) -> SemanticBddPtr<'a, P> {
        f.neg()
    }

    fn ite(
        &'a self,
        f: SemanticBddPtr<'a, P>,
        g: SemanticBddPtr<'a, P>,
        h: SemanticBddPtr<'a, P>,
    ) -> SemanticBddPtr<'a, P> {
        self.ite_helper(f, g, h)
    }

    fn iff(&'a self, a: SemanticBddPtr<'a, P>, b: SemanticBddPtr<'a, P>) -> SemanticBddPtr<'a, P> {
        self.ite(a, b, b.neg())
    }

    fn xor(&'a self, a: SemanticBddPtr<'a, P>, b: SemanticBddPtr<'a, P>) -> SemanticBddPtr<'a, P> {
        self.ite(a, b.neg(), b)
    }

    fn exists(&'a self, bdd: SemanticBddPtr<'a, P>, lbl: VarLabel) -> SemanticBddPtr<'a, P> {
        let v1 = self.condition(bdd, lbl, true);
        let v2 = self.condition(bdd, lbl, false);
        self.or(v1, v2)
    }

    /// Compute the Boolean function `f | var = value`
    fn condition(
        &'a self,
        bdd: SemanticBddPtr<'a, P>,
        lbl: VarLabel,
        value: bool,
    ) -> SemanticBddPtr<'a, P> {
        self.cond_with_alloc(bdd, lbl, value, &mut Vec::new())
    }

    fn compile_cnf(&'a self, cnf: &crate::repr::cnf::Cnf) -> SemanticBddPtr<'a, P> {
        todo!()
    }
}

impl<'a, const P: u128> SemanticBddBuilder<'a, P> {
    pub fn map(&'a self) -> &WmcParams<FiniteField<P>> {
        &self.map
    }

    pub fn deref_semantic_hash(&'a self, hash: FiniteField<P>) -> SemanticBddPtr<'a, P> {
        self.get_bdd_ptr(hash, self.hash_field(hash)).unwrap()
    }

    fn less_than(&self, a: VarLabel, b: VarLabel) -> bool {
        self.order.borrow().lt(a, b)
    }

    fn ite_helper(
        &'a self,
        f: SemanticBddPtr<'a, P>,
        g: SemanticBddPtr<'a, P>,
        h: SemanticBddPtr<'a, P>,
    ) -> SemanticBddPtr<'a, P> {
        self.stats.borrow_mut().num_recursive_calls += 1;
        let o = |a: SemanticBddPtr<P>, b: SemanticBddPtr<P>| match (a, b) {
            (SemanticBddPtr::PtrTrue, _) | (SemanticBddPtr::PtrFalse, _) => true,
            (_, SemanticBddPtr::PtrTrue) | (_, SemanticBddPtr::PtrFalse) => false,
            (
                SemanticBddPtr::Reg(node_a) | SemanticBddPtr::Compl(node_a),
                SemanticBddPtr::Reg(node_b) | SemanticBddPtr::Compl(node_b),
            ) => self.less_than(node_a.var(), node_b.var()),
        };

        let ite = Ite::new(o, f, g, h);

        if let Ite::IteConst(f) = ite {
            return f;
        }

        let hash = self.apply_table.borrow().hash(&ite);
        if let Some(v) = self.apply_table.borrow().get(ite, hash) {
            return v;
        }

        // ok the work!
        // find the first essential variable for f, g, or h
        let lbl = self.order.borrow().first_essential(f, g, h);
        let fx = self.condition_essential(f, lbl, true);
        let gx = self.condition_essential(g, lbl, true);
        let hx = self.condition_essential(h, lbl, true);
        let fxn = self.condition_essential(f, lbl, false);
        let gxn = self.condition_essential(g, lbl, false);
        let hxn = self.condition_essential(h, lbl, false);
        let t = self.ite(fx, gx, hx);
        let f = self.ite(fxn, gxn, hxn);

        if t == f {
            return t;
        };

        // now we have a new BDD
        let node = SemanticBddNode::new(lbl, f.semantic_hash(), t.semantic_hash(), self);
        let r = self.get_or_insert(node);
        self.apply_table.borrow_mut().insert(ite, r, hash);
        r
    }

    // condition a BDD *only* if the top variable is `v`; used in `ite`
    fn condition_essential(
        &'a self,
        f: SemanticBddPtr<'a, P>,
        lbl: VarLabel,
        v: bool,
    ) -> SemanticBddPtr<'a, P> {
        match f {
            SemanticBddPtr::PtrTrue | SemanticBddPtr::PtrFalse => f,
            SemanticBddPtr::Reg(node) | SemanticBddPtr::Compl(node) => {
                if node.var() != lbl {
                    return f;
                }
                let r = if v { node.high() } else { node.low() };
                if f.is_neg() {
                    r.neg()
                } else {
                    r
                }
            }
        }
    }

    fn cond_with_alloc(
        &'a self,
        bdd: SemanticBddPtr<'a, P>,
        lbl: VarLabel,
        value: bool,
        alloc: &mut Vec<SemanticBddPtr<'a, P>>,
    ) -> SemanticBddPtr<'a, P> {
        self.stats.borrow_mut().num_recursive_calls += 1;
        match bdd {
            SemanticBddPtr::PtrTrue | SemanticBddPtr::PtrFalse => bdd,
            SemanticBddPtr::Reg(node) | SemanticBddPtr::Compl(node) => {
                if self.order.borrow().lt(lbl, node.var()) {
                    // we passed the variable in the order, we will never find it
                    return bdd;
                }

                if node.var() == lbl {
                    let r = if value { node.high() } else { node.low() };
                    return if bdd.is_neg() { r.neg() } else { r };
                }

                // check cache
                match bdd.scratch::<usize>() {
                    None => (),
                    Some(v) => {
                        return if bdd.is_neg() {
                            alloc[v].neg()
                        } else {
                            alloc[v]
                        }
                    }
                };

                // recurse on the children
                let l = self.cond_with_alloc(node.low(), lbl, value, alloc);
                let h = self.cond_with_alloc(node.high(), lbl, value, alloc);

                if l == h {
                    // reduce the BDD -- two children identical
                    if bdd.is_neg() {
                        return l.neg();
                    } else {
                        return l;
                    };
                };
                let res = if l != node.low() || h != node.high() {
                    // cache and return the new BDD
                    let new_bdd = SemanticBddNode::new(
                        node.var(),
                        l.semantic_hash(),
                        h.semantic_hash(),
                        self,
                    );
                    let r = self.get_or_insert(new_bdd);
                    if bdd.is_neg() {
                        r.neg()
                    } else {
                        r
                    }
                } else {
                    // nothing changed
                    bdd
                };

                let idx = if bdd.is_neg() {
                    alloc.push(res.neg());
                    alloc.len() - 1
                } else {
                    alloc.push(res);
                    alloc.len() - 1
                };
                bdd.set_scratch(idx);
                res
            }
        }
    }

    fn get_bdd_ptr(
        &self,
        semantic_hash: FiniteField<P>,
        hash: u64,
    ) -> Option<SemanticBddPtr<'a, P>> {
        match semantic_hash.value() {
            0 => Some(SemanticBddPtr::PtrFalse),
            1 => Some(SemanticBddPtr::PtrTrue),
            _ => unsafe {
                let tbl = &mut *self.compute_table.as_ptr();
                if let Some(node) = tbl.get_by_hash(hash) {
                    return Some(SemanticBddPtr::Reg(node));
                }
                None
            },
        }
    }

    fn check_cached_hash_and_neg(
        &self,
        semantic_hash: FiniteField<P>,
    ) -> Option<SemanticBddPtr<'a, P>> {
        // check regular hash
        let mut hasher = FxHasher::default();
        semantic_hash.value().hash(&mut hasher);
        let hash = hasher.finish();
        if let Some(bdd) = self.get_bdd_ptr(semantic_hash, hash) {
            return Some(bdd);
        }

        // check negated hash
        let semantic_hash = semantic_hash.negate();
        let mut hasher = FxHasher::default();
        semantic_hash.value().hash(&mut hasher);
        let hash = hasher.finish();
        if let Some(bdd) = self.get_bdd_ptr(semantic_hash, hash) {
            return Some(bdd.neg());
        }
        None
    }

    // Normalizes and fetches a node from the store
    fn get_or_insert(&'a self, bdd: SemanticBddNode<'a, P>) -> SemanticBddPtr<'a, P> {
        if let Some(ptr) = self.check_cached_hash_and_neg(bdd.semantic_hash()) {
            return ptr;
        }

        let hash = self.hash_field(bdd.semantic_hash());
        unsafe {
            let tbl = &mut *self.compute_table.as_ptr();
            SemanticBddPtr::Reg(tbl.get_or_insert_by_hash(hash, bdd, true))
        }
    }

    fn hash_field(&'a self, field: FiniteField<P>) -> u64 {
        let mut hasher = FxHasher::default();
        field.value().hash(&mut hasher);
        hasher.finish()
    }
}
