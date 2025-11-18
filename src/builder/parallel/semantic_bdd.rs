use crate::{
    builder::{
        bdd::BddBuilderStats,
        cache::{AllIteTable, Ite, IteTable},
        BottomUpBuilder,
    },
    repr::{
        create_semantic_hash_map, Cnf, DDNNFPtr, SemanticBddNode, SemanticBddPtr, VarLabel,
        VarOrder, WmcParams,
    },
    util::semirings::{FiniteField, Semiring},
};
use std::{cmp::Ordering, collections::HashMap, sync::RwLock};

#[derive(Debug)]
pub struct SemanticBddBuilder<'a, const P: u128> {
    compute_table: RwLock<HashMap<FiniteField<P>, SemanticBddNode<P>>>,
    apply_table: RwLock<AllIteTable<SemanticBddPtr<'a, P>>>,
    stats: RwLock<BddBuilderStats>,
    order: RwLock<VarOrder>,
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
        let bdd =
            SemanticBddNode::new_from_builder(label, FiniteField::zero(), FiniteField::one(), self);
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

    fn compile_cnf(&'a self, cnf: &Cnf) -> SemanticBddPtr<'a, P> {
        let mut cvec: Vec<SemanticBddPtr<P>> = Vec::with_capacity(cnf.clauses().len());
        if cnf.clauses().is_empty() {
            return SemanticBddPtr::true_ptr();
        }
        // check if there is an empty clause -- if so, UNSAT
        if cnf.clauses().iter().any(|x| x.is_empty()) {
            return SemanticBddPtr::false_ptr();
        }

        // sort the clauses based on a best-effort bottom-up ordering of clauses
        let mut cnf_sorted = cnf.clauses().to_vec();
        cnf_sorted.sort_by(|c1, c2| {
            // order the clause with the first-most variable last
            let fst1 = c1
                .iter()
                .max_by(|l1, l2| {
                    if self.less_than(l1.label(), l2.label()) {
                        Ordering::Less
                    } else {
                        Ordering::Equal
                    }
                })
                .unwrap();
            let fst2 = c2
                .iter()
                .max_by(|l1, l2| {
                    if self.less_than(l1.label(), l2.label()) {
                        Ordering::Less
                    } else {
                        Ordering::Equal
                    }
                })
                .unwrap();
            if self.less_than(fst1.label(), fst2.label()) {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        });

        for lit_vec in cnf_sorted.iter() {
            let (vlabel, val) = (lit_vec[0].label(), lit_vec[0].polarity());
            let mut bdd = self.var(vlabel, val);
            for lit in lit_vec {
                let (vlabel, val) = (lit.label(), lit.polarity());
                let var = self.var(vlabel, val);
                bdd = self.or(bdd, var);
            }
            cvec.push(bdd);
        }
        // now cvec has a list of all the clauses; collapse it down
        let r = self.collapse_clauses(&cvec);
        match r {
            None => SemanticBddPtr::true_ptr(),
            Some(x) => x,
        }
    }
}

impl<'a, const P: u128> SemanticBddBuilder<'a, P> {
    pub fn new(order: VarOrder) -> SemanticBddBuilder<'a, P> {
        let map = create_semantic_hash_map(order.num_vars());
        SemanticBddBuilder {
            compute_table: RwLock::new(HashMap::default()),
            order: RwLock::new(order),
            apply_table: RwLock::new(AllIteTable::default()),
            stats: RwLock::new(BddBuilderStats::new()),
            map,
        }
    }

    pub fn new_with_map(
        order: VarOrder,
        map: WmcParams<FiniteField<P>>,
    ) -> SemanticBddBuilder<'a, P> {
        SemanticBddBuilder {
            compute_table: RwLock::new(HashMap::default()),
            order: RwLock::new(order),
            apply_table: RwLock::new(AllIteTable::default()),
            stats: RwLock::new(BddBuilderStats::new()),
            map,
        }
    }

    pub fn map(&'a self) -> &WmcParams<FiniteField<P>> {
        &self.map
    }

    pub fn deref_semantic_node(
        &'a self,
        semantic_hash: &FiniteField<P>,
    ) -> Option<SemanticBddNode<P>> {
        if let Some(bdd) = self.compute_table.read().unwrap().get(semantic_hash) {
            return Some(bdd.clone());
        }

        // check negated hash
        let semantic_hash = semantic_hash.negate();
        if let Some(bdd) = self.compute_table.read().unwrap().get(&semantic_hash) {
            return Some(bdd.clone());
        }
        None
    }

    pub fn deref_semantic_hash(&'a self, hash: &FiniteField<P>) -> SemanticBddPtr<'a, P> {
        self.check_cached_hash_and_neg(hash)
            .unwrap_or_else(|| panic!("Could not find item for hash: {}.", hash))
    }

    fn less_than(&self, a: VarLabel, b: VarLabel) -> bool {
        self.order.read().unwrap().lt(a, b)
    }

    fn ite_helper(
        &'a self,
        f: SemanticBddPtr<'a, P>,
        g: SemanticBddPtr<'a, P>,
        h: SemanticBddPtr<'a, P>,
    ) -> SemanticBddPtr<'a, P> {
        self.stats.write().unwrap().num_recursive_calls += 1;
        let o = |a: SemanticBddPtr<P>, b: SemanticBddPtr<P>| match (a, b) {
            (SemanticBddPtr::PtrTrue, _) | (SemanticBddPtr::PtrFalse, _) => true,
            (_, SemanticBddPtr::PtrTrue) | (_, SemanticBddPtr::PtrFalse) => false,
            (
                SemanticBddPtr::Reg(ff_a, _) | SemanticBddPtr::Compl(ff_a, _),
                SemanticBddPtr::Reg(ff_b, _) | SemanticBddPtr::Compl(ff_b, _),
            ) => {
                let node_a = self.deref_semantic_node(&ff_a).unwrap();
                let node_b = self.deref_semantic_node(&ff_b).unwrap();
                self.less_than(node_a.var(), node_b.var())
            }
        };

        let ite = Ite::new(o, f, g, h);

        if let Ite::IteConst(f) = ite {
            return f;
        }

        let hash = self.apply_table.read().unwrap().hash(&ite);
        if let Some(v) = self.apply_table.read().unwrap().get(ite, hash) {
            return v;
        }

        // ok the work!
        // find the first essential variable for f, g, or h
        let lbl = self.order.read().unwrap().first_essential(&f, &g, &h);
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
        let node =
            SemanticBddNode::new_from_builder(lbl, f.semantic_hash(), t.semantic_hash(), self);
        let r = self.get_or_insert(node);
        self.apply_table.write().unwrap().insert(ite, r, hash);
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
            SemanticBddPtr::Reg(semantic_hash, _) | SemanticBddPtr::Compl(semantic_hash, _) => {
                let node = self.deref_semantic_node(&semantic_hash).unwrap();
                if node.var() != lbl {
                    return f;
                }
                let r = if v { node.high(self) } else { node.low(self) };
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
        self.stats.write().unwrap().num_recursive_calls += 1;
        match bdd {
            SemanticBddPtr::PtrTrue | SemanticBddPtr::PtrFalse => bdd,
            SemanticBddPtr::Reg(semantic_hash, _) | SemanticBddPtr::Compl(semantic_hash, _) => {
                let node = self.deref_semantic_node(&semantic_hash).unwrap();
                if self.order.read().unwrap().lt(lbl, node.var()) {
                    // we passed the variable in the order, we will never find it
                    return bdd;
                }

                if node.var() == lbl {
                    let r = if value {
                        node.high(self)
                    } else {
                        node.low(self)
                    };
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
                let l = self.cond_with_alloc(node.low(self), lbl, value, alloc);
                let h = self.cond_with_alloc(node.high(self), lbl, value, alloc);

                if l == h {
                    // reduce the BDD -- two children identical
                    if bdd.is_neg() {
                        return l.neg();
                    } else {
                        return l;
                    };
                };
                let res = if l != node.low(self) || h != node.high(self) {
                    // cache and return the new BDD
                    let new_bdd = SemanticBddNode::new_from_builder(
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

    fn collapse_clauses(&'a self, vec: &[SemanticBddPtr<'a, P>]) -> Option<SemanticBddPtr<'a, P>> {
        if vec.is_empty() {
            None
        } else if vec.len() == 1 {
            Some(vec[0])
        } else {
            let (l, r) = vec.split_at(vec.len() / 2);
            let sub_l = self.collapse_clauses(l);
            let sub_r = self.collapse_clauses(r);
            match (sub_l, sub_r) {
                (None, None) => None,
                (Some(v), None) | (None, Some(v)) => Some(v),
                (Some(l), Some(r)) => Some(self.and(l, r)),
            }
        }
    }

    fn get_bdd_ptr(&'a self, semantic_hash: &FiniteField<P>) -> Option<SemanticBddPtr<'a, P>> {
        match semantic_hash.value() {
            0 => Some(SemanticBddPtr::PtrFalse),
            1 => Some(SemanticBddPtr::PtrTrue),
            _ => {
                if self
                    .compute_table
                    .read()
                    .unwrap()
                    .get(semantic_hash)
                    .is_some()
                {
                    return Some(SemanticBddPtr::Reg(*semantic_hash, self));
                }
                None
            }
        }
    }

    fn check_cached_hash_and_neg(
        &'a self,
        semantic_hash: &FiniteField<P>,
    ) -> Option<SemanticBddPtr<'a, P>> {
        // check regular hash
        if let Some(bdd) = self.get_bdd_ptr(semantic_hash) {
            return Some(bdd);
        }

        // check negated hash
        let semantic_hash = semantic_hash.negate();
        if let Some(bdd) = self.get_bdd_ptr(&semantic_hash) {
            return Some(bdd.neg());
        }
        None
    }

    // Normalizes and fetches a node from the store
    fn get_or_insert(&'a self, bdd: SemanticBddNode<P>) -> SemanticBddPtr<'a, P> {
        if let Some(ptr) = self.check_cached_hash_and_neg(&bdd.semantic_hash()) {
            return ptr;
        }

        let semantic_hash = bdd.semantic_hash();

        self.compute_table
            .write()
            .unwrap()
            .insert(semantic_hash, bdd);

        SemanticBddPtr::Reg(semantic_hash, self)
    }

    pub fn merge_from<'b>(
        &'a self,
        other: &'b Self,
        ptrs: &[SemanticBddPtr<'b, P>],
    ) -> Vec<SemanticBddPtr<'a, P>> {
        let new_roots = ptrs
            .iter()
            .map(|ptr| match *ptr {
                SemanticBddPtr::PtrTrue => SemanticBddPtr::PtrTrue,
                SemanticBddPtr::PtrFalse => SemanticBddPtr::PtrFalse,
                SemanticBddPtr::Reg(node, _) => SemanticBddPtr::Reg(node, self),
                SemanticBddPtr::Compl(node, _) => SemanticBddPtr::Compl(node, self),
            })
            .collect();

        for (k, v) in other.compute_table.read().unwrap().iter() {
            self.compute_table.write().unwrap().insert(*k, v.clone());
        }

        for (k, v) in other.apply_table.read().unwrap().iter() {
            self.apply_table.write().unwrap().insert_directly(*k, *v);
        }

        new_roots
    }
}

unsafe impl<const P: u128> Send for SemanticBddBuilder<'_, P> {}
unsafe impl<const P: u128> Sync for SemanticBddBuilder<'_, P> {}

#[cfg(test)]
mod test {
    use crate::{
        builder::BottomUpBuilder,
        constants::primes,
        repr::{VarLabel, VarOrder},
    };

    use super::SemanticBddBuilder;

    #[test]
    fn trivial_semantic_builder() {
        let order = VarOrder::linear_order(2);
        let builder: SemanticBddBuilder<'_, { primes::U64_LARGEST }> =
            SemanticBddBuilder::new(order);

        println!("{:?}", builder.map());

        let v1 = builder.var(VarLabel::new(0), true);
        let v2 = builder.var(VarLabel::new(1), true);
        let r1 = builder.or(v1, v2);
        let r2 = builder.and(r1, v1);

        assert!(builder.eq(v1, r2), "Not eq:\n {:?}\n{:?}", v1, r2);
    }

    #[test]
    fn e2e_merge() {
        let order = VarOrder::linear_order(2);
        let builder: SemanticBddBuilder<'_, { primes::U64_LARGEST }> =
            SemanticBddBuilder::new(order);

        println!("{:?}", builder.map());

        let v1 = builder.var(VarLabel::new(0), true);
        let v2 = builder.var(VarLabel::new(1), true);
        let r1 = builder.or(v1, v2);
        let r2 = builder.and(r1, v1);

        let order = VarOrder::linear_order(2);
        let builder2: SemanticBddBuilder<'_, { primes::U64_LARGEST }> =
            SemanticBddBuilder::new_with_map(order, builder.map().clone());

        let v3 = builder2.var(VarLabel::new(0), true);
        let v4 = builder2.var(VarLabel::new(1), true);
        let r3 = builder2.and(v3, v4);
        let r4 = builder2.and(r3, v3);

        // this should always be true...
        assert!(
            builder.eq(v1, r2),
            "Invariant, pre-merge: Not eq:\n {:?}\n{:?}",
            v1,
            r2
        );
        assert!(
            builder2.eq(r3, r4),
            "Invariant, pre-merge: Not eq:\n {:?}\n{:?}",
            r3,
            r4
        );

        println!("starting merge...");

        let res = builder.merge_from(&builder2, &[r3, r4]);

        println!("merge done...");

        // and still be true *after* the merge
        assert!(
            builder.eq(v1, r2),
            "Invariant, post-merge: Not eq:\n {:?}\n{:?}",
            v1,
            r2
        );
        assert!(
            builder.eq(res[0], res[1]),
            "Invariant, post-merge: Not eq:\n {:?}\n{:?}",
            res[0],
            res[1]
        );
    }
}
