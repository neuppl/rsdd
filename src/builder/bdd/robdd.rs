use crate::{
    backing_store::{BackedRobinhoodTable, UniqueTable},
    builder::{
        bdd::{BddBuilder, BddBuilderStats},
        cache::{Ite, IteTable},
        BottomUpBuilder,
    },
    repr::{BddNode, BddPtr, DDNNFPtr, PartialModel, VarLabel, VarOrder},
};
use std::cell::RefCell;

pub struct RobddBuilder<'a, T: IteTable<'a, BddPtr<'a>> + Default> {
    compute_table: RefCell<BackedRobinhoodTable<'a, BddNode<'a>>>,
    apply_table: RefCell<T>,
    stats: RefCell<BddBuilderStats>,
    order: RefCell<VarOrder>,
}

impl<'a, T: IteTable<'a, BddPtr<'a>> + Default> BddBuilder<'a> for RobddBuilder<'a, T> {
    fn less_than(&self, a: VarLabel, b: VarLabel) -> bool {
        self.order.borrow().lt(a, b)
    }

    /// Normalizes and fetches a node from the store
    fn get_or_insert(&'a self, bdd: BddNode<'a>) -> BddPtr<'a> {
        unsafe {
            // TODO: Make this safe if possible
            let tbl = &mut *self.compute_table.as_ptr();
            if bdd.high.is_neg() || bdd.high.is_false() {
                let bdd: BddNode<'a> = BddNode::new(bdd.var, bdd.low.neg(), bdd.high.neg());
                let r: &'a BddNode<'a> = tbl.get_or_insert(bdd);
                BddPtr::Compl(r)
            } else {
                let bdd = BddNode::new(bdd.var, bdd.low, bdd.high);
                BddPtr::Reg(tbl.get_or_insert(bdd))
            }
        }
    }

    fn ite_helper(&'a self, f: BddPtr<'a>, g: BddPtr<'a>, h: BddPtr<'a>) -> BddPtr<'a> {
        self.stats.borrow_mut().num_recursive_calls += 1;
        let o = |a: BddPtr, b: BddPtr| match (a, b) {
            (BddPtr::PtrTrue, _) | (BddPtr::PtrFalse, _) => true,
            (_, BddPtr::PtrTrue) | (_, BddPtr::PtrFalse) => false,
            (
                BddPtr::Reg(node_a) | BddPtr::Compl(node_a),
                BddPtr::Reg(node_b) | BddPtr::Compl(node_b),
            ) => self.order.borrow().lt(node_a.var, node_b.var),
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
        let lbl = self.order.borrow().first_essential(&f, &g, &h);
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
        let node = BddNode::new(lbl, f, t);
        let r = self.get_or_insert(node);
        self.apply_table.borrow_mut().insert(ite, r, hash);
        r
    }

    fn cond_helper(&'a self, bdd: BddPtr<'a>, lbl: VarLabel, value: bool) -> BddPtr<'a> {
        self.cond_with_alloc(bdd, lbl, value, &mut Vec::new())
    }
}

impl<'a, T: IteTable<'a, BddPtr<'a>> + Default> RobddBuilder<'a, T> {
    /// Creates a new variable manager with the specified order
    pub fn new(order: VarOrder) -> RobddBuilder<'a, T> {
        RobddBuilder {
            compute_table: RefCell::new(BackedRobinhoodTable::new()),
            order: RefCell::new(order),
            apply_table: RefCell::new(T::default()),
            stats: RefCell::new(BddBuilderStats::new()),
        }
    }

    /// Make a BDD manager with a default variable ordering
    pub fn new_with_linear_order(num_vars: usize) -> RobddBuilder<'a, T> {
        let default_order = VarOrder::linear_order(num_vars);
        RobddBuilder::new(default_order)
    }

    /// Returns the number of variables in the manager
    #[inline]
    pub fn num_vars(&self) -> usize {
        self.order.borrow().num_vars()
    }

    /// Generate a new variable label which was not in the original order. Places the
    /// new variable label at the end of the current order. Returns the newly
    /// generated label.
    #[inline]
    pub fn new_label(&self) -> VarLabel {
        self.order.borrow_mut().new_last()
    }

    /// Generate a new pointer which was not in the original order. Uses
    /// `new_label` to produce a new label at the end of the current order, then
    /// uses `var` to create a pointer in the manager. Returns the output of both.
    #[inline]
    pub fn new_var(&'a self, polarity: bool) -> (VarLabel, BddPtr<'a>) {
        let label = self.new_label();
        let ptr = self.var(label, polarity);
        (label, ptr)
    }

    /// Use `new_var` to create a new positive pointer.
    #[inline]
    pub fn new_pos(&'a self) -> (VarLabel, BddPtr<'a>) {
        self.new_var(true)
    }

    /// Use `new_var` to create a new negative pointer.
    #[inline]
    pub fn new_neg(&'a self) -> (VarLabel, BddPtr<'a>) {
        self.new_var(false)
    }

    /// Get the current variable order
    #[inline]
    pub fn order(&self) -> &VarOrder {
        // TODO fix this, it doesn't need to be unsafe
        unsafe { &*self.order.as_ptr() }
    }

    // condition a BDD *only* if the top variable is `v`; used in `ite`
    fn condition_essential(&'a self, f: BddPtr<'a>, lbl: VarLabel, v: bool) -> BddPtr<'a> {
        match f {
            BddPtr::PtrTrue | BddPtr::PtrFalse => f,
            BddPtr::Reg(node) | BddPtr::Compl(node) => {
                if node.var != lbl {
                    return f;
                }
                let r = if v { f.high_raw() } else { f.low_raw() };
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
        bdd: BddPtr<'a>,
        lbl: VarLabel,
        value: bool,
        alloc: &mut Vec<BddPtr<'a>>,
    ) -> BddPtr<'a> {
        self.stats.borrow_mut().num_recursive_calls += 1;
        match bdd {
            BddPtr::PtrTrue | BddPtr::PtrFalse => bdd,
            BddPtr::Reg(node) | BddPtr::Compl(node) => {
                if self.order.borrow().lt(lbl, node.var) {
                    // we passed the variable in the order, we will never find it
                    return bdd;
                }

                if node.var == lbl {
                    let r = if value { bdd.high_raw() } else { bdd.low_raw() };
                    return if bdd.is_neg() { r.neg() } else { r };
                }

                // check cache
                match bdd.scratch::<usize>() {
                    None => (),
                    Some(v) => {
                        if alloc.len() > v {
                            return if bdd.is_neg() {
                                alloc[v].neg()
                            } else {
                                alloc[v]
                            };
                        }
                    }
                };

                // recurse on the children
                let l = self.cond_with_alloc(bdd.low_raw(), lbl, value, alloc);
                let h = self.cond_with_alloc(bdd.high_raw(), lbl, value, alloc);

                if l == h {
                    // reduce the BDD -- two children identical
                    if bdd.is_neg() {
                        return l.neg();
                    } else {
                        return l;
                    };
                };
                let res = if l != bdd.low_raw() || h != bdd.high_raw() {
                    // cache and return the new BDD
                    let new_bdd = BddNode::new(node.var, l, h);
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

    fn cond_model_h(&'a self, bdd: BddPtr<'a>, m: &PartialModel) -> BddPtr<'a> {
        // TODO: optimize this
        let mut bdd = bdd;
        for m in m.assignment_iter() {
            bdd = self.condition(bdd, m.label(), m.polarity());
        }
        bdd
    }

    /// Compute the Boolean function `f | var = value` for every set value in
    /// the partial model `m`
    ///
    /// Pre-condition: scratch cleared
    pub fn condition_model(&'a self, bdd: BddPtr<'a>, m: &PartialModel) -> BddPtr<'a> {
        debug_assert!(bdd.is_scratch_cleared());
        let r = self.cond_model_h(bdd, m);
        bdd.clear_scratch();
        r
    }

    /// Prints the total number of recursive calls executed so far by the RobddBuilder
    /// This is a stable way to track performance
    pub fn num_recursive_calls(&self) -> usize {
        self.stats.borrow().num_recursive_calls
    }

    fn smooth_helper(&'a self, bdd: BddPtr<'a>, current: usize, total: usize) -> BddPtr<'a> {
        debug_assert!(current <= total);
        if current >= total {
            return bdd;
        }

        match bdd {
            BddPtr::Reg(node) => {
                let smoothed_node = BddNode::new(
                    node.var,
                    self.smooth_helper(node.low, current + 1, total),
                    self.smooth_helper(node.high, current + 1, total),
                );
                self.get_or_insert(smoothed_node)
            }
            BddPtr::Compl(node) => self.smooth_helper(BddPtr::Reg(node), current, total).neg(),
            BddPtr::PtrTrue | BddPtr::PtrFalse => {
                let var = self.order.borrow().var_at_level(current);
                let smoothed_node = BddNode::new(
                    var,
                    self.smooth_helper(bdd, current + 1, total),
                    self.smooth_helper(bdd, current + 1, total),
                );
                self.get_or_insert(smoothed_node)
            }
        }
    }

    /// Return a smoothed version of the input BDD. Requires:
    /// - BDD is an ROBDD, i.e. each variable only appears once per path
    /// - variable ordering respects the builder's order
    pub fn smooth(&'a self, bdd: BddPtr<'a>, num_vars: usize) -> BddPtr<'a> {
        // TODO: this num_vars should be tied to the specific BDD, not the manager
        self.smooth_helper(bdd, 0, num_vars)
    }

    pub fn stats(&'a self) -> BddBuilderStats {
        BddBuilderStats {
            num_recursive_calls: self.stats.borrow().num_recursive_calls,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::builder::BottomUpBuilder;
    use crate::repr::WmcParams;
    use crate::util::semirings::{FiniteField, RealSemiring};
    use crate::{builder::cache::AllIteTable, repr::DDNNFPtr};

    use crate::{
        builder::bdd::robdd::RobddBuilder,
        repr::{BddPtr, Cnf, VarLabel},
    };

    // check that (a \/ b) /\ a === a
    #[test]
    fn simple_equality() {
        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(3);
        let v1 = builder.var(VarLabel::new(0), true);
        let v2 = builder.var(VarLabel::new(1), true);
        let r1 = builder.or(v1, v2);
        let r2 = builder.and(r1, v1);
        assert!(
            builder.eq(v1, r2),
            "Not eq:\n {}\n{}",
            v1.to_string_debug(),
            r2.to_string_debug()
        );
    }

    #[test]
    fn simple_ite1() {
        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(3);
        let v1 = builder.var(VarLabel::new(0), true);
        let v2 = builder.var(VarLabel::new(1), true);
        let r1 = builder.or(v1, v2);
        let r2 = builder.ite(r1, v1, BddPtr::false_ptr());
        assert!(
            builder.eq(v1, r2),
            "Not eq:\n {}\n{}",
            v1.to_string_debug(),
            r2.to_string_debug()
        );
    }

    #[test]
    fn test_newvar() {
        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(0);
        let l1 = builder.new_label();
        let l2 = builder.new_label();
        let v1 = builder.var(l1, true);
        let v2 = builder.var(l2, true);
        let r1 = builder.or(v1, v2);
        let r2 = builder.and(r1, v1);
        assert!(
            builder.eq(v1, r2),
            "Not eq:\n {}\n{}",
            v1.to_string_debug(),
            r2.to_string_debug()
        );
    }

    #[test]
    fn test_wmc() {
        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(2);
        let v1 = builder.var(VarLabel::new(0), true);
        let v2 = builder.var(VarLabel::new(1), true);
        let r1 = builder.or(v1, v2);
        let weights = HashMap::from_iter([
            (VarLabel::new(0), (RealSemiring(0.2), RealSemiring(0.8))),
            (VarLabel::new(1), (RealSemiring(0.1), RealSemiring(0.9))),
        ]);
        let params = WmcParams::new(weights);
        let wmc = r1.unsmoothed_wmc(&params);
        assert!((wmc.0 - (1.0 - 0.2 * 0.1)).abs() < 0.000001);
    }

    #[test]
    fn test_condition() {
        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(3);
        let v1 = builder.var(VarLabel::new(0), true);
        let v2 = builder.var(VarLabel::new(1), true);
        let r1 = builder.or(v1, v2);
        let r3 = builder.condition(r1, VarLabel::new(1), false);
        assert!(builder.eq(r3, v1));
    }

    #[test]
    fn test_condition_compl() {
        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(3);
        let v1 = builder.var(VarLabel::new(0), false);
        let v2 = builder.var(VarLabel::new(1), false);
        let r1 = builder.and(v1, v2);
        let r3 = builder.condition(r1, VarLabel::new(1), false);
        assert!(
            builder.eq(r3, v1),
            "Not eq:\nOne: {}\nTwo: {}",
            r3.to_string_debug(),
            v1.to_string_debug()
        );
    }

    #[test]
    fn test_exist() {
        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(3);
        // 1 /\ 2 /\ 3
        let v1 = builder.var(VarLabel::new(0), true);
        let v2 = builder.var(VarLabel::new(1), true);
        let v3 = builder.var(VarLabel::new(2), true);
        let a1 = builder.and(v1, v2);
        let r1 = builder.and(a1, v3);
        let r_expected = builder.and(v1, v3);
        let res = builder.exists(r1, VarLabel::new(1));
        assert!(
            builder.eq(r_expected, res),
            "Got:\nOne: {}\nExpected: {}",
            res.to_string_debug(),
            r_expected.to_string_debug()
        );
    }

    #[test]
    fn test_exist_compl() {
        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(3);
        // 1 /\ 2 /\ 3
        let v1 = builder.var(VarLabel::new(0), false);
        let v2 = builder.var(VarLabel::new(1), false);
        let v3 = builder.var(VarLabel::new(2), false);
        let a1 = builder.and(v1, v2);
        let r1 = builder.and(a1, v3);
        let r_expected = builder.and(v1, v3);
        let res = builder.exists(r1, VarLabel::new(1));
        // let res = r1;
        assert!(
            builder.eq(r_expected, res),
            "Got:\n: {}\nExpected: {}",
            res.to_string_debug(),
            r_expected.to_string_debug()
        );
    }

    #[test]
    fn test_compose() {
        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(3);
        let v0 = builder.var(VarLabel::new(0), true);
        let v1 = builder.var(VarLabel::new(1), true);
        let v2 = builder.var(VarLabel::new(2), true);
        let v0_and_v1 = builder.and(v0, v1);
        let v0_and_v2 = builder.and(v0, v2);
        let res = builder.compose(v0_and_v1, VarLabel::new(1), v2);
        assert!(
            builder.eq(res, v0_and_v2),
            "\nGot: {}\nExpected: {}",
            res.to_string_debug(),
            v0_and_v2.to_string_debug()
        );
    }

    #[test]
    fn test_compose_2() {
        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(4);
        let v0 = builder.var(VarLabel::new(0), true);
        let v1 = builder.var(VarLabel::new(1), true);
        let v2 = builder.var(VarLabel::new(2), true);
        let v3 = builder.var(VarLabel::new(3), true);
        let v0_and_v1 = builder.and(v0, v1);
        let v2_and_v3 = builder.and(v2, v3);
        let v0v2v3 = builder.and(v0, v2_and_v3);
        let res = builder.compose(v0_and_v1, VarLabel::new(1), v2_and_v3);
        assert!(
            builder.eq(res, v0v2v3),
            "\nGot: {}\nExpected: {}",
            res.to_string_debug(),
            v0v2v3.to_string_debug()
        );
    }

    #[test]
    fn test_compose_3() {
        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(4);
        let v0 = builder.var(VarLabel::new(0), true);
        let v1 = builder.var(VarLabel::new(1), true);
        let v2 = builder.var(VarLabel::new(2), true);
        let f = builder.ite(v0, BddPtr::false_ptr(), v1);
        let res = builder.compose(f, VarLabel::new(1), v2);
        let expected = builder.ite(v0, BddPtr::false_ptr(), v2);
        assert!(
            builder.eq(res, expected),
            "\nGot: {}\nExpected: {}",
            res.to_string_debug(),
            expected.to_string_debug()
        );
    }

    #[test]
    fn test_compose_4() {
        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(20);
        let v0 = builder.var(VarLabel::new(4), true);
        let v1 = builder.var(VarLabel::new(5), true);
        let v2 = builder.var(VarLabel::new(6), true);
        let f = builder.ite(v1, BddPtr::false_ptr(), v2);
        let res = builder.compose(f, VarLabel::new(6), v0);
        let expected = builder.ite(v1, BddPtr::false_ptr(), v0);
        assert!(
            builder.eq(res, expected),
            "\nGot: {}\nExpected: {}",
            res.to_string_debug(),
            expected.to_string_debug()
        );
    }

    #[test]
    fn test_new_label() {
        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(0);
        let vlbl1 = builder.new_label();
        let vlbl2 = builder.new_label();
        let v1 = builder.var(vlbl1, false);
        let v2 = builder.var(vlbl2, false);
        let r1 = builder.and(v1, v2);
        let r3 = builder.condition(r1, VarLabel::new(1), false);
        assert!(
            builder.eq(r3, v1),
            "Not eq:\nOne: {}\nTwo: {}",
            r3.to_string_debug(),
            v1.to_string_debug()
        );
    }

    #[test]
    fn circuit1() {
        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(3);
        let x = builder.var(VarLabel::new(0), false);
        let y = builder.var(VarLabel::new(1), true);
        let delta = builder.and(x, y);
        let yp = builder.var(VarLabel::new(2), true);
        let inner = builder.iff(yp, y);
        let conj = builder.and(inner, delta);
        let res = builder.exists(conj, VarLabel::new(1));

        let expected = builder.and(x, yp);
        assert!(
            builder.eq(res, expected),
            "Not eq:\nGot: {}\nExpected: {}",
            res.to_string_debug(),
            expected.to_string_debug()
        );
    }

    #[test]
    fn simple_cond() {
        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(3);
        let x = builder.var(VarLabel::new(0), true);
        let y = builder.var(VarLabel::new(1), false);
        let z = builder.var(VarLabel::new(2), false);
        let r1 = builder.and(x, y);
        let r2 = builder.and(r1, z);
        // now r2 is x /\ !y /\ !z

        let res = builder.condition(r2, VarLabel::new(1), true); // condition on y=T
        let expected = BddPtr::false_ptr();
        assert!(
            builder.eq(res, expected),
            "\nOriginal BDD: {}\nNot eq:\nGot: {}\nExpected: {}",
            r2.to_string_debug(),
            res.to_string_debug(),
            expected.to_string_debug()
        );
    }

    #[test]
    fn wmc_test_2() {
        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(4);
        let x = builder.var(VarLabel::new(0), true);
        let y = builder.var(VarLabel::new(1), true);
        let f1 = builder.var(VarLabel::new(2), true);
        let f2 = builder.var(VarLabel::new(3), true);

        let map = HashMap::from_iter([
            (VarLabel::new(0), (RealSemiring(1.0), RealSemiring(1.0))),
            (VarLabel::new(1), (RealSemiring(1.0), RealSemiring(1.0))),
            (VarLabel::new(2), (RealSemiring(0.8), RealSemiring(0.2))),
            (VarLabel::new(3), (RealSemiring(0.7), RealSemiring(0.3))),
        ]);

        let wmc = WmcParams::new(map);
        let iff1 = builder.iff(x, f1);
        let iff2 = builder.iff(y, f2);
        let obs = builder.or(x, y);
        let and1 = builder.and(iff1, iff2);
        let f = builder.and(and1, obs);
        assert_eq!(f.unsmoothed_wmc(&wmc).0, 0.2 * 0.3 + 0.2 * 0.7 + 0.8 * 0.3);
    }

    #[test]
    fn test_ite_1() {
        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(16);
        let c1 = Cnf::from_string("(1 || 2) && (0 || -2)");
        let c2 = Cnf::from_string("(0 || 1) && (-4 || -7)");
        let cnf1 = builder.compile_cnf(&c1);
        let cnf2 = builder.compile_cnf(&c2);
        let iff1 = builder.iff(cnf1, cnf2);

        let clause1 = builder.and(cnf1, cnf2);
        let clause2 = builder.and(cnf1.neg(), cnf2.neg());
        let and = builder.or(clause1, clause2);

        if and != iff1 {
            println!("cnf1: {}", c1);
            println!("cnf2: {}", c2);
            println!(
                "not equal:\nBdd1: {}\nBdd2: {}",
                and.to_string_debug(),
                iff1.to_string_debug()
            );
        }
        assert_eq!(and, iff1);
    }

    #[test]
    fn smoothed_model_count_with_finite_field_simple() {
        static CNF: &str = "
        p cnf 3 1
        1 2 3 0
        ";
        let cnf = Cnf::from_dimacs(CNF);

        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(cnf.num_vars());

        let bdd = builder.compile_cnf(&cnf);

        let smoothed = builder.smooth(bdd, cnf.num_vars());

        let weights = WmcParams::<FiniteField<1000001>>::new(HashMap::from_iter([
            (VarLabel::new(0), (FiniteField::new(1), FiniteField::new(1))),
            (VarLabel::new(1), (FiniteField::new(1), FiniteField::new(1))),
            (VarLabel::new(2), (FiniteField::new(1), FiniteField::new(1))),
        ]));

        let unsmoothed_model_count = bdd.unsmoothed_wmc(&weights);

        let smoothed_model_count = smoothed.unsmoothed_wmc(&weights);

        assert_eq!(unsmoothed_model_count.value(), 3);
        assert_eq!(smoothed_model_count.value(), 7);
    }

    #[test]
    fn smoothed_weighted_model_count_with_finite_field_simple() {
        // see: https://pysdd.readthedocs.io/en/latest/examples/model_counting.html#perform-weighted-model-counting-on-cnf-file-from-cli
        static CNF: &str = "
        p cnf 2 2
        c weights 0.4 0.6 0.3 0.7
        -1 2 0
        1 -2 0
        ";
        let cnf = Cnf::from_dimacs(CNF);

        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(cnf.num_vars());

        let bdd = builder.compile_cnf(&cnf);

        let smoothed = builder.smooth(bdd, cnf.num_vars());

        let weighted_model_count =
            smoothed.unsmoothed_wmc(&WmcParams::<RealSemiring>::new(HashMap::from_iter([
                (VarLabel::new(0), (RealSemiring(0.4), RealSemiring(0.6))),
                (VarLabel::new(1), (RealSemiring(0.3), RealSemiring(0.7))),
            ])));

        assert_eq!(weighted_model_count.0, 0.54);
    }

    #[test]
    fn wmc_test_with_finite_field_complex() {
        static CNF: &str = "
        p cnf 6 3
        c weights 0.05 0.10 0.15 0.20 0.25 0.30 0.35 0.40 0.45 0.50 0.55 0.60
        1 2 3 4 0
        -2 -3 4 5 0
        -4 -5 6 6 0
        ";
        let cnf = Cnf::from_dimacs(CNF);

        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(cnf.num_vars());

        let bdd = builder.compile_cnf(&cnf);

        let smoothed = builder.smooth(bdd, cnf.num_vars());

        let model_count = smoothed.unsmoothed_wmc(&WmcParams::<FiniteField<1000001>>::new(
            HashMap::from_iter([
                (VarLabel::new(0), (FiniteField::new(1), FiniteField::new(1))),
                (VarLabel::new(1), (FiniteField::new(1), FiniteField::new(1))),
                (VarLabel::new(2), (FiniteField::new(1), FiniteField::new(1))),
                (VarLabel::new(3), (FiniteField::new(1), FiniteField::new(1))),
                (VarLabel::new(4), (FiniteField::new(1), FiniteField::new(1))),
                (VarLabel::new(5), (FiniteField::new(1), FiniteField::new(1))),
            ]),
        ));

        // TODO: this WMC test is broken. not sure why :(
        // let weighted_model_count = smoothed.unsmoothed_wmc(
        //     builder.order(),
        //     &WmcParams::new(HashMap::from_iter([
        //         // (VarLabel::new(0), (RealSemiring(0.10), RealSemiring(0.05))),
        //         // (VarLabel::new(1), (RealSemiring(0.20), RealSemiring(0.15))),
        //         // (VarLabel::new(2), (RealSemiring(0.30), RealSemiring(0.25))),
        //         // (VarLabel::new(3), (RealSemiring(0.40), RealSemiring(0.35))),
        //         // (VarLabel::new(4), (RealSemiring(0.50), RealSemiring(0.45))),
        //         // (VarLabel::new(5), (RealSemiring(0.60), RealSemiring(0.55))),
        //         (VarLabel::new(0), (RealSemiring(0.05), RealSemiring(0.10))),
        //         (VarLabel::new(1), (RealSemiring(0.15), RealSemiring(0.20))),
        //         (VarLabel::new(2), (RealSemiring(0.25), RealSemiring(0.30))),
        //         (VarLabel::new(3), (RealSemiring(0.35), RealSemiring(0.40))),
        //         (VarLabel::new(4), (RealSemiring(0.45), RealSemiring(0.50))),
        //         (VarLabel::new(5), (RealSemiring(0.55), RealSemiring(0.60))),
        //     ])),
        // );

        // verified with pysdd
        //
        // given tiny2-with-weights.cnf
        //
        // p cnf 6 3
        // c weights 0.05 0.10 0.15 0.20 0.25 0.30 0.35 0.40 0.45 0.50 0.55 0.60
        // 1 2 3 4 0
        // -2 -3 4 5 0
        // -4 -5 6 6 0
        //
        // $ pysdd -c tiny2-with-weights.cnf
        // reading cnf...
        // Read CNF: vars=6 clauses=3
        // creating initial vtree balanced
        // creating manager...
        // compiling...

        // compilation time         : 0.001 sec
        //  sdd size                : 10
        //  sdd node count          : 5
        //  sdd model count         : 48    0.000 sec
        //  sdd weighted model count: 0.017015015625000005    0.000 sec
        // done

        assert_eq!(model_count.value(), 48);
        // assert_eq!(weighted_model_count.0, 0.017015015625000005);
    }
}
