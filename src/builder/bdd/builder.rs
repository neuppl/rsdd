use crate::repr::robdd::BddNode;

use crate::builder::BottomUpBuilder;

pub use crate::builder::cache::LruTable;
pub use crate::repr::ddnnf::DDNNFPtr;
pub use crate::repr::robdd::BddPtr;
pub use crate::repr::var_label::VarLabel;

pub trait BddBuilder<'a>: BottomUpBuilder<'a, BddPtr<'a>> {
    fn get_or_insert(&'a self, bdd: BddNode<'a>) -> BddPtr<'a>;

    // implementation-dependent helper functions

    fn ite_helper(&'a self, f: BddPtr<'a>, g: BddPtr<'a>, h: BddPtr<'a>) -> BddPtr<'a>;
    fn cond_helper(&'a self, bdd: BddPtr<'a>, lbl: VarLabel, value: bool) -> BddPtr<'a>;

    // convenience utilities
    /// disjoins a list of BDDs
    fn or_lst(&'a self, f: &[BddPtr<'a>]) -> BddPtr<'a> {
        let mut cur_bdd = BddPtr::false_ptr();
        for &itm in f {
            cur_bdd = self.or(cur_bdd, itm);
        }
        cur_bdd
    }

    /// disjoins a list of BDDs
    fn and_lst(&'a self, f: &[BddPtr<'a>]) -> BddPtr<'a> {
        let mut cur_bdd = BddPtr::true_ptr();
        for &itm in f {
            cur_bdd = self.and(cur_bdd, itm);
        }
        cur_bdd
    }
}

impl<'a, T> BottomUpBuilder<'a, BddPtr<'a>> for T
where
    T: BddBuilder<'a>,
{
    fn true_ptr(&self) -> BddPtr<'a> {
        BddPtr::true_ptr()
    }

    fn false_ptr(&self) -> BddPtr<'a> {
        BddPtr::false_ptr()
    }

    /// Get a pointer to the variable with label `lbl` and polarity `polarity`
    fn var(&'a self, label: VarLabel, polarity: bool) -> BddPtr<'a> {
        let bdd = BddNode::new(label, BddPtr::false_ptr(), BddPtr::true_ptr());
        let r = self.get_or_insert(bdd);
        if polarity {
            r
        } else {
            r.neg()
        }
    }

    fn eq(&'a self, a: BddPtr<'a>, b: BddPtr<'a>) -> bool {
        a == b
    }

    /// Produce a new BDD that is the result of conjoining `f` and `g`
    /// ```
    /// # use rsdd::builder::bdd::robdd::RobddBuilder;
    /// # use rsdd::builder::BottomUpBuilder;
    /// # use rsdd::repr::var_label::VarLabel;
    /// # use rsdd::repr::ddnnf::DDNNFPtr;
    /// # use rsdd::builder::cache::all_app::AllTable;
    /// # use rsdd::repr::robdd::BddPtr;
    /// let mut builder = RobddBuilder::<AllTable<BddPtr>>::new_default_order(10);
    /// let lbl_a = builder.new_label();
    /// let a = builder.var(lbl_a, true);
    /// let a_and_not_a = builder.and(a, a.neg());
    /// assert!(a_and_not_a.is_false());
    /// ```
    fn and(&'a self, f: BddPtr<'a>, g: BddPtr<'a>) -> BddPtr<'a> {
        self.ite(f, g, BddPtr::false_ptr())
    }

    fn negate(&'a self, f: BddPtr<'a>) -> BddPtr<'a> {
        f.neg()
    }

    /// if f then g else h
    fn ite(&'a self, f: BddPtr<'a>, g: BddPtr<'a>, h: BddPtr<'a>) -> BddPtr<'a> {
        self.ite_helper(f, g, h)
    }

    /// Compute the Boolean function `f iff g`
    fn iff(&'a self, f: BddPtr<'a>, g: BddPtr<'a>) -> BddPtr<'a> {
        self.ite(f, g, g.neg())
    }

    fn xor(&'a self, f: BddPtr<'a>, g: BddPtr<'a>) -> BddPtr<'a> {
        self.ite(f, g.neg(), g)
    }

    /// Existentially quantifies out the variable `lbl` from `f`
    fn exists(&'a self, bdd: BddPtr<'a>, lbl: VarLabel) -> BddPtr<'a> {
        // TODO this can be optimized by specializing it
        let v1 = self.condition(bdd, lbl, true);
        let v2 = self.condition(bdd, lbl, false);
        self.or(v1, v2)
    }

    /// Compute the Boolean function `f | var = value`
    fn condition(&'a self, bdd: BddPtr<'a>, lbl: VarLabel, value: bool) -> BddPtr<'a> {
        let r = self.cond_helper(bdd, lbl, value);
        bdd.clear_scratch();
        r
    }

    /// Compose `g` into `f` by substituting for `lbl`
    fn compose(&'a self, f: BddPtr<'a>, lbl: VarLabel, g: BddPtr<'a>) -> BddPtr<'a> {
        // TODO this can be optimized with a specialized implementation to make
        // it a single traversal
        let var = self.var(lbl, true);
        let iff = self.iff(var, g);
        let a = self.and(iff, f);

        self.exists(a, lbl)
    }
}
