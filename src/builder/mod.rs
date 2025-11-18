//! Contains the core datastructures for constructing and maintaining decision
//! diagrams.

pub mod cache;

pub mod bdd;
pub mod decision_nnf;
pub mod parallel;
pub mod sdd;

use crate::{
    plan::BottomUpPlan,
    repr::{Cnf, LogicalExpr, VarLabel},
};

pub trait BottomUpBuilder<'a, Ptr> {
    // constants --- can elide the input lifetimes
    fn true_ptr(&self) -> Ptr;
    fn false_ptr(&self) -> Ptr;

    fn var(&'a self, label: VarLabel, polarity: bool) -> Ptr;

    // primitive operations

    /// Test for *semantic equality* (not pointer/structural equality)
    fn eq(&'a self, a: Ptr, b: Ptr) -> bool;

    fn and(&'a self, a: Ptr, b: Ptr) -> Ptr;

    /// Compute the Boolean function `f || g`
    /// by default, or is defined using de morgan's law as and
    fn or(&'a self, a: Ptr, b: Ptr) -> Ptr {
        self.negate(self.and(self.negate(a), self.negate(b)))
    }

    fn negate(&'a self, f: Ptr) -> Ptr;

    /// if f then g else h
    fn ite(&'a self, f: Ptr, g: Ptr, h: Ptr) -> Ptr;

    /// if and only if (i.e., Boolean equality)
    fn iff(&'a self, a: Ptr, b: Ptr) -> Ptr;

    /// logical exclusive-or
    fn xor(&'a self, a: Ptr, b: Ptr) -> Ptr;

    /// existentially quantifies `v` out of `f`
    fn exists(&'a self, f: Ptr, v: VarLabel) -> Ptr;

    /// conditions f | v = value
    fn condition(&'a self, a: Ptr, v: VarLabel, value: bool) -> Ptr;

    /// compose g into f for variable v
    /// I.e., computes the logical function (exists v. (g <=> v) /\ f).
    fn compose(&'a self, f: Ptr, lbl: VarLabel, g: Ptr) -> Ptr {
        // TODO this can be optimized with a specialized implementation to make
        // it a single traversal
        let var = self.var(lbl, true);
        let iff = self.iff(var, g);
        let a = self.and(iff, f);

        self.exists(a, lbl)
    }

    // compilation

    /// directly compile a CNF
    fn compile_cnf(&'a self, cnf: &Cnf) -> Ptr;

    /// directly compile a logical expression
    fn compile_logical_expr(&'a self, expr: &LogicalExpr) -> Ptr {
        match &expr {
            LogicalExpr::Literal(lbl, polarity) => self.var(VarLabel::new(*lbl as u64), *polarity),
            LogicalExpr::And(ref l, ref r) => {
                let r1 = self.compile_logical_expr(l);
                let r2 = self.compile_logical_expr(r);
                self.and(r1, r2)
            }
            LogicalExpr::Or(ref l, ref r) => {
                let r1 = self.compile_logical_expr(l);
                let r2 = self.compile_logical_expr(r);
                self.or(r1, r2)
            }
            LogicalExpr::Not(ref e) => self.negate(self.compile_logical_expr(e)),
            LogicalExpr::Iff(ref l, ref r) => {
                let r1 = self.compile_logical_expr(l);
                let r2 = self.compile_logical_expr(r);
                self.iff(r1, r2)
            }
            LogicalExpr::Xor(ref l, ref r) => {
                let r1 = self.compile_logical_expr(l);
                let r2 = self.compile_logical_expr(r);
                self.xor(r1, r2)
            }
            LogicalExpr::Ite {
                ref guard,
                ref thn,
                ref els,
            } => {
                let g = self.compile_logical_expr(guard);
                let t = self.compile_logical_expr(thn);
                let e = self.compile_logical_expr(els);
                self.ite(g, t, e)
            }
        }
    }

    /// Compiles from a BottomUpPlan, which represents a deferred computation
    fn compile_plan(&'a self, expr: &BottomUpPlan) -> Ptr {
        match &expr {
            BottomUpPlan::Literal(var, polarity) => self.var(*var, *polarity),
            BottomUpPlan::And(ref l, ref r) => {
                let r1 = self.compile_plan(l);
                let r2 = self.compile_plan(r);
                self.and(r1, r2)
            }
            BottomUpPlan::Or(ref l, ref r) => {
                let r1 = self.compile_plan(l);
                let r2 = self.compile_plan(r);
                self.or(r1, r2)
            }
            BottomUpPlan::Iff(ref l, ref r) => {
                let r1 = self.compile_plan(l);
                let r2 = self.compile_plan(r);
                self.iff(r1, r2)
            }
            BottomUpPlan::Ite(ref f, ref g, ref h) => {
                let f = self.compile_plan(f);
                let g = self.compile_plan(g);
                let h = self.compile_plan(h);
                self.ite(f, g, h)
            }
            BottomUpPlan::Not(ref f) => {
                let f = self.compile_plan(f);
                self.negate(f)
            }
            BottomUpPlan::ConstTrue => self.true_ptr(),
            BottomUpPlan::ConstFalse => self.false_ptr(),
        }
    }
}

pub trait TopDownBuilder<'a, Ptr> {
    fn var(&'a self, label: VarLabel, polarity: bool) -> Ptr;

    /// conditions f | v = value
    fn condition(&'a self, a: Ptr, v: VarLabel, value: bool) -> Ptr;
}
