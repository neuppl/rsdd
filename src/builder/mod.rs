//! Contains the core datastructures for constructing and maintaining decision
//! diagrams.

pub mod cache;

pub mod bdd;
pub mod decision_nnf;
pub mod sdd;

use crate::repr::var_label::VarLabel;

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
    fn compose(&'a self, f: Ptr, lbl: VarLabel, g: Ptr) -> Ptr;
}

pub trait TopDownBuilder<'a, Ptr> {
    fn var(&'a self, label: VarLabel, polarity: bool) -> Ptr;

    /// conditions f | v = value
    fn condition(&'a self, a: Ptr, v: VarLabel, value: bool) -> Ptr;
}
