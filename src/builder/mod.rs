//! Contains the core datastructures for constructing and maintaining decision
//! diagrams.

use self::bdd_builder::VarLabel;

pub mod cache;

pub mod bdd_builder;
pub mod bdd_plan;
pub mod decision_nnf_builder;
pub mod sdd_builder;

pub trait BottomUpBuilder<'a> {
    type Ptr;

    // constants --- can elide the input lifetimes
    fn true_ptr(&self) -> Self::Ptr;
    fn false_ptr(&self) -> Self::Ptr;

    fn var(&'a self, label: VarLabel, polarity: bool) -> Self::Ptr;

    // primitive operations
    fn and(&'a self, a: Self::Ptr, b: Self::Ptr) -> Self::Ptr;

    /// Compute the Boolean function `f || g`
    /// by default, or is defined using de morgan's law as and
    fn or(&'a self, a: Self::Ptr, b: Self::Ptr) -> Self::Ptr {
        self.negate(self.and(self.negate(a), self.negate(b)))
    }
    fn negate(&'a self, f: Self::Ptr) -> Self::Ptr;

    /// if f then g else h
    fn ite(&'a self, f: Self::Ptr, g: Self::Ptr, h: Self::Ptr) -> Self::Ptr;

    /// if and only if (i.e., Boolean equality)
    fn iff(&'a self, a: Self::Ptr, b: Self::Ptr) -> Self::Ptr;

    /// logical exclusive-or
    fn xor(&'a self, a: Self::Ptr, b: Self::Ptr) -> Self::Ptr;

    /// existentially quantifies `v` out of `f`
    fn exists(&'a self, f: Self::Ptr, v: VarLabel) -> Self::Ptr;

    /// conditions f | v = value
    fn condition(&'a self, a: Self::Ptr, v: VarLabel, value: bool) -> Self::Ptr;

    /// compose g into f for variable v
    /// I.e., computes the logical function (exists v. (g <=> v) /\ f).
    fn compose(&'a self, f: Self::Ptr, lbl: VarLabel, g: Self::Ptr) -> Self::Ptr;
}
