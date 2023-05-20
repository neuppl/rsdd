//! Contains the core datastructures for constructing and maintaining decision
//! diagrams.

use self::bdd_builder::VarLabel;

pub mod cache;

pub mod bdd_builder;
pub mod bdd_plan;
pub mod canonicalize;
pub mod decision_nnf_builder;
pub mod sdd_builder;

trait BottomUpBuilder {
    type Ptr<'a>
    where
        Self: 'a;

    /// if f then g else h
    fn ite<'a>(&'a self, f: Self::Ptr<'a>, g: Self::Ptr<'a>, h: Self::Ptr<'a>) -> Self::Ptr<'a>;
    fn and<'a>(&'a self, a: Self::Ptr<'a>, b: Self::Ptr<'a>) -> Self::Ptr<'a>;
    fn or<'a>(&'a self, a: Self::Ptr<'a>, b: Self::Ptr<'a>) -> Self::Ptr<'a>;

    /// if and only if (i.e., Boolean equality)
    fn iff<'a>(&'a self, a: Self::Ptr<'a>, b: Self::Ptr<'a>) -> Self::Ptr<'a>;

    /// logical exclusive-or
    fn xor<'a>(&'a self, a: Self::Ptr<'a>, b: Self::Ptr<'a>) -> Self::Ptr<'a>;

    /// existentially quantifies `v` out of `f`
    fn exists<'a>(&'a self, f: Self::Ptr<'a>, v: VarLabel) -> Self::Ptr<'a>;

    /// conditions f | v = value
    fn condition<'a>(&'a self, a: Self::Ptr<'a>, v: VarLabel, value: bool) -> Self::Ptr<'a>;

    /// compose g into f for variable v
    /// I.e., computes the logical function (exists v. (g <=> v) /\ f).
    fn compose<'a>(&'a self, a: Self::Ptr<'a>, v: VarLabel, value: bool) -> Self::Ptr<'a>;

    fn negate<'a>(&'a self, f: Self::Ptr<'a>) -> Self::Ptr<'a>;

    // constants --- can elide the input lifetimes
    fn true_ptr(&self) -> Self::Ptr<'_>;
    fn false_ptr(&self) -> Self::Ptr<'_>;
    fn var(&self, label: VarLabel, polarity: bool) -> Self::Ptr<'_>;
}
