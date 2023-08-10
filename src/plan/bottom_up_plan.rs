//! Represents a deferred BDD computation
//! BDD plans are executed according to an in-order left-first depth-first traversal of the tree

use crate::repr::{DTree, VarLabel};

#[derive(Debug, Clone)]
pub enum BottomUpPlan {
    And(Box<BottomUpPlan>, Box<BottomUpPlan>),
    Or(Box<BottomUpPlan>, Box<BottomUpPlan>),
    Iff(Box<BottomUpPlan>, Box<BottomUpPlan>),
    Ite(Box<BottomUpPlan>, Box<BottomUpPlan>, Box<BottomUpPlan>),
    Not(Box<BottomUpPlan>),
    ConstTrue,
    ConstFalse,
    Literal(VarLabel, bool),
}

impl BottomUpPlan {
    // this is a naming thing; perhaps consider renaming this in the future?
    // (but, both not and neg are std::ops, so you'll hit this clippy with both natural choices)
    #[allow(clippy::should_implement_trait)]
    pub fn not(p: BottomUpPlan) -> Self {
        Self::Not(Box::new(p))
    }

    pub fn and(p1: BottomUpPlan, p2: BottomUpPlan) -> Self {
        Self::And(Box::new(p1), Box::new(p2))
    }

    pub fn or(p1: BottomUpPlan, p2: BottomUpPlan) -> Self {
        Self::Or(Box::new(p1), Box::new(p2))
    }

    pub fn iff(p1: BottomUpPlan, p2: BottomUpPlan) -> Self {
        Self::Iff(Box::new(p1), Box::new(p2))
    }

    pub fn ite(pc: BottomUpPlan, pt: BottomUpPlan, pf: BottomUpPlan) -> Self {
        Self::Ite(Box::new(pc), Box::new(pt), Box::new(pf))
    }

    pub fn literal(label: VarLabel, polarity: bool) -> Self {
        Self::Literal(label, polarity)
    }

    /// Given a dtree decomposition of a CNF, generates an appropriate plan for
    /// compiling the CNF
    /// i.e., for a dtree
    ///          /\
    ///        /  (C ∨ D)
    ///       /\
    ///      /  \
    ///     /    \
    /// (A ∨ B)  (B ∨ C)
    /// generates a plan
    ///     &&
    ///    /  \
    ///       (C || D)
    ///   /
    ///     &&
    ///  /       \
    /// (A ∨ B)  (B ∨ C)
    pub fn from_dtree(dtree: &DTree) -> BottomUpPlan {
        match dtree {
            DTree::Node {
                l,
                r,
                cutset: _,
                vars: _,
            } => {
                let l = Self::from_dtree(l);
                let r = Self::from_dtree(r);
                Self::and(l, r)
            }
            DTree::Leaf {
                clause,
                cutset: _,
                vars: _,
            } => {
                if clause.is_empty() {
                    Self::ConstFalse
                } else if clause.len() == 1 {
                    Self::literal(clause[0].label(), clause[0].polarity())
                } else {
                    let first_lit = Self::literal(clause[0].label(), clause[0].polarity());
                    clause.iter().skip(1).fold(first_lit, |acc, i| {
                        let new_l = Self::literal(i.label(), i.polarity());
                        Self::or(acc, new_l)
                    })
                }
            }
        }
    }
}
