//! Represents a deferred BDD computation
//! BDD plans are executed according to an in-order left-first depth-first traversal of the tree

use crate::repr::{dtree::DTree, var_label::VarLabel};

#[derive(Debug, Clone)]
pub enum BddPlan {
    And(Box<BddPlan>, Box<BddPlan>),
    Or(Box<BddPlan>, Box<BddPlan>),
    Iff(Box<BddPlan>, Box<BddPlan>),
    Ite(Box<BddPlan>, Box<BddPlan>, Box<BddPlan>),
    Not(Box<BddPlan>),
    ConstTrue,
    ConstFalse,
    Literal(VarLabel, bool),
}

impl BddPlan {
    #[allow(clippy::should_implement_trait)] // this is a naming thing; perhaps consider renaming this in the future?
    pub fn not(p: BddPlan) -> Self {
        Self::Not(Box::new(p))
    }

    pub fn and(p1: BddPlan, p2: BddPlan) -> Self {
        Self::And(Box::new(p1), Box::new(p2))
    }

    pub fn or(p1: BddPlan, p2: BddPlan) -> Self {
        Self::Or(Box::new(p1), Box::new(p2))
    }

    pub fn iff(p1: BddPlan, p2: BddPlan) -> Self {
        Self::Iff(Box::new(p1), Box::new(p2))
    }

    pub fn ite(pc: BddPlan, pt: BddPlan, pf: BddPlan) -> Self {
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
    pub fn from_dtree(dtree: &DTree) -> BddPlan {
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
                    Self::literal(clause[0].get_label(), clause[0].get_polarity())
                } else {
                    let first_lit = Self::literal(clause[0].get_label(), clause[0].get_polarity());
                    clause.iter().skip(1).fold(first_lit, |acc, i| {
                        let new_l = Self::literal(i.get_label(), i.get_polarity());
                        Self::or(acc, new_l)
                    })
                }
            }
        }
    }
}
