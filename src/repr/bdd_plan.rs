//! Represents a deferred BDD computation

use super::var_label::Literal;

#[derive(Debug, Clone)]
pub enum BddPlan {
    And(Box<BddPlan>, Box<BddPlan>),
    Or(Box<BddPlan>, Box<BddPlan>),
    Iff(Box<BddPlan>, Box<BddPlan>),
    Ite(Box<BddPlan>, Box<BddPlan>, Box<BddPlan>),
    Not(Box<BddPlan>),
    ConstTrue,
    ConstFalse,
    Literal(u64, bool)
}