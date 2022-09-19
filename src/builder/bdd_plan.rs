//! Represents a deferred BDD computation

// use super::var_label::Literal;

#[derive(Debug, Clone)]
pub enum BddPlan {
    And(Box<BddPlan>, Box<BddPlan>),
    Or(Box<BddPlan>, Box<BddPlan>),
    Iff(Box<BddPlan>, Box<BddPlan>),
    Ite(Box<BddPlan>, Box<BddPlan>, Box<BddPlan>),
    Not(Box<BddPlan>),
    ConstTrue,
    ConstFalse,
    Literal(u64, bool),
}

impl BddPlan {
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
}
