//! Models and partial models of logical sentences

use crate::repr::var_label::{Literal, VarLabel, VarSet};
use std::fmt::Display;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PartialModel {
    /// None if variable is unset
    pub true_assignments: VarSet,
    false_assignments: VarSet, // assignments: Vec<Option<bool>>,
}

impl PartialModel {
    pub fn new(num_vars: usize) -> PartialModel {
        PartialModel {
            true_assignments: VarSet::new_with_num_vars(num_vars),
            false_assignments: VarSet::new_with_num_vars(num_vars),
        }
    }

    pub fn from_vec(assignments: Vec<Option<bool>>) -> PartialModel {
        let mut true_v = VarSet::new_with_num_vars(assignments.len());
        let mut false_v = VarSet::new_with_num_vars(assignments.len());
        for (i, assignment) in assignments.iter().enumerate() {
            match assignment {
                Some(true) => true_v.insert(VarLabel::new_usize(i)),
                Some(false) => false_v.insert(VarLabel::new_usize(i)),
                None => (),
            }
        }
        PartialModel {
            true_assignments: true_v,
            false_assignments: false_v,
        }
    }

    /// Creates a partial model from a total model (assignment to all vars)
    pub fn from_total_model(assignments: Vec<bool>) -> PartialModel {
        Self::from_vec(assignments.into_iter().map(Some).collect())
    }

    pub fn from_litvec(assignments: &[Literal], num_vars: usize) -> PartialModel {
        let mut init_assgn = vec![None; num_vars];
        for assgn in assignments {
            init_assgn[assgn.get_label().value_usize()] = Some(assgn.get_polarity());
        }
        Self::from_vec(init_assgn)
    }

    /// Unsets a variable's value in the model
    pub fn unset(&mut self, label: VarLabel) {
        self.true_assignments.remove(label);
        self.false_assignments.remove(label);
    }

    pub fn set(&mut self, label: VarLabel, value: bool) {
        if value {
            self.true_assignments.insert(label);
            self.false_assignments.remove(label);
        } else {
            self.true_assignments.remove(label);
            self.false_assignments.insert(label);
        }
    }

    /// Returns the value of a variable (None if unset)
    pub fn get(&self, label: VarLabel) -> Option<bool> {
        if self.true_assignments.contains(label) {
            Some(true)
        } else if self.false_assignments.contains(label) {
            Some(false)
        } else {
            None
        }
    }

    pub fn lit_implied(&self, lit: Literal) -> bool {
        match self.get(lit.get_label()) {
            Some(v) => v == lit.get_polarity(),
            None => false,
        }
    }

    pub fn lit_neg_implied(&self, lit: Literal) -> bool {
        match self.get(lit.get_label()) {
            Some(v) => v != lit.get_polarity(),
            None => false,
        }
    }

    /// True if this is a set variable, false otherwise
    pub fn is_set(&self, label: VarLabel) -> bool {
        self.true_assignments.contains(label) || self.false_assignments.contains(label)
    }

    /// Produces an iterator of all the assigned literals
    pub fn assignment_iter(&self) -> impl Iterator<Item = Literal> + '_ {
        let false_iter = self
            .false_assignments
            .iter()
            .map(|x| Literal::new(x, false));
        let true_iter = self.true_assignments.iter().map(|x| Literal::new(x, true));
        false_iter.chain(true_iter)
    }

    pub fn difference<'a>(&'a self, other: &'a Self) -> impl Iterator<Item = Literal> + 'a {
        let true_diff = self
            .true_assignments
            .difference(&other.true_assignments)
            .map(|x| Literal::new(x, true));
        let false_diff = self
            .false_assignments
            .difference(&other.false_assignments)
            .map(|x| Literal::new(x, false));
        false_diff.chain(true_diff)
    }
}

impl Display for PartialModel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "T: {}\nF: {}",
            self.true_assignments, self.false_assignments
        ))
    }
}
