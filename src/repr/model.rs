//! Models and partial models of logical sentences

use super::var_label::{Literal, VarLabel};


#[derive(Debug, Clone)]
pub struct PartialModel {
    /// None if variable is unset
    assignments: Vec<Option<bool>>
}

impl PartialModel {
    pub fn from_vec(assignments: Vec<Option<bool>>) -> PartialModel {
        PartialModel { assignments }
    }

    pub fn from_litvec(assignments: &[Literal], num_vars: usize) -> PartialModel {
        let mut init_assgn = vec![None; num_vars];
        for assgn in assignments {
            init_assgn[assgn.get_label().value_usize()] = Some(assgn.get_polarity());
        }
        PartialModel { assignments: init_assgn }
    }

    /// Unsets a variable's value in the model
    pub fn unset(&mut self, label: VarLabel) -> () {
        self.assignments[label.value_usize()] = None;
    }

    pub fn set(&mut self, label: VarLabel, value: bool) -> () {
        self.assignments[label.value_usize()] = Some(value);
    }

    /// Returns the value of a variable (None if unset)
    pub fn get(&self, label: VarLabel) -> Option<bool> {
        self.assignments[label.value_usize()]
    }
}