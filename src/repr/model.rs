//! Models and partial models of logical sentences

use super::var_label::{Literal, VarLabel};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PartialModel {
    /// None if variable is unset
    assignments: Vec<Option<bool>>,
}

impl PartialModel {
    pub fn from_vec(assignments: Vec<Option<bool>>) -> PartialModel {
        PartialModel { assignments }
    }

    /// Creates a partial model from a total model (assignment to all vars)
    pub fn from_total_model(assignments: Vec<bool>) -> PartialModel {
        PartialModel {
            assignments: assignments.into_iter().map(Some).collect(),
        }
    }

    pub fn from_litvec(assignments: &[Literal], num_vars: usize) -> PartialModel {
        let mut init_assgn = vec![None; num_vars];
        for assgn in assignments {
            init_assgn[assgn.get_label().value_usize()] = Some(assgn.get_polarity());
        }
        PartialModel {
            assignments: init_assgn,
        }
    }

    /// Unsets a variable's value in the model
    pub fn unset(&mut self, label: VarLabel) {
        self.assignments[label.value_usize()] = None;
    }

    pub fn set(&mut self, label: VarLabel, value: bool) {
        self.assignments[label.value_usize()] = Some(value);
    }

    /// Returns the value of a variable (None if unset)
    pub fn get(&self, label: VarLabel) -> Option<bool> {
        self.assignments[label.value_usize()]
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

    pub fn get_vec(&self) -> &[Option<bool>] {
        &self.assignments
    }

    /// True if this is a set variable, false otherwise
    pub fn is_set(&self, label: VarLabel) -> bool {
        self.assignments[label.value_usize()].is_some()
    }

    /// Produces an iterator of all the assigned literals
    pub fn assignment_iter<'a>(&'a self) -> impl Iterator<Item = Literal> + 'a {
        self.assignments.iter().enumerate().filter_map(|(idx, x)| {
            x.as_ref()
                .map(|v| Literal::new(VarLabel::new_usize(idx), *v))
        })
    }

    pub fn unassigned_vars<'a>(&'a self) -> impl Iterator<Item = VarLabel> + 'a {
        self.assignments
            .iter()
            .enumerate()
            .filter_map(|(idx, x)| match x {
                None => Some(VarLabel::new_usize(idx)),
                Some(_) => None,
            })
    }
}
