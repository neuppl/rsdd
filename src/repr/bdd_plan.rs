//! Represent an arbitrary logical formula as an abstract syntax tree

use super::var_label::VarLabel;
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
enum LogicalFormula {
    Var(VarLabel),
    True,
    False,
    And(Box<LogicalFormula>, Box<LogicalFormula>),
    Or(Box<LogicalFormula>, Box<LogicalFormula>),
    Iff(Box<LogicalFormula>, Box<LogicalFormula>),
    Not(Box<LogicalFormula>),
}

