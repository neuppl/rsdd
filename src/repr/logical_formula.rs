//! Represent an arbitrary logical formula as a tree

use rand::distributions::LogNormal;

use super::var_label::VarLabel;
use egg::{*, rewrite as rw};

// define_language! {
//     enum LogicalFormula {
//         "true" = True,
//         "false" = False,
//         "and" = And([Id; 2]),
//         ""
//         "not" = Not(Box<LogicalFormula>),
//         "iff" = Iff(Box<LogicalFormula>, Box<LogicalFormula>),
//         Symbol(Symbol)
//     }
// }