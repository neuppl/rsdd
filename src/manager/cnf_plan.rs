//! Defines a plan for compiling a CNF efficiently

use repr::cnf::*;
use repr::var_label::{Literal, VarLabel};

struct CompileTree<'a> {
    Node: (&'a CompileTree<'a>, &'a CompileTree<'a>),
    Leaf: &'a Vec<Vec<Literal>>
}