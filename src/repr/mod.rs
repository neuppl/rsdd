//! This module contains the core data-structures for representing logical
//! formulae, as well as auxiliary data-structures that describe how 
//! formulae behave (i.e. variable orderings and decompositions)
//! 
//! (i.e., conjunctive normal forms, arbitrary logical formulae, etc.)

pub mod bdd;
pub mod cnf;
pub mod logical_expr;
pub mod model;
pub mod sat_solver;
pub mod var_label;
pub mod var_order;
pub mod dtree;
pub mod vtree;