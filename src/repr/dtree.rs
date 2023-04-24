//! Implementation of a decomposition tree (dtree) For lots more details, see
//! Chapter 9.5 of 'Modeling and Reasoning with Bayesian Networks' by Adnan
//! Darwiche
//!
//! A dtree, short for decomposition tree, is a binary tree whose leaves are
//! clauses from a CNF. Example:
//! Let φ = (A ∨ B) ∧ (B ∨ C) ∧ (C ∨ D)
//! Then, we can construct the following dtree:
//! ```text
//! //         /\
//! //        /  (C ∨ D)
//! //       /\
//! //      /  \
//! //     /    \
//! // (A ∨ B)  (B ∨ C)
//! ```
//!
//! The primary purpose of a dtree is to describe a recursive decomposition of a
//! CNF. To see how this works, we define a notion of *cutset*.
//!
//! Definition: The *cutset* for an internal node in a dtree is the minimal set
//! of variables where, if conditioned, renders all children of that node
//! independent of all other nodes in the dtree.
//!    Phrased mathematically,
//!      cutset(n) = (vars(l) ∩ vars(r)) \ (ancestor cutset(n))
//!
//!    where vars(n) is the set of all variables in subchildren of n, l is the
//!    left child of n, and r is the right child.
//!
//! We can annotate the above diagram with its cutsets:
//! ```text
//! //         {C}
//! //         /\
//! //        /  (C ∨ D)
//! //       {B}
//! //       /\
//! //      /  \
//! //     /    \
//! // (A ∨ B)  (B ∨ C)
//! ```
//!
//! The *cut-width* of a dtree is the size of the largest cutset. An effective
//! dtree is one that does not have large cutwidth.

use crate::repr::{cnf::Cnf, var_label::Literal};
use serde::Serialize;

use super::{var_label::VarSet, var_order::VarOrder};

#[derive(Clone, Debug, Serialize)]
pub enum DTree {
    Node {
        /// left child
        l: Box<DTree>,
        /// right child
        r: Box<DTree>,
        /// cutset: vars(l) ∩ vars(r) \ ancestor cutsets
        /// in English: set of variables that are in both left and right children that
        /// are not cut by an ancestor
        cutset: VarSet,
        /// vars = vars(l) ∪ vars(r)
        vars: VarSet,
    },
    Leaf {
        /// each leaf associated with a clause
        clause: Vec<Literal>,
        cutset: VarSet,
        /// `vars` is simply set of vars in the clause
        vars: VarSet,
    },
}

impl DTree {
    fn get_vars(&self) -> &VarSet {
        match self {
            DTree::Node {
                l: _,
                r: _,
                cutset: _,
                vars,
            } => vars,
            DTree::Leaf {
                clause: _,
                cutset: _,
                vars,
            } => vars,
        }
    }

    /// initialize the set of vars so that, for each node, it holds that
    /// vars = vars(l) ∪ vars(r)
    fn init_vars(&mut self) {
        match self {
            DTree::Node {
                l,
                r,
                cutset: _,
                vars,
            } => {
                l.init_vars();
                r.init_vars();
                *vars = l.get_vars().union(r.get_vars());
            }
            DTree::Leaf {
                clause,
                cutset: _,
                vars,
            } => {
                for c in clause.iter() {
                    vars.insert(c.get_label());
                }
            }
        }
    }

    fn gen_cutset(&mut self, ancestor_cutset: &VarSet) {
        match self {
            DTree::Node {
                l,
                r,
                cutset,
                vars: _,
            } => {
                // cutset of a node is defined (vars(l) ∩ vars(r)) \ ancestor cutset
                let intersect = l.get_vars().intersect_varset(r.get_vars());
                let my_cutset = intersect.minus(ancestor_cutset);
                let new_ancestor_cutset = ancestor_cutset.union(&my_cutset);
                l.gen_cutset(&new_ancestor_cutset);
                r.gen_cutset(&new_ancestor_cutset);
                *cutset = my_cutset;
            }
            DTree::Leaf {
                clause: _,
                cutset,
                vars,
            } => {
                let my_cutset = vars.minus(ancestor_cutset);
                *cutset = my_cutset;
            }
        }
    }

    /// given a slice of dtree nodes, composes a balanced tree out of these nodes
    /// i.e., for list [a, b, c, d] creates a dtree
    ///    /\
    ///   /  \
    ///  /\  /\
    /// a b  c d
    fn balanced(trees: &[DTree]) -> DTree {
        assert!(!trees.is_empty());
        if trees.len() == 1 {
            trees[0].clone()
        } else {
            let (l, r) = trees.split_at(trees.len() / 2);
            let subl = DTree::balanced(l);
            let subr = DTree::balanced(r);
            DTree::Node {
                l: Box::new(subl),
                r: Box::new(subr),
                cutset: VarSet::new(),
                vars: VarSet::new(),
            }
        }
    }

    /// given an elimination order `elim_order`, generate a corresponding
    /// dtree.
    /// follows algorithm 25, `eo2dtree`, from Darwiche
    pub fn from_cnf(cnf: &Cnf, elim_order: &VarOrder) -> DTree {
        // first generate all leaf subtrees
        let mut subtrees: Vec<DTree> = cnf
            .clauses()
            .iter()
            .map(|clause| {
                let mut l = DTree::Leaf {
                    clause: clause.clone(),
                    cutset: VarSet::new(),
                    vars: VarSet::new(),
                };
                l.init_vars();
                l
            })
            .collect();
        // now recursively construct the dtree (lines 2 -- 6 in Alg 25)
        for o in elim_order.in_order_iter() {
            // collect all subtrees that contain o and compose them
            let (t, s): (Vec<DTree>, Vec<DTree>) =
                subtrees.into_iter().partition(|t| t.get_vars().contains(o));
            subtrees = s;
            if t.is_empty() {
                continue;
            }
            // t is now the set of all subtrees mentioning o
            // compose t and add them to subtrees
            let mut new_t = DTree::balanced(&t);
            new_t.init_vars();
            subtrees.push(new_t);
        }
        // invariant: subtrees now contains only a single element, return that
        assert!(subtrees.len() == 1);
        let mut res = subtrees[0].clone();
        res.gen_cutset(&VarSet::new());
        res
    }

    /// computes the cutwidth of the dtree, which is the size of the largest cut
    ///
    /// the width is a measure of the complexity of elimination for this dtree
    pub fn cutwidth(&self) -> usize {
        match &&self {
            Self::Leaf {
                clause: _,
                cutset: _,
                vars: _,
            } => 0,
            Self::Node {
                l,
                r,
                cutset,
                vars: _,
            } => {
                let l_len = l.cutwidth();
                let r_len = r.cutwidth();
                let cur = cutset.iter().count();
                usize::max(cur, usize::max(l_len, r_len))
            }
        }
    }
}

#[test]
fn test_dtree() {
    let cnf = Cnf::from_string(String::from("(1 || -2) && (2 || 3) && (3 || 4) "));
    // let cnf = Cnf::from_string(String::from("(1 || -2 || 3) && (2 || 3)"));
    // let cnf = Cnf::from_string(String::from("(3 || 4) && (1 || 2) "));
    println!("cnf: {:?}", cnf.to_string());
    let order = VarOrder::linear_order(cnf.num_vars());
    let dtree = DTree::from_cnf(&cnf, &order);
    println!("{:#?}", dtree);
    println!("{:#?}", crate::repr::vtree::VTree::from_dtree(&dtree));
    // assert!(false);
}
