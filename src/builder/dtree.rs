//! Implementation of a decomposition tree (dtree)
//! A dtree describes a decomposition structure on clauses of a CNF. See Chapter
//! 9.5 of 'Modeling and Reasoning with Bayesian Networks' by Adnan Darwiche
//! 

use std::{collections::HashSet, iter::FromIterator};

use crate::repr::{var_label::{Literal, VarLabel}, cnf::Cnf};

use super::{var_order::VarOrder, repr::builder_sdd::VTree};

#[derive(Clone, Debug)]
pub enum DTree {
    Node{ l: Box<DTree>, r: Box<DTree>, cutset: Option<im::HashSet<VarLabel>> }, 
    Leaf { v: Vec<Literal>, cutset: Option<im::HashSet<VarLabel>> }
}

impl DTree {
    fn get_vars(&self) -> im::HashSet<VarLabel> {
        match self {
            Self::Leaf { v, cutset: _ } => {
                im::HashSet::from_iter(v.iter().map(|x| x.get_label()))
            }, 
            Self::Node { l, r, cutset: _ } => {
                let l_vars = l.get_vars();
                let r_vars = r.get_vars();
                l_vars.union(r_vars)
            }
        }
    }

    fn gen_cutset(&mut self, parent_vars: im::HashSet<VarLabel>) -> () {
        match self { 
            Self::Leaf{v, cutset} => {
                // cutset of a leaf is all variables not mentioned in any ancestor
                let c : im::HashSet<VarLabel> = v.iter().filter_map(|cur_lit| {
                    if !parent_vars.contains(&cur_lit.get_label()) {
                        Some(cur_lit.get_label())
                    } else {
                        None
                    }
                }).collect();
                *cutset = Some(c.clone());
            },
            Self::Node { l, r, cutset } => {
                // cutset of a node is (vars(l) ∩ vars(r)) \ cutset(ancestor)
                let l_cuts = l.get_vars();
                let r_cuts = r.get_vars();
                let mut intersection = l_cuts.intersection(r_cuts);
                // subtract off parents
                intersection.retain(|x| !parent_vars.contains(x));

                let all_parents = intersection.clone().union(parent_vars);
                l.gen_cutset(all_parents.clone());
                r.gen_cutset(all_parents.clone());
                *cutset = Some(intersection);
            }
        }
    }

    fn balanced(leaves: &[&Vec<Literal>]) -> DTree {
        assert!(leaves.len() > 0);
        if leaves.len() == 1 {
            DTree::Leaf { v: leaves[0].clone(), cutset: None}
        } else {
            let (l, r) = leaves.split_at(leaves.len() / 2);
            let subl = DTree::balanced(l);
            let subr = DTree::balanced(r);
            DTree::Node{ l: Box::new(subl), r: Box::new(subr), cutset: None }
        }
    }

    pub fn from_cnf(cnf: &Cnf, elim_order: &VarOrder) -> DTree {
        let mut root : Option<DTree> = None;
        let mut clauses : Vec<Vec<Literal>> = cnf.clauses().to_vec();
        for var in elim_order.in_order_iter() {
            let all_mentioned : Vec<&Vec<Literal>> = clauses.iter().filter(|x| x.iter().find(|v| v.get_label() == var).is_some()).collect();
            if all_mentioned.len() == 0 {
                continue;
            }
            let subtree = DTree::balanced(&all_mentioned); 
            if root.is_none() {
                root = Some(subtree);
            } else {
                root = Some(DTree::Node { l: Box::new(root.unwrap()), r: Box::new(subtree), cutset: None})
            }
            clauses.retain(|x| x.iter().find(|v| v.get_label() == var).is_none());
        }
        let mut r = root.unwrap();
        r.gen_cutset(im::HashSet::new());
        r
    }

    /// construct a right-linear vtree that has an optional continuation
    /// a continuation is a sub-tree that the right-linear vtree terminates to
    fn right_linear(vars: &[VarLabel], continuation: &Option<VTree>) -> VTree {
        // slice patterns are great!
        match (vars, continuation) {
            (&[], None) => panic!("invalid vtree: no vars"),
            (&[], Some(v)) => v.clone(),
            (&[v1], Some(v2)) => VTree::new_node(Box::new(VTree::new_leaf(vec![v1])), Box::new(v2.clone())),
            (&[v1], None) => VTree::new_leaf(vec![v1]),
            (&[v, ref vars @ .. ], _) => {
                let sub = DTree::right_linear(&vars, continuation);
                VTree::new_node(Box::new(VTree::new_leaf(vec![v])), Box::new(sub))
            }
        }
    }

    /// Converts a dtree into a vtree
    /// For details on this process, see Section 3.5 of 
    /// Oztok, Umut, and Adnan Darwiche. "On compiling CNF into decision-DNNF."
    /// International Conference on Principles and Practice of Constraint
    /// Programming. Springer, Cham, 2014.
    pub fn to_vtree(&self) -> Option<VTree> {
        match &self {
            &Self::Leaf { v: _v, cutset } => {
                let cutset_v : Vec<VarLabel> = cutset.clone().unwrap().into_iter().collect();
                if cutset_v.is_empty() { 
                    None
                } else {
                    Some(DTree::right_linear(cutset_v.as_slice(), &None))
                }
            }, 
            &Self::Node { l, r, cutset } => {
                let cutset_v : Vec<VarLabel> = cutset.clone().unwrap().into_iter().collect();
                let l_vtree = l.to_vtree();
                let r_vtree = r.to_vtree();
                match (l_vtree, r_vtree) {
                    (None, None) if cutset_v.is_empty() => None,
                    (None, None) => Some(DTree::right_linear(cutset_v.as_slice(), &None)),
                    (Some(l), None) => Some(DTree::right_linear(cutset_v.as_slice(), &Some(l))),
                    (None, Some(r)) => Some(DTree::right_linear(cutset_v.as_slice(), &Some(r))),
                    (Some(l), Some(r)) => {
                        let subtree = VTree::new_node(Box::new(l), Box::new(r));
                        Some(DTree::right_linear(cutset_v.as_slice(), &Some(subtree)))
                    }
                    _ => { todo!() }
                }
            }
        }
    }
}

// #[test]
// fn test_dtree() {
//     // let cnf = Cnf::from_string(String::from("(1 || -2 || 3) && (2 || 3) && (1 || 2) && (3 || 4) "));
//     // let cnf = Cnf::from_string(String::from("(1 || -2 || 3) && (2 || 3)"));
//     let cnf = Cnf::from_string(String::from("(1 || 2) && (2 || 3) && (3 || 4) "));
//     println!("cnf: {:?}", cnf.to_string());
//     let order = VarOrder::linear_order(cnf.num_vars());
//     let dtree = DTree::from_cnf(&cnf, &order);
//     println!("{:?}", dtree.to_vtree());
//     assert!(false);
// }