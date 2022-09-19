//! Implementation of a decomposition tree (dtree)
//! A dtree describes a decomposition structure on clauses of a CNF. See Chapter
//! 9.5 of 'Modeling and Reasoning with Bayesian Networks' by Adnan Darwiche
//!

use bit_set::BitSet;
use std::{cmp::Ordering, iter::FromIterator};

use crate::repr::{
    cnf::Cnf,
    var_label::{Literal, VarLabel},
};

use super::{repr::builder_sdd::VTree, var_order::VarOrder};

type VarSet = BitSet;

#[derive(Clone, Debug)]
pub enum DTree {
    Node {
        l: Box<DTree>,
        r: Box<DTree>,
        cutset: Option<VarSet>,
        vars: Option<VarSet>,
    },
    Leaf {
        v: Vec<Literal>,
        cutset: Option<VarSet>,
        vars: Option<VarSet>,
    },
}

impl DTree {
    fn get_vars(&self) -> &VarSet {
        match self {
            // TODO: resolve unused
            #[allow(unused)]
            Self::Leaf { v, cutset: _, vars } => vars.as_ref().unwrap(),
            // TODO: resolve unused
            #[allow(unused)]
            Self::Node {
                l,
                r,
                cutset: _,
                vars,
            } => vars.as_ref().unwrap(),
        }
    }

    fn init_vars(&mut self) -> VarSet {
        match self {
            Self::Leaf {
                v,
                cutset: _,
                ref mut vars,
            } => {
                let r = BitSet::from_iter(v.iter().map(|x| x.get_label().value_usize()));
                *vars = Some(r.clone());
                r
            }
            Self::Node {
                l,
                r,
                cutset: _,
                ref mut vars,
            } if vars.is_none() => {
                let l_vars = l.init_vars();
                let r_vars = r.init_vars();
                let r: BitSet = l_vars.union(&r_vars).collect();
                *vars = Some(r.clone());
                r
            }
            // TODO: resolve unused
            #[allow(unused)]
            Self::Node {
                l,
                r,
                cutset: _,
                ref mut vars,
            } if vars.is_some() => vars.as_ref().unwrap().clone(),
            _ => panic!(),
        }
    }

    fn gen_cutset(&mut self, parent_vars: &BitSet) {
        match self {
            Self::Leaf { v: _, cutset, vars } => {
                // cutset of leaf is all vars mentioned in leaf not mentioned in parent
                // println!("parent vars: {:?}", parent_vars);
                let mut c = vars.as_ref().unwrap().clone();
                c.difference_with(parent_vars);
                *cutset = Some(c);
            }
            Self::Node {
                l,
                r,
                cutset,
                vars: _,
            } => {
                // cutset of a node is (vars(l) ∩ vars(r)) \ cutset(ancestor)
                let mut cuts = l.get_vars().clone();
                let r_cuts = r.get_vars().clone();
                cuts.intersect_with(&r_cuts);
                cuts.difference_with(parent_vars);

                let mut all_parents = cuts.clone();
                all_parents.union_with(parent_vars);

                l.gen_cutset(&all_parents);
                r.gen_cutset(&all_parents);
                *cutset = Some(cuts);
            }
        }
    }

    fn balanced(leaves: &[&Vec<Literal>]) -> DTree {
        assert!(!leaves.is_empty());
        if leaves.len() == 1 {
            DTree::Leaf {
                v: leaves[0].clone(),
                cutset: None,
                vars: None,
            }
        } else {
            let (l, r) = leaves.split_at(leaves.len() / 2);
            let subl = DTree::balanced(l);
            let subr = DTree::balanced(r);
            DTree::Node {
                l: Box::new(subl),
                r: Box::new(subr),
                cutset: None,
                vars: None,
            }
        }
    }

    pub fn from_cnf(cnf: &Cnf, elim_order: &VarOrder) -> DTree {
        let mut clauses: Vec<Vec<Literal>> = cnf
            .clauses()
            .iter()
            .map(|clause| {
                // sort the literals in each clause by order
                let mut new_c = clause.clone();
                new_c.sort_by(|lit1, lit2| {
                    let lt = elim_order.lt(lit1.get_label(), lit2.get_label());
                    if lt {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    }
                });
                new_c
            })
            .collect();
        clauses.sort_by(|c1, c2| {
            if c1.is_empty() {
                Ordering::Less
            } else if c2.is_empty() {
                Ordering::Greater
            } else {
                let lit1 = c1[0];
                let lit2 = c2[0];
                let lt = elim_order.lt(lit1.get_label(), lit2.get_label());
                if lt {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            }
        });
        let mut all_mentioned = Vec::new();
        let mut root = None;
        for clause in clauses.iter() {
            if all_mentioned.is_empty() {
                all_mentioned.push(clause);
            } else {
                let top_v = all_mentioned[0][0];
                let cur_v = clause[0];
                if top_v.get_label() == cur_v.get_label() {
                    all_mentioned.push(clause);
                } else {
                    // found a new top variable; refresh the tree
                    let subtree = DTree::balanced(&all_mentioned);
                    if root.is_none() {
                        root = Some(subtree)
                    } else {
                        root = Some(DTree::Node {
                            l: Box::new(root.unwrap()),
                            r: Box::new(subtree),
                            cutset: None,
                            vars: None,
                        })
                    }
                    all_mentioned = vec![clause];
                }
            }
        }

        if !all_mentioned.is_empty() {
            let subtree = DTree::balanced(&all_mentioned);
            if root.is_none() {
                root = Some(subtree)
            } else {
                root = Some(DTree::Node {
                    l: Box::new(root.unwrap()),
                    r: Box::new(subtree),
                    cutset: None,
                    vars: None,
                })
            }
        }
        let mut r = root.unwrap();
        r.init_vars();
        r.gen_cutset(&BitSet::new());
        r
    }

    /// construct a right-linear vtree that has an optional continuation
    /// a continuation is a sub-tree that the right-linear vtree terminates to
    fn right_linear(vars: &[VarLabel], continuation: &Option<VTree>) -> VTree {
        // slice patterns are great!
        match (vars, continuation) {
            (&[], None) => panic!("invalid vtree: no vars"),
            (&[], Some(v)) => v.clone(),
            (&[v1], Some(v2)) => {
                VTree::new_node(Box::new(VTree::new_leaf(vec![v1])), Box::new(v2.clone()))
            }
            (&[v1], None) => VTree::new_leaf(vec![v1]),
            (&[v, ref vars @ ..], _) => {
                let sub = DTree::right_linear(vars, continuation);
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
            // TODO: resolve unused
            #[allow(unused)]
            &Self::Leaf {
                v: _v,
                cutset,
                vars,
            } => {
                let cutset_v: Vec<VarLabel> = cutset
                    .clone()
                    .unwrap()
                    .into_iter()
                    .map(|x| VarLabel::new(x as u64))
                    .collect();
                if cutset_v.is_empty() {
                    None
                } else {
                    Some(DTree::right_linear(cutset_v.as_slice(), &None))
                }
            }
            // TODO: resolve unused
            #[allow(unused)]
            &Self::Node { l, r, cutset, vars } => {
                let cutset_v: Vec<VarLabel> = cutset
                    .clone()
                    .unwrap()
                    .into_iter()
                    .map(|x| VarLabel::new(x as u64))
                    .collect();
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
                }
            }
        }
    }

    pub fn width(&self) -> usize {
        // TODO: resolve unused
        #[allow(unused)]
        match &self {
            &Self::Leaf { v, cutset, vars } => cutset.as_ref().unwrap().len(),
            &Self::Node { l, r, cutset, vars } => {
                let l_len = l.width();
                let r_len = r.width();
                let cur = cutset.as_ref().unwrap().len();
                usize::max(cur, usize::max(l_len, r_len))
            }
        }
    }
}

// #[test]
// fn test_dtree() {
//     // let cnf = Cnf::from_string(String::from("(1 || -2 || 3) && (2 || 3) && (1 || 2) && (3 || 4) "));
//     // let cnf = Cnf::from_string(String::from("(1 || -2 || 3) && (2 || 3)"));
//     let cnf = Cnf::from_string(String::from("(3 || 4) && (1 || 2) "));
//     println!("cnf: {:?}", cnf.to_string());
//     let order = VarOrder::linear_order(cnf.num_vars());
//     let dtree = DTree::from_cnf(&cnf, &order);
//     println!("{:?}", dtree);
//     println!("{:?}", dtree.to_vtree());
//     assert!(false);
// }
