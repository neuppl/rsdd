//! Implementation of a decomposition tree (dtree)
//! A dtree describes a decomposition structure on clauses of a CNF. See Chapter
//! 9.5 of 'Modeling and Reasoning with Bayesian Networks' by Adnan Darwiche
//! 

use std::{iter::FromIterator};
use im::HashSet;

use crate::repr::{var_label::{Literal, VarLabel}, cnf::Cnf};

use super::{var_order::VarOrder, repr::builder_sdd::VTree};

type VarSet = HashSet<VarLabel>;

// #[derive(Clone, Debug)]
// struct VarSet {
//     data: bit_set::BitSet
// }

// impl VarSet {

// }

#[derive(Clone, Debug)]
pub enum DTree {
    Node{ l: Box<DTree>, r: Box<DTree>, cutset: Option<VarSet>, vars: Option<VarSet>}, 
    Leaf { v: Vec<Literal>, cutset: Option<VarSet>, vars: Option<VarSet> }
}

impl DTree {
    fn get_vars(&self) -> &VarSet {
        match self {
            Self::Leaf { v, cutset: _ , vars} => &vars.as_ref().unwrap(), 
            Self::Node { l, r, cutset: _, vars} => vars.as_ref().unwrap()
        }
    }

    fn init_vars(&mut self) -> VarSet {
        match self {
            Self::Leaf { v, cutset: _ , ref mut vars} => {
                let r = im::HashSet::from_iter(v.iter().map(|x| x.get_label()));
                *vars = Some(r.clone());
                r
            }, 
            Self::Node { l, r, cutset: _, ref mut vars } if vars.is_none() => {
                let l_vars = l.init_vars().clone();
                let r_vars = r.init_vars().clone();
                let r = l_vars.union(r_vars);
                *vars = Some(r.clone());
                r
            }
            Self::Node { l, r, cutset: _, ref mut vars } if vars.is_some() => vars.as_ref().unwrap().clone(),
            _ => panic!()
        }
    }

    fn gen_cutset(&mut self, parent_vars: im::HashSet<VarLabel>) -> () {
        match self { 
            Self::Leaf{v, cutset, vars} => {
                // cutset of a leaf is all variables not mentioned in any ancestor
                // let c : im::HashSet<VarLabel> = v.iter().filter_map(|cur_lit| {
                //     if !parent_vars.contains(&cur_lit.get_label()) {
                //         Some(cur_lit.get_label())
                //     } else {
                //         None
                //     }
                // }).collect();
                let c = vars.as_ref().unwrap().clone().relative_complement(parent_vars);
                *cutset = Some(c.clone());
            },
            Self::Node { l, r, cutset, vars } => {
                // cutset of a node is (vars(l) ∩ vars(r)) \ cutset(ancestor)
                let l_cuts = l.get_vars().clone();
                let r_cuts = r.get_vars().clone();
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
            DTree::Leaf { v: leaves[0].clone(), cutset: None, vars: None}
        } else {
            let (l, r) = leaves.split_at(leaves.len() / 2);
            let subl = DTree::balanced(l);
            let subr = DTree::balanced(r);
            DTree::Node{ l: Box::new(subl), r: Box::new(subr), cutset: None, vars: None}
        }
    }

    pub fn from_cnf(cnf: &Cnf, elim_order: &VarOrder) -> DTree {
        let mut root : Option<DTree> = None;
        let mut clauses : Vec<Vec<Literal>> = cnf.clauses().to_vec();
        println!("#clauses: {}", clauses.len());
        for var in elim_order.in_order_iter() {
            let all_mentioned : Vec<&Vec<Literal>> = clauses.iter().filter(|x| x.iter().find(|v| v.get_label() == var).is_some()).collect();
            if all_mentioned.len() == 0 {
                continue;
            }
            let subtree = DTree::balanced(&all_mentioned); 
            if root.is_none() {
                root = Some(subtree);
            } else {
                root = Some(DTree::Node { l: Box::new(root.unwrap()), r: Box::new(subtree), cutset: None, vars: None})
            }
            clauses.retain(|x| x.iter().find(|v| v.get_label() == var).is_none());
        }
        let mut r = root.unwrap();
        println!("init vars");
        r.init_vars();
        println!("init vars done");
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
            &Self::Leaf { v: _v, cutset, vars } => {
                let cutset_v : Vec<VarLabel> = cutset.clone().unwrap().into_iter().collect();
                if cutset_v.is_empty() { 
                    None
                } else {
                    Some(DTree::right_linear(cutset_v.as_slice(), &None))
                }
            }, 
            &Self::Node { l, r, cutset, vars } => {
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

    pub fn width(&self) -> usize {
        match &self  { 
            &Self::Leaf { v, cutset, vars } => {
                cutset.as_ref().unwrap().len()
            },
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
//     let cnf = Cnf::from_string(String::from("(1 || 2) && (2 || 3) && (3 || 4) "));
//     println!("cnf: {:?}", cnf.to_string());
//     let order = VarOrder::linear_order(cnf.num_vars());
//     let dtree = DTree::from_cnf(&cnf, &order);
//     println!("{:?}", dtree.to_vtree());
//     assert!(false);
// }