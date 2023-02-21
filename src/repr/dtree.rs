//! Implementation of a decomposition tree (dtree)
//! A dtree describes a decomposition structure on clauses of a CNF. See Chapter
//! 9.5 of 'Modeling and Reasoning with Bayesian Networks' by Adnan Darwiche

use std::{cmp::Ordering, iter::FromIterator};

use crate::repr::{
    cnf::Cnf,
    var_label::{Literal, VarLabel},
};

use super::{var_order::VarOrder, vtree::VTree, var_label::VarSet};


#[derive(Clone, Debug)]
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
            DTree::Node { l, r, cutset, vars } => &vars,
            DTree::Leaf { clause, cutset, vars } => &vars,
        }
    }

    /// initialize the set of vars so that, for each node, it holds that 
    /// vars = vars(l) ∪ vars(r)
    fn init_vars(&mut self) -> () {
        match self {
            DTree::Node { l, r, cutset, vars } => {
                l.init_vars();
                r.init_vars();
                *vars = l.get_vars().union(r.get_vars());
            },
            DTree::Leaf { clause, cutset, vars } => {
                for c in clause.iter() {
                    vars.insert(c.get_label());
                }
            },
        }
    }

    fn gen_cutset(&mut self, ancestor_cutset: &VarSet) {
        match self {
            DTree::Node { l, r, cutset, vars } => {
                // cutset of a node is defined (vars(l) ∩ vars(r)) \ ancestor cutset
                let intersect = l.get_vars().intersect_varset(r.get_vars());
                let my_cutset = intersect.minus(ancestor_cutset);
                l.gen_cutset(&my_cutset);
                r.gen_cutset(&my_cutset);
                *cutset = my_cutset;
            },
            DTree::Leaf { clause, cutset, vars } => {
                println!("ancestor cutset: {:?}", ancestor_cutset);
                let my_cutset = vars.minus(ancestor_cutset);
                println!("my cutset: {:?}", my_cutset);
                *cutset = my_cutset;
            },
        }
    }

    fn balanced(leaves: &[&Vec<Literal>]) -> DTree {
        assert!(!leaves.is_empty());
        if leaves.len() == 1 {
            DTree::Leaf {
                vars: VarSet::new(),
                cutset: VarSet::new(),
                clause: leaves[0].clone(),
            }
        } else {
            let (l, r) = leaves.split_at(leaves.len() / 2);
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
                            cutset: VarSet::new(),
                            vars: VarSet::new(),
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
                    cutset: VarSet::new(),
                    vars: VarSet::new(),
                })
            }
        }
        let mut r = root.unwrap();
        r.init_vars();
        r.gen_cutset(&VarSet::new());
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
                VTree::new_node(Box::new(VTree::new_leaf(v1)), Box::new(v2.clone()))
            }
            (&[v1], None) => VTree::new_leaf(v1),
            (&[v, ref vars @ ..], _) => {
                let sub = DTree::right_linear(vars, continuation);
                VTree::new_node(Box::new(VTree::new_leaf(v)), Box::new(sub))
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
            &Self::Leaf {
                clause,
                cutset,
                vars,
            } => {
                let cutset_v : Vec<VarLabel> = cutset.iter().collect();
                if cutset.is_empty() {
                    None
                } else {
                    Some(DTree::right_linear(cutset_v.as_slice(), &None))
                }
            }
            &Self::Node {
                l,
                r,
                cutset,
                vars: _,
            } => {
                let cutset_v: Vec<VarLabel> = cutset.iter().collect();
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
        match &self {
            &Self::Leaf {
                clause,
                cutset,
                vars,
            } => cutset.iter().count(),
            &Self::Node {
                l,
                r,
                cutset,
                vars: _,
            } => {
                let l_len = l.width();
                let r_len = r.width();
                let cur = cutset.iter().count();
                usize::max(cur, usize::max(l_len, r_len))
            }
        }
    }
}

#[test]
fn test_dtree() {
    let cnf = Cnf::from_string(String::from("(1 || -2 || 3) && (2 || 3) && (1 || 2) && (3 || 4) "));
    // let cnf = Cnf::from_string(String::from("(1 || -2 || 3) && (2 || 3)"));
    // let cnf = Cnf::from_string(String::from("(3 || 4) && (1 || 2) "));
    println!("cnf: {:?}", cnf.to_string());
    let order = VarOrder::linear_order(cnf.num_vars());
    let dtree = DTree::from_cnf(&cnf, &order);
    println!("{:#?}", dtree);
    println!("{:#?}", dtree.to_vtree());
    assert!(false);
}
