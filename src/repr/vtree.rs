//! Defines the vtree datastructure used by SDDs for decomposition

use crate::util::btree::BTree;
use super::var_label::VarLabel;

pub type VTree = BTree<(), Vec<VarLabel>>;

impl VTree {
    pub fn new_node(l: Box<VTree>, r: Box<VTree>) -> VTree {
        VTree::Node((), l, r)
    }
    pub fn new_leaf(v: Vec<VarLabel>) -> VTree {
        VTree::Leaf(v)
    }

    pub fn num_vars(&self) -> usize {
        match self {
            BTree::Leaf(v) => v.len(),
            BTree::Node((), l, r) => l.num_vars() + r.num_vars(),
        }
    }
}
