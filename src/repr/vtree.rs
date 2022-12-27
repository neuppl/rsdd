//! Defines the vtree datastructure used by SDDs for decomposition

use super::var_label::VarLabel;
use crate::util::btree::{BTree, LeastCommonAncestor};

pub type VTree = BTree<(), VarLabel>;

impl VTree {
    pub fn new_node(l: Box<VTree>, r: Box<VTree>) -> VTree {
        VTree::Node((), l, r)
    }
    pub fn new_leaf(v: VarLabel) -> VTree {
        VTree::Leaf(v)
    }
    pub fn num_vars(&self) -> usize {
        match self {
            BTree::Leaf(v) => v.value_usize() + 1,
            BTree::Node((), l, r) => usize::max(l.num_vars(), r.num_vars()),
        }
    }

    /// produces a right-linear vtree with the variable order given by `order`
    pub fn right_linear(order: &[VarLabel]) -> VTree {
        match order {
            [x] => BTree::Leaf(*x),
            [cur, rest @ ..] => {
                let l_tree = BTree::Leaf(*cur);
                let r_tree = Self::right_linear(rest);
                BTree::Node((), Box::new(l_tree), Box::new(r_tree))
            }
            [] => panic!("invalid right_linear on empty list"),
        }
    }

    /// true if this vtree is a right-linear fragment
    pub fn is_right_linear(&self) -> bool {
        match &self {
            BTree::Node((), l, _) => l.is_leaf(),
            _ => false,
        }
    }

    /// generate an even vtree by splitting a variable ordering in half repeatedly
    /// times; then reverts to a right-linear vtree for the remainder
    pub fn even_split(order: &[VarLabel], num_splits: usize) -> VTree {
        if num_splits <= 0 {
            Self::right_linear(order)
        } else {
            let (l_s, r_s) = order.split_at(order.len() / 2);
            let l_tree = Self::even_split(l_s, num_splits - 1);
            let r_tree = Self::even_split(r_s, num_splits - 1);
            BTree::Node((), Box::new(l_tree), Box::new(r_tree))
        }
    }
}

/// A vtree index uniquely identifies a node in a vtree via a left-first
/// depth-first traversal. For example, each node in vtree is
/// given the following indexing structure:
///        6
///    2       5
///  0  1    3   4
#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct VTreeIndex(usize);

impl VTreeIndex {
    pub fn value(&self) -> usize {
        self.0
    }
}

/// Handles VTree related operations
#[derive(Clone, Debug)]
pub struct VTreeManager {
    /// the vtree datastructure
    tree: VTree,
    /// a mapping from DFS indexes to BFS indexes
    dfs_to_bfs: Vec<usize>,
    /// a mapping from BFS indexes to DFS indexes
    bfs_to_dfs: Vec<usize>,
    /// maps an Sdd VarLabel into its vtree index in the depth-first order
    vtree_idx: Vec<usize>,
    index_lookup: Vec<Box<VTree>>,
    lca: LeastCommonAncestor,
}

impl VTreeManager {
    pub fn new(tree: VTree) -> VTreeManager {
        let mut vtree_lookup = vec![0; tree.num_vars()];
        let mut index_lookup = Vec::new();
        for (idx, v) in tree.inorder_dfs_iter().enumerate() {
            index_lookup.push(Box::new(v.clone()));
            if v.is_leaf() {
                vtree_lookup[v.extract_leaf().value_usize()] = idx;
            }
        }
        VTreeManager {
            dfs_to_bfs: tree.dfs_to_bfs_mapping(),
            bfs_to_dfs: tree.bfs_to_dfs_mapping(),
            index_lookup,
            vtree_idx: vtree_lookup,
            lca: LeastCommonAncestor::new(&tree),
            tree,
        }
    }

    pub fn vtree_root(&self) -> &VTree {
        &self.tree
    }

    /// Computes the least-common ancestor between `l` and `r`
    pub fn lca(&self, l: VTreeIndex, r: VTreeIndex) -> VTreeIndex {
        let bfs_l = self.dfs_to_bfs[l.0];
        let bfs_r = self.dfs_to_bfs[r.0];
        let bfs_idx = self.lca.lca(bfs_l, bfs_r);
        VTreeIndex(self.bfs_to_dfs[bfs_idx])
    }

    /// Given a vtree index, produce a pointer to the vtree this corresponds with
    pub fn get_idx(&self, idx: VTreeIndex) -> &VTree {
        &*(self.index_lookup[idx.0])
    }

    /// Find the index into self.vtree that contains the label `lbl`
    /// panics if this does not exist.
    pub fn get_varlabel_idx(&self, lbl: VarLabel) -> VTreeIndex {
        VTreeIndex(self.vtree_idx[lbl.value_usize()])
    }

    /// true if `l` is prime to `r`
    pub fn is_prime(&self, l: VTreeIndex, r: VTreeIndex) -> bool {
        // due to our indexing scheme, checking if a node is prime is
        // as simple as comparing their indices (see Figure 1 from
        // 'SDD: A New Canonical Representation of Propositional Knowledge Bases'
        l.0 < r.0
    }
}
