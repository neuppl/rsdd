//! Defines the vtree datastructure used by SDDs for decomposition

use super::var_label::{VarLabel, VarSet};
use crate::util::btree::{BTree, LeastCommonAncestor};

pub type VTreeBuilder = BTree<(), VarLabel>;

impl VTreeBuilder {
    pub fn new_node(l: Box<VTreeBuilder>, r: Box<VTreeBuilder>) -> VTreeBuilder {
        VTreeBuilder::Node((), l, r)
    }
    pub fn new_leaf(v: VarLabel) -> VTreeBuilder {
        VTreeBuilder::Leaf(v)
    }


    /// produces a right-linear vtree with the variable order given by `order`
    pub fn right_linear(order: &[VarLabel]) -> VTreeBuilder {
        match order {
            [x] => BTree::Leaf(*x),
            [cur, rest @ ..] => {
                let l_tree = BTree::Leaf(*cur);
                let r_tree = Self::right_linear(rest);
                BTree::Node((), Box::new(l_tree), Box::new(r_tree))
            }
            [] => panic!("invalid right_linear on empty list")
        }
    }

    /// generate an even vtree by splitting a variable ordering in half repeatedly
    /// times; then reverts to a right-linear vtree for the remainder
    pub fn even_split(order: &[VarLabel], num_splits: usize) -> VTreeBuilder {
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VTreeNodeData {
    pub index: VTreeIndex,
    pub decision_set: VarSet
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VTreeLeafData {
    pub index: VTreeIndex,
    pub label: VarLabel
}

pub type VTree = BTree<VTreeNodeData, VTreeLeafData>;

impl VTree {
    pub fn num_vars(&self) -> usize {
        match self {
            BTree::Leaf(v) => v.label.value_usize()+1,
            BTree::Node(_, l, r) => usize::max(l.num_vars(), r.num_vars()),
        }
    }

    pub fn index(&self) -> VTreeIndex {
        match self {
            BTree::Leaf(d) => d.index,
            BTree::Node(d, _, _) => d.index,
        }
    }

    pub fn index_usize(&self) -> usize {
        match self {
            BTree::Leaf(d) => d.index.0,
            BTree::Node(d, _, _) => d.index.0,
        }
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
    /// a mapping from DFS indexes to VTree pointers
    dfs_to_ptr: Vec<*const VTree>,
    /// maps an Sdd VarLabel into its vtree index in the depth-first order
    vtree_idx: Vec<usize>,
    lca: LeastCommonAncestor,
}

impl VTreeManager {
    /// returns (vtreeindex, set of all variables, node)
    fn import(tree: &VTreeBuilder, cur_idx: usize) -> (usize, VarSet, VTree) {
        match tree {
            BTree::Leaf(v) => {
                let mut s = VarSet::new();
                s.insert(*v);
                (cur_idx+1, s, VTree::Leaf(VTreeLeafData { index: VTreeIndex(cur_idx), label: *v }))
            },
            BTree::Node((), l, r) => {
                let (l_idx, mut l_vars, l) = Self::import(l, cur_idx);
                let (r_idx, r_vars, r) = Self::import(r, l_idx+1);
                let d = VTreeNodeData {index: VTreeIndex(l_idx), decision_set: VarSet::new() };
                l_vars.union_with(&r_vars);
                (r_idx, l_vars, BTree::Node(d, Box::new(l), Box::new(r)))
            },
        }
    }

    pub fn new(tree: VTreeBuilder) -> VTreeManager {
        // import the vtree
        let (_, _, t) = Self::import(&tree, 0);
        let mut dfs_to_ptr : Vec<*const VTree> = Vec::new();

        let mut vtree_lookup = vec![0; t.num_vars()];
        for (idx, v) in tree.inorder_dfs_iter().enumerate() {
            if v.is_leaf() {
                vtree_lookup[v.extract_leaf().value_usize()] = idx;
            }
        }

        for v in t.inorder_dfs_iter() {
            unsafe {
                dfs_to_ptr.push(&*(v as *const VTree));
            }
        }

        VTreeManager {
            dfs_to_bfs: tree.dfs_to_bfs_mapping(),
            bfs_to_dfs: tree.bfs_to_dfs_mapping(),
            vtree_idx: vtree_lookup,
            lca: LeastCommonAncestor::new(&tree),
            tree: t,
            dfs_to_ptr
        }
    }

    pub fn vtree_root(&self) -> &VTree {
        &self.tree
    }

    /// Computes the least-common ancestor between `l` and `r`
    pub fn lca(&self, l: *const VTree, r: *const VTree) -> *const VTree {
        let bfs_l = self.dfs_to_bfs[unsafe { *l }.index().0];
        let bfs_r = self.dfs_to_bfs[unsafe { *r }.index().0];
        let bfs_idx = self.lca.lca(bfs_l, bfs_r);
        self.dfs_to_ptr[self.bfs_to_dfs[bfs_idx]]
    }

    /// Given a vtree index, produce a pointer to the vtree this corresponds with
    pub fn get_idx(&self, idx: VTreeIndex) -> &VTree {
        return self.tree.inorder_dfs_iter().nth(idx.0).unwrap();
    }

    /// Find the index into self.vtree that contains the label `lbl`
    /// panics if this does not exist.
    pub fn get_varlabel(&self, lbl: VarLabel) -> *const VTree {
        unsafe { *&( self.vtree_idx[lbl.value_usize()]) }
    }

    /// true if `l` is prime to `r`
    pub fn is_prime(&self, l: VTreeIndex, r: VTreeIndex) -> bool {
        // due to our indexing scheme, checking if a node is prime is 
        // as simple as comparing their indices (see Figure 1 from 
        // 'SDD: A New Canonical Representation of Propositional Knowledge Bases'
        l.0 < r.0
    }
}
