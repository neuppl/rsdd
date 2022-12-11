//! Defines the vtree datastructure used by SDDs for decomposition

use crate::util::btree::{BTree, LeastCommonAncestor};
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


#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct VTreeIndex(usize);

impl VTreeIndex {
    pub fn value(&self) -> usize {
        self.0
    }
}

/// holds metadata for an SDD pointer as a packed u32. It has the following fields:
/// `vtree`: holds the index into a depth-first left-first traversal of the SDD vtree
/// `is_bdd`: true if this SDD pointer points to a BDD
/// `is_const`: true if this SDD pointer is a constant (true or false)
///
/// There is some redundant information that occurs between SddPtr and BddPtr (for instance,
/// whether or not an edge is complemented, or true or false); whenever possible, this
/// information is "pushed inside" the BddPtr
#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Copy)]
struct PackedInternalData {
    data: u32,
}

BITFIELD!(PackedInternalData data : u32 [
    vtree set_vtree[0..16],
    is_bdd set_is_bdd[16..17],
    is_const set_is_const[17..18],
    compl set_compl[18..19],
]);

impl PackedInternalData {
    fn new(vtree: u16, is_bdd: u32, is_const: u32, compl: u32) -> PackedInternalData {
        let mut n = PackedInternalData { data: 0 };
        n.set_vtree(vtree as u32);
        n.set_is_bdd(is_bdd);
        n.set_is_const(is_const);
        n.set_compl(compl);
        n
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
    lca: LeastCommonAncestor,
}


impl VTreeManager {
    pub fn new(tree: VTree) -> VTreeManager {
        let mut vtree_lookup = vec![0; tree.num_vars()];
        for (idx, v) in tree.dfs_iter().enumerate() {
            if v.is_leaf() {
                for label in v.extract_leaf().iter() {
                    vtree_lookup[label.value_usize()] = idx;
                }
            }
        }
        VTreeManager {
            dfs_to_bfs: tree.dfs_to_bfs_mapping(),
            bfs_to_dfs: tree.bfs_to_dfs_mapping(),
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

    pub fn get_idx(&self, idx: VTreeIndex) -> &VTree {
        return self.tree.dfs_iter().nth(idx.0).unwrap();
    }

    /// Find the index into self.vtree that contains the label `lbl`
    /// panics if this does not exist.
    pub fn get_varlabel_idx(&self, lbl: VarLabel) -> VTreeIndex {
        VTreeIndex(self.vtree_idx[lbl.value_usize()])
    }

    /// true if `l` is prime to `r`
    pub fn is_prime(&self, l: VTreeIndex, r: VTreeIndex) -> bool {
        l.0 < r.0
    }
}
