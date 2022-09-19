//! Defines the internal representations for a trimmed and compressed SDD with
//! complemented edges.

use crate::builder::repr::builder_bdd::*;
use crate::repr::var_label::VarLabel;
use crate::util::btree::*;
use std::mem;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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

/// An SddPtr is either (1) a BDD pointer, or (2) a pointer to an SDD node.
#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Copy)]
pub struct SddPtr {
    /// the index into the table *or* a BDD pointer, depending on the is_bdd
    /// flag
    idx: usize,
    pack: PackedInternalData,
}

/// An SddOr node is a vector of (prime, sub) pairs.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct SddOr {
    pub nodes: Vec<(SddPtr, SddPtr)>,
}

pub enum SddPtrType {
    True,
    False,
    Node,
}

impl SddPtr {
    pub fn new_node(idx: usize, vtree: u16) -> SddPtr {
        SddPtr {
            idx,
            pack: PackedInternalData::new(vtree, 0, 0, 0),
        }
    }

    /// negate an SDD pointer
    pub fn neg(&self) -> SddPtr {
        if self.is_bdd() {
            let v = self.as_bdd_ptr();
            SddPtr::new_bdd(v.neg(), self.pack.vtree() as u16)
        } else {
            let mut v = *self;
            v.pack.set_compl(if self.is_compl() { 0 } else { 1 });
            v
        }
    }

    /// true if the node is complemented
    pub fn is_compl(&self) -> bool {
        self.pack.compl() == 1
    }

    /// create a new constant value
    pub fn new_const(v: bool) -> SddPtr {
        SddPtr {
            idx: 0,
            pack: PackedInternalData::new(0, 0, 1, if v { 0 } else { 1 }),
        }
    }

    /// create a new BDD pointer at the vtree `vtree`
    pub fn new_bdd(ptr: BddPtr, vtree: u16) -> SddPtr {
        SddPtr {
            idx: ptr.raw() as usize,
            pack: PackedInternalData::new(
                vtree,
                1,
                if ptr.is_const() { 1 } else { 0 },
                if ptr.is_compl() { 1 } else { 0 },
            ),
        }
    }

    /// produce an uncomplemented version of an SDD
    pub fn regular(&self) -> SddPtr {
        if self.is_bdd() {
            // produce a regular BDD pointer
            let bdd = BddPtr::from_raw(self.idx as u64);
            let mut v = *self;
            v.idx = bdd.regular().raw() as usize;
            v
        } else {
            let mut v = *self;
            v.pack.set_compl(0);
            v
        }
    }

    pub fn is_const(&self) -> bool {
        self.pack.is_const() == 1
    }

    /// true if this SddPtr represents a logically true Boolean function
    pub fn is_true(&self) -> bool {
        self.is_const() && !self.is_compl()
    }

    /// true if this SddPtr represents a false true Boolean function
    pub fn is_false(&self) -> bool {
        self.is_const() && self.is_compl()
    }

    pub fn is_bdd(&self) -> bool {
        self.pack.is_bdd() == 1
    }

    pub fn as_bdd_ptr(&self) -> BddPtr {
        assert!(self.is_bdd());
        if self.is_true() {
            BddPtr::true_node()
        } else if self.is_false() {
            BddPtr::false_node()
        } else {
            BddPtr::from_raw(self.idx as u64)
        }
    }

    pub fn idx(&self) -> usize {
        self.idx
    }

    /// retrieve the vtree index (as its index in a left-first depth-first traversal)
    pub fn vtree(&self) -> VTreeIndex {
        VTreeIndex(self.pack.vtree() as usize)
    }

    pub fn ptr_type(&self) -> SddPtrType {
        if self.is_false() {
            SddPtrType::False
        } else if self.is_true() {
            SddPtrType::True
        } else {
            SddPtrType::Node
        }
    }
}

/// Represent an SDD as a list of BDD pointers. The ordering of the BDDs
/// corresponds with the depth-first left-first traversal of the vtree
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Sdd {
    Or(SddOr),
    Bdd(BddPtr),
}

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
