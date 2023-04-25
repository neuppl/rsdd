//! Defines the vtree datastructure used by SDDs for decomposition

use std::collections::HashSet;

use super::var_label::VarSet;
use super::{sdd::SddPtr, var_label::VarLabel};
use crate::quickcheck::{Arbitrary, Gen};
use crate::repr::dtree::DTree;
use crate::util::btree::{BTree, LeastCommonAncestor};
use rand::rngs::SmallRng;
use rand::seq::SliceRandom;
use rand::{Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;

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

    pub fn get_all_vars(&self) -> HashSet<usize> {
        match self {
            BTree::Leaf(v) => HashSet::from([v.value_usize()]),
            BTree::Node((), l, r) => {
                let lvar = l.get_all_vars();
                let rvar = r.get_all_vars();
                &lvar | &rvar
            }
        }
    }

    /// panics if the vtree contains redundant variables; used for debug assertions
    fn check_redundant_vars(&self, s: &mut HashSet<usize>) -> bool {
        match self {
            BTree::Leaf(v) => {
                if s.contains(&v.value_usize()) {
                    return true;
                }
                s.insert(v.value_usize());
                return false;
            }
            BTree::Node((), l, r) => l.check_redundant_vars(s) || r.check_redundant_vars(s),
        }
    }

    /// produces a left-linear vtree with the variable order given by `order`
    pub fn left_linear(order: &[VarLabel]) -> VTree {
        match order {
            [x] => BTree::Leaf(*x),
            [cur, rest @ ..] => {
                let l_tree = Self::left_linear(rest);
                let r_tree = BTree::Leaf(*cur);
                BTree::Node((), Box::new(l_tree), Box::new(r_tree))
            }
            [] => panic!("invalid left_linear on empty list"),
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

    /// true if this vtree is a left-linear fragment
    pub fn is_left_linear(&self) -> bool {
        match &self {
            BTree::Node((), _, r) => r.is_leaf(),
            _ => false,
        }
    }

    /// true if this vtree is a right-linear fragment
    pub fn is_right_linear(&self) -> bool {
        match &self {
            BTree::Node((), l, _) => l.is_leaf(),
            _ => false,
        }
    }

    /// construct a right-linear vtree that has an optional continuation
    /// a continuation is a sub-tree that the right-linear vtree terminates to
    fn right_linear_c(vars: &[VarLabel], continuation: &Option<VTree>) -> VTree {
        // slice patterns are great!
        match (vars, continuation) {
            (&[], None) => panic!("invalid vtree: no vars"),
            (&[], Some(v)) => v.clone(),
            (&[v1], Some(v2)) => {
                VTree::new_node(Box::new(VTree::new_leaf(v1)), Box::new(v2.clone()))
            }
            (&[v1], None) => VTree::new_leaf(v1),
            (&[v, ref vars @ ..], _) => {
                let sub = VTree::right_linear_c(vars, continuation);
                VTree::new_node(Box::new(VTree::new_leaf(v)), Box::new(sub))
            }
        }
    }

    /// Converts a dtree into a vtree
    /// For details on this process, see Section 3.5 of
    /// Oztok, Umut, and Adnan Darwiche. "On compiling CNF into decision-DNNF."
    /// International Conference on Principles and Practice of Constraint
    /// Programming. Springer, Cham, 2014.
    pub fn from_dtree(dtree: &DTree) -> Option<VTree> {
        match &&dtree {
            DTree::Leaf {
                clause: _,
                cutset,
                vars: _,
            } => {
                let cutset_v: Vec<VarLabel> = cutset.iter().collect();
                if cutset.is_empty() {
                    None
                } else {
                    Some(VTree::right_linear_c(cutset_v.as_slice(), &None))
                }
            }
            DTree::Node {
                l,
                r,
                cutset,
                vars: _,
            } => {
                let cutset_v: Vec<VarLabel> = cutset.iter().collect();
                let l_vtree = VTree::from_dtree(l);
                let r_vtree = VTree::from_dtree(r);
                match (l_vtree, r_vtree) {
                    (None, None) if cutset_v.is_empty() => None,
                    (None, None) => Some(VTree::right_linear_c(cutset_v.as_slice(), &None)),
                    (Some(l), None) => Some(VTree::right_linear_c(cutset_v.as_slice(), &Some(l))),
                    (None, Some(r)) => Some(VTree::right_linear_c(cutset_v.as_slice(), &Some(r))),
                    (Some(l), Some(r)) => {
                        let subtree = VTree::new_node(Box::new(l), Box::new(r));
                        Some(VTree::right_linear_c(cutset_v.as_slice(), &Some(subtree)))
                    }
                }
            }
        }
    }

    /// generate an even vtree by splitting a variable ordering in half repeatedly
    /// times; then reverts to a right-linear vtree for the remainder
    pub fn even_split(order: &[VarLabel], num_splits: usize) -> VTree {
        if num_splits == 0 {
            Self::right_linear(order)
        } else {
            let (l_s, r_s) = order.split_at(order.len() / 2);
            let l_tree = Self::even_split(l_s, num_splits - 1);
            let r_tree = Self::even_split(r_s, num_splits - 1);
            BTree::Node((), Box::new(l_tree), Box::new(r_tree))
        }
    }

    pub fn rand_split(order: &[VarLabel]) -> VTree {
        match order.len() {
            0 => panic!("invalid label order passed; expects at least one VarLabel"),
            1 => VTree::new_leaf(order[0]),
            2 => VTree::new_node(
                Box::new(VTree::new_leaf(order[0])),
                Box::new(VTree::new_leaf(order[1])),
            ),
            len => {
                // clamps so we're guaranteed at least one item in l_s, r_s
                let mut rng = ChaCha8Rng::from_entropy();
                let split_index = rng.gen_range(1..len);
                let (l_s, r_s) = order.split_at(split_index);
                VTree::new_node(
                    Box::new(Self::rand_split(l_s)),
                    Box::new(Self::rand_split(r_s)),
                )
            }
        }
    }

    pub fn flatten_vtree(vtree: &VTree) -> Vec<&VarLabel> {
        match vtree {
            BTree::Leaf(v) => vec![v],
            BTree::Node((), l, r) => [Self::flatten_vtree(l), Self::flatten_vtree(r)].concat(),
        }
    }

    pub fn is_valid_vtree(vtree: &VTree) -> bool {
        let flat = Self::flatten_vtree(vtree);
        let mut varset = VarSet::new();
        for var in flat.iter() {
            varset.insert(**var);
        }
        flat.len() == varset.len()
    }
}

impl Arbitrary for VTree {
    /// generate an arbitrary vtree on 16 variables
    fn arbitrary(g: &mut Gen) -> VTree {
        let mut rng = SmallRng::seed_from_u64(u64::arbitrary(g));
        let mut vars: Vec<VarLabel> = (0..16).map(VarLabel::new).collect();

        vars.shuffle(&mut rng);

        fn rand_split(order: &[VarLabel], g: &mut Gen) -> VTree {
            match order.len() {
                0 => panic!("invalid label order passed; expects at least one VarLabel"),
                1 => VTree::new_leaf(order[0]),
                2 => VTree::new_node(
                    Box::new(VTree::new_leaf(order[0])),
                    Box::new(VTree::new_leaf(order[1])),
                ),
                len => {
                    // clamps so we're guaranteed at least one item in l_s, r_s
                    let split_index = (usize::arbitrary(g) % (len - 1)) + 1;
                    let (l_s, r_s) = order.split_at(split_index);
                    VTree::new_node(Box::new(rand_split(l_s, g)), Box::new(rand_split(r_s, g)))
                }
            }
        }

        rand_split(&vars[..], g)
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
        debug_assert!(
            !tree.check_redundant_vars(&mut HashSet::new()),
            "VTree contains redundant variables: {:#?}",
            tree
        );
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
        &(self.index_lookup[idx.0])
    }

    /// Find the index into self.vtree that contains the label `lbl`
    /// panics if this does not exist.
    pub fn get_varlabel_idx(&self, lbl: VarLabel) -> VTreeIndex {
        VTreeIndex(self.vtree_idx[lbl.value_usize()])
    }

    /// true if a is prime to b
    /// panics if either is constant
    pub fn is_prime(&self, a: SddPtr, b: SddPtr) -> bool {
        let a_vtree = if a.is_var() {
            self.get_varlabel_idx(a.get_var_label())
        } else {
            a.vtree()
        };
        let b_vtree = if b.is_var() {
            self.get_varlabel_idx(b.get_var_label())
        } else {
            b.vtree()
        };
        self.is_prime_index(a_vtree, b_vtree)
    }

    /// true if a is prime to b
    /// panics if either is constant
    pub fn is_prime_var(&self, a: VarLabel, b: VarLabel) -> bool {
        self.is_prime_index(self.get_varlabel_idx(a), self.get_varlabel_idx(b))
    }

    /// true if the vtree index `l` is prime to `r`
    pub fn is_prime_index(&self, l: VTreeIndex, r: VTreeIndex) -> bool {
        // due to our indexing scheme, checking if a node is prime is
        // as simple as comparing their indices (see Figure 1 from
        // 'SDD: A New Canonical Representation of Propositional Knowledge Bases'
        l.0 < r.0
    }

    /// produces the number of variables allocated by this vtree
    pub fn num_vars(&self) -> usize {
        self.vtree_root().get_all_vars().into_iter().max().unwrap()
    }
}

#[test]
fn from_dtree_is_valid_vtree() {
    let cnf_input = "p cnf 3 6
    1 2 3 4 0
    -2 -3 4 5 0
    -4 -5 6 6 0"
        .to_string();
    let cnf = super::cnf::Cnf::from_file(cnf_input);
    let dtree = DTree::from_cnf(&cnf, &cnf.min_fill_order());
    let vtree = VTree::from_dtree(&dtree).unwrap();
    println!("{:?}", VTree::flatten_vtree(&vtree));
    assert!(VTree::is_valid_vtree(&vtree));
}
