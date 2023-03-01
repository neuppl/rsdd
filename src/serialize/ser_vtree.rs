//! Serialized vtree
//! Represents a vtree in the following JSON format:
//!     An array a=[node0, node1, node2]
//!     where a[0] is the root of the tree
//!     each node is of type SerVTree
//! Example JSON serialization:
//! For the VTree:
//! ```
//! //       /\
//! //      0  \
//! //         /\
//! //         1 2
//! ```
//! The following JSON will be generated
//! ```
//! // {
//! //  "root": {
//! //    "Node": {
//! //      "left": {
//! //        "Leaf": 0
//! //      },
//! //      "right": {
//! //        "Node": {
//! //          "left": {
//! //            "Leaf": 1
//! //          },
//! //          "right": {
//! //            "Node": {
//! //              "left": {
//! //                "Leaf": 2
//! //              },
//! //              "right": {
//! //                "Leaf": 3
//! //              }
//! //            }
//! //          }
//! //        }
//! //      }
//! //    }
//! //  }
//! // }
//! ```


use crate::repr::vtree::VTree;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum SerVTree {
    /// a var label leaf
    Leaf(usize),
    /// a left and right index
    Node { left: Box<SerVTree>, right: Box<SerVTree> }
}


#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct VTreeSerializer {
    root: SerVTree
}

impl VTreeSerializer {
    pub fn from_vtree(vtree: &VTree) -> VTreeSerializer {
        // traverses the vtree and returns a node to the current root
        fn helper(t: &VTree) -> Box<SerVTree> {
            match t {
                crate::util::btree::BTree::Leaf(v) => {
                    Box::new(SerVTree::Leaf(v.value_usize()))
                },
                crate::util::btree::BTree::Node(_, l, r) => { 
                    let left = helper(l);
                    let right = helper(r);
                    Box::new(SerVTree::Node { left, right })
                },
            }
        }
        let root = helper(vtree);
        VTreeSerializer { root: *root }
    }
}