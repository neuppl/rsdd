//! Serialized vtree
//! Represents a vtree in the following JSON format:
//!     An array `a=[node0, node1, node2]`
//!     where `a[0]` is the root of the tree
//!     each node is of type SerVTree
//! Example JSON serialization:
//! For the VTree:
//! ```text
//! //       /\
//! //      0  \
//! //         /\
//! //         1 2
//! ```
//! The following JSON will be generated
//! ```text
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
use petgraph::{dot::Dot, graph::NodeIndex, Graph};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum SerVTree {
    /// a var label leaf
    Leaf(usize),
    /// a left and right index
    Node {
        left: Box<SerVTree>,
        right: Box<SerVTree>,
    },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct VTreeSerializer {
    root: SerVTree,
}

impl VTreeSerializer {
    pub fn from_vtree(vtree: &VTree) -> VTreeSerializer {
        // traverses the vtree and returns a node to the current root
        fn helper(t: &VTree) -> Box<SerVTree> {
            match t {
                crate::util::btree::BTree::Leaf(v) => Box::new(SerVTree::Leaf(v.value_usize())),
                crate::util::btree::BTree::Node(_, l, r) => {
                    let left = helper(l);
                    let right = helper(r);
                    Box::new(SerVTree::Node { left, right })
                }
            }
        }
        let root = helper(vtree);
        VTreeSerializer { root: *root }
    }

    pub fn to_dot(&self) -> String {
        let mut graph = Graph::<_, _>::new();

        fn to_dot_helper(vtree: &SerVTree, graph: &mut Graph<String, &str>) -> NodeIndex {
            match vtree {
                SerVTree::Leaf(v) => graph.add_node(format!("{v}")),
                SerVTree::Node { left, right } => {
                    let index = graph.add_node(String::from(""));
                    let left = to_dot_helper(left, graph);
                    let right = to_dot_helper(right, graph);
                    graph.add_edge(index, left, "left");
                    graph.add_edge(index, right, "right");
                    index
                }
            }
        }

        to_dot_helper(&self.root, &mut graph);

        format!("{:?}", Dot::with_config(&graph, &[]))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        repr::{var_label::VarLabel, vtree::VTree},
        serialize::VTreeSerializer,
    };

    #[test]
    fn test_dot() {
        let v0 = VarLabel::new_usize(0);
        let v1 = VarLabel::new_usize(1);
        let v2 = VarLabel::new_usize(2);

        let vtree = VTree::right_linear(&[v0, v1, v2]);

        let serialized = VTreeSerializer::from_vtree(&vtree);

        println!("{}", serialized.to_dot());
    }
}
