//! serializable representation of a BDD

use std::collections::HashMap;

use petgraph::{dot::Dot, graph::NodeIndex, Graph};

use crate::repr::{
    bdd::{BddNode, BddPtr},
    ddnnf::DDNNFPtr,
};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum SerBDDPtr {
    Ptr { index: usize, compl: bool },
    True,
    False,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SerBDD {
    topvar: usize,
    low: SerBDDPtr,
    high: SerBDDPtr,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BDDSerializer {
    /// list of nodes allocated in the BDD
    /// `nodes[0]` is true, `nodes[1]` is false
    nodes: Vec<SerBDD>,
    /// list of all roots (to support multi-rooted BDD serialization with shared structure)
    roots: Vec<SerBDDPtr>,
}

impl BDDSerializer {
    fn serialize_helper<'a>(
        bdd: BddPtr<'a>,
        table: &mut HashMap<&'a BddNode<'a>, usize>,
        nodes: &mut Vec<SerBDD>,
    ) -> SerBDDPtr {
        match bdd {
            BddPtr::PtrTrue => SerBDDPtr::True,
            BddPtr::PtrFalse => SerBDDPtr::False,
            BddPtr::Reg(node) | BddPtr::Compl(node) => {
                if table.contains_key(&node) {
                    return SerBDDPtr::Ptr {
                        index: *table.get(&node).unwrap(),
                        compl: bdd.is_neg(),
                    };
                }

                // new node, recurse
                let l = BDDSerializer::serialize_helper(bdd.low_raw(), table, nodes);
                let h = BDDSerializer::serialize_helper(bdd.high_raw(), table, nodes);
                let new_node = SerBDD {
                    topvar: node.var.value_usize(),
                    low: l,
                    high: h,
                };
                nodes.push(new_node);
                let index = nodes.len() - 1;
                let new_ptr = SerBDDPtr::Ptr {
                    index,
                    compl: bdd.is_neg(),
                };
                table.insert(node, index);
                new_ptr
            }
        }
    }

    pub fn from_bdd(bdd: BddPtr) -> BDDSerializer {
        let mut nodes = Vec::new();
        #[allow(clippy::mutable_key_type)]
        // this is a false positive, since BddNode's Hash/Ord ignore the scratch.
        let mut table = HashMap::new();
        let r = BDDSerializer::serialize_helper(bdd, &mut table, &mut nodes);
        BDDSerializer {
            nodes,
            roots: vec![r],
        }
    }

    pub fn to_dot(&self) -> String {
        let mut graph = Graph::<_, _>::new();
        graph.add_node(String::from("F"));
        graph.add_node(String::from("T"));

        for node in self.nodes.iter() {
            graph.add_node(format!("{}", node.topvar + 1));
        }

        fn node_petgraph_index(ptr: &SerBDDPtr) -> NodeIndex {
            match ptr {
                SerBDDPtr::Ptr { index, compl: _ } => NodeIndex::new(index + 2),
                SerBDDPtr::True => NodeIndex::new(1),
                SerBDDPtr::False => NodeIndex::new(0),
            }
        }

        for (index, node) in self.nodes.iter().enumerate() {
            graph.add_edge(
                NodeIndex::new(index + 2),
                node_petgraph_index(&node.low),
                "low",
            );
            graph.add_edge(
                NodeIndex::new(index + 2),
                node_petgraph_index(&node.high),
                "high",
            );
        }

        format!("{:?}", Dot::with_config(&graph, &[]))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        builder::{
            bdd::{BddBuilder, RobddBuilder},
            cache::lru_app::BddApplyTable,
        },
        repr::{bdd::BddPtr, cnf::Cnf},
    };

    use super::BDDSerializer;

    #[test]
    fn test_dot() {
        static CNF: &str = "
        p cnf 2 2
        1 0
        2 0
        ";
        let cnf = Cnf::from_dimacs(CNF);

        let builder: RobddBuilder<'_, BddApplyTable<BddPtr<'_>>> =
            RobddBuilder::<BddApplyTable<BddPtr>>::new_default_order_lru(cnf.num_vars());
        let bdd = builder.compile_cnf(&cnf);

        let serialized = BDDSerializer::from_bdd(bdd);

        println!("{}", serialized.to_dot());
    }
}
