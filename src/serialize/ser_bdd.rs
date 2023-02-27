//! serializable representation of a BDD

use std::collections::HashMap;

use crate::repr::{bdd::BddPtr, ddnnf::DDNNFPtr};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SerBDDPtr {
    index: usize,
    compl: bool
}

impl SerBDDPtr {
    pub fn neg(&self) -> SerBDDPtr { 
        SerBDDPtr { index: self.index, compl: !self.compl }
    }
}


#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum SerBDD { 
    True,
    False,
    Node { topvar: usize, low: SerBDDPtr, high: SerBDDPtr }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BDDSerializer {
    /// list of nodes allocated in the BDD
    /// nodes[0] is true, nodes[1] is false
    nodes: Vec<SerBDD>,
    /// list of all roots (to support multi-rooted BDD serialization with shared structure)
    roots: Vec<SerBDDPtr>
}

impl BDDSerializer { 
    fn serialize_helper(bdd: BddPtr, table: &mut HashMap<BddPtr, SerBDDPtr>, nodes: &mut Vec<SerBDD>) -> SerBDDPtr {
        if bdd.is_true() { 
            return SerBDDPtr { index: 0, compl: false }
        }
        if bdd.is_false() {
            return SerBDDPtr { index: 1, compl: false }
        }
        if table.contains_key(&bdd) {
            return table.get(&bdd).unwrap().clone();
        }

        // new node, recurse
        let l = BDDSerializer::serialize_helper(bdd.low_raw(), table, nodes);
        let h = BDDSerializer::serialize_helper(bdd.high_raw(), table, nodes);
        let new_node = SerBDD::Node { topvar: bdd.var().value_usize(), low: l, high: h };
        nodes.push(new_node);
        let new_ptr = SerBDDPtr { index: nodes.len() - 1, compl: bdd.is_neg() };
        table.insert(bdd, new_ptr.clone());
        new_ptr
    }

    pub fn from_bdd(bdd: BddPtr) -> BDDSerializer { 
        let mut nodes = vec![SerBDD::True, SerBDD::False];
        let mut table = HashMap::new();
        let r = BDDSerializer::serialize_helper(bdd, &mut table, &mut nodes);
        BDDSerializer { nodes, roots: vec![r] }
    }
}