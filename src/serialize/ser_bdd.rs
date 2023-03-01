//! serializable representation of a BDD

use std::collections::HashMap;

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
    fn serialize_helper(
        bdd: BddPtr,
        table: &mut HashMap<*mut BddNode, usize>,
        nodes: &mut Vec<SerBDD>,
    ) -> SerBDDPtr {
        if bdd.is_true() {
            return SerBDDPtr::True;
        }
        if bdd.is_false() {
            return SerBDDPtr::False;
        }
        if table.contains_key(&bdd.ptr_raw()) {
            let index = table.get(&bdd.ptr_raw()).unwrap().clone();
            return SerBDDPtr::Ptr {
                index: index,
                compl: bdd.is_neg(),
            };
        }

        // new node, recurse
        let l = BDDSerializer::serialize_helper(bdd.low_raw(), table, nodes);
        let h = BDDSerializer::serialize_helper(bdd.high_raw(), table, nodes);
        let new_node = SerBDD {
            topvar: bdd.var().value_usize(),
            low: l,
            high: h,
        };
        nodes.push(new_node);
        let index = nodes.len() - 1;
        let new_ptr = SerBDDPtr::Ptr {
            index: index,
            compl: bdd.is_neg(),
        };
        table.insert(bdd.ptr_raw(), index);
        new_ptr
    }

    pub fn from_bdd(bdd: BddPtr) -> BDDSerializer {
        let mut nodes = Vec::new();
        let mut table = HashMap::new();
        let r = BDDSerializer::serialize_helper(bdd, &mut table, &mut nodes);
        BDDSerializer {
            nodes,
            roots: vec![r],
        }
    }
}
