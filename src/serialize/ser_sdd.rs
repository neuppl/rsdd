//! serializes an SDD 

use std::collections::HashMap;

use crate::repr::{sdd::SddPtr, ddnnf::DDNNFPtr};

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum SerSDDPtr {
    Ptr { index: usize, compl: bool },
    True,
    False,
    Literal { label: usize, polarity: bool }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SDDAnd {
    prime: SerSDDPtr,
    sub: SerSDDPtr
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SDDOr(Vec<SDDAnd>);
    

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SDDSerializer {
    /// list of nodes allocated in the BDD
    /// nodes[0] is true, nodes[1] is false
    nodes: Vec<SDDOr>,
    roots: Vec<SerSDDPtr>
}

impl SDDSerializer { 
    fn serialize_helper(sdd: SddPtr, table: &mut HashMap<SddPtr, usize>, nodes: &mut Vec<SDDOr>) -> SerSDDPtr {
        if table.contains_key(&sdd.to_reg()) {
            let r = table.get(&sdd.to_reg()).unwrap();
            return SerSDDPtr::Ptr { index: *r, compl: sdd.is_neg() }
        }
        let (r, idx) = if sdd.is_true() {
            return SerSDDPtr::True
        } else if sdd.is_false() {
            return SerSDDPtr::False
        } else if sdd.is_var() {
            let l = sdd.get_var();
            return SerSDDPtr::Literal { label: l.get_label().value_usize(), polarity: l.get_polarity() }
        } else if sdd.is_bdd() {
            let v = sdd.topvar();
            let prime_t = SerSDDPtr::Literal { label: v.value_usize(), polarity: true };
            let prime_f = SerSDDPtr::Literal { label: v.value_usize(), polarity: false };
            let l = SDDSerializer::serialize_helper(sdd.low_raw(), table, nodes);
            let h = SDDSerializer::serialize_helper(sdd.high_raw(), table, nodes);
            let o = SDDOr(vec![SDDAnd { prime: prime_t, sub: h}, SDDAnd { prime: prime_f, sub: l}]);
            nodes.push(o);
            let idx = nodes.len() - 1;
            (SerSDDPtr::Ptr { index: idx, compl: sdd.is_neg() }, idx)
        } else { // it's an SDD
            let o : Vec<SDDAnd> = sdd.node_iter().map(|and| {
                let p = SDDSerializer::serialize_helper(and.prime(), table, nodes);
                let s = SDDSerializer::serialize_helper(and.sub(), table, nodes);
                SDDAnd { prime: p, sub: s }
            }).collect();
            nodes.push(SDDOr(o));
            let idx = nodes.len() - 1;
            (SerSDDPtr::Ptr { index: idx, compl: sdd.is_neg() }, idx)
        };
        table.insert(sdd.to_reg(), idx);
        return r;
    }

    pub fn from_sdd(sdd: SddPtr) -> SDDSerializer { 
        let mut nodes = Vec::new();
        let mut table = HashMap::new();
        let r = SDDSerializer::serialize_helper(sdd, &mut table, &mut nodes);
        SDDSerializer { nodes, roots: vec![r] }
    }
}