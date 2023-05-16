//! serializes an SDD

use std::collections::HashMap;

use crate::repr::{ddnnf::DDNNFPtr, sdd::SddPtr};

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum SerSDDPtr {
    Ptr { index: usize, compl: bool },
    True,
    False,
    Literal { label: usize, polarity: bool },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SDDAnd {
    prime: SerSDDPtr,
    sub: SerSDDPtr,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SDDOr(Vec<SDDAnd>);

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SDDSerializer {
    /// list of nodes allocated in the BDD
    /// `nodes[0]` is true, `nodes[1]` is false
    nodes: Vec<SDDOr>,
    roots: Vec<SerSDDPtr>,
}

impl SDDSerializer {
    fn serialize_helper<'a>(
        sdd: SddPtr<'a>,
        table: &mut HashMap<&'a SddPtr<'a>, usize>,
        nodes: &mut Vec<SDDOr>,
    ) -> SerSDDPtr {
        todo!();
        // if let Some(index) = table.get(&sdd.to_reg()) {
        //     return SerSDDPtr::Ptr {
        //         index: *index,
        //         compl: sdd.is_neg(),
        //     };
        // }
        // match sdd {
        //     SddPtr::PtrTrue => SerSDDPtr::True,
        //     SddPtr::PtrFalse => SerSDDPtr::False,
        //     SddPtr::Var(label, polarity) => SerSDDPtr::Literal {
        //         label: label.value_usize(),
        //         polarity,
        //     },
        //     SddPtr::BDD(_) | SddPtr::ComplBDD(_) => {
        //         let v = sdd.topvar();
        //         let prime_t = SerSDDPtr::Literal {
        //             label: v.value_usize(),
        //             polarity: true,
        //         };
        //         let prime_f = SerSDDPtr::Literal {
        //             label: v.value_usize(),
        //             polarity: false,
        //         };
        //         let l = SDDSerializer::serialize_helper(sdd.low_raw(), table, nodes);
        //         let h = SDDSerializer::serialize_helper(sdd.high_raw(), table, nodes);
        //         let o = SDDOr(vec![
        //             SDDAnd {
        //                 prime: prime_t,
        //                 sub: h,
        //             },
        //             SDDAnd {
        //                 prime: prime_f,
        //                 sub: l,
        //             },
        //         ]);
        //         nodes.push(o);
        //         let index = nodes.len() - 1;
        //         let r = SerSDDPtr::Ptr {
        //             index,
        //             compl: sdd.is_neg(),
        //         };
        //         table.insert(sdd.to_reg(), index);
        //         r
        //     }
        //     SddPtr::Compl(_) | SddPtr::Reg(_) => {
        //         let o: Vec<SDDAnd> = sdd
        //             .node_iter()
        //             .map(|and| {
        //                 let p = SDDSerializer::serialize_helper(and.prime(), table, nodes);
        //                 let s = SDDSerializer::serialize_helper(and.sub(), table, nodes);
        //                 SDDAnd { prime: p, sub: s }
        //             })
        //             .collect();
        //         nodes.push(SDDOr(o));
        //         let index = nodes.len() - 1;
        //         let r = SerSDDPtr::Ptr {
        //             index,
        //             compl: sdd.is_neg(),
        //         };
        //         table.insert(&sdd.to_reg(), index);
        //         r
        //     }
        // }
    }

    pub fn from_sdd(sdd: SddPtr) -> SDDSerializer {
        let mut nodes = Vec::new();
        let mut table = HashMap::new();
        let r = SDDSerializer::serialize_helper(sdd, &mut table, &mut nodes);
        SDDSerializer {
            nodes,
            roots: vec![r],
        }
    }
}
