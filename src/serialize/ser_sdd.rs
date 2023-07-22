//! serializes an SDD

use std::collections::HashMap;

use petgraph::{Graph, graph::NodeIndex, dot::Dot};

use crate::repr::sdd::SddPtr;

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
        table: &mut HashMap<SddPtr<'a>, usize>,
        nodes: &mut Vec<SDDOr>,
    ) -> SerSDDPtr {
        let compl = matches!(
            sdd,
            SddPtr::PtrFalse | SddPtr::Var(_, false) | SddPtr::ComplBDD(_) | SddPtr::Compl(_)
        );

        let reg = match sdd {
            SddPtr::ComplBDD(bdd) => SddPtr::BDD(bdd),
            SddPtr::Compl(or) => SddPtr::Reg(or),
            _ => sdd,
        };

        if let Some(index) = table.get(&reg) {
            return SerSDDPtr::Ptr {
                index: *index,
                compl,
            };
        }
        match sdd {
            SddPtr::PtrTrue => SerSDDPtr::True,
            SddPtr::PtrFalse => SerSDDPtr::False,
            SddPtr::Var(label, polarity) => SerSDDPtr::Literal {
                label: label.value_usize(),
                polarity,
            },
            SddPtr::BDD(bdd) | SddPtr::ComplBDD(bdd) => {
                let v = bdd.label();
                let prime_t = SerSDDPtr::Literal {
                    label: v.value_usize(),
                    polarity: true,
                };
                let prime_f = SerSDDPtr::Literal {
                    label: v.value_usize(),
                    polarity: false,
                };
                let l = SDDSerializer::serialize_helper(bdd.low(), table, nodes);
                let h = SDDSerializer::serialize_helper(bdd.high(), table, nodes);
                let o = SDDOr(vec![
                    SDDAnd {
                        prime: prime_t,
                        sub: h,
                    },
                    SDDAnd {
                        prime: prime_f,
                        sub: l,
                    },
                ]);
                nodes.push(o);
                let index = nodes.len() - 1;
                let r = SerSDDPtr::Ptr { index, compl };
                table.insert(SddPtr::BDD(bdd), index);
                r
            }
            SddPtr::Compl(or) | SddPtr::Reg(or) => {
                let o: Vec<SDDAnd> = or
                    .iter()
                    .map(|and| {
                        let p = SDDSerializer::serialize_helper(and.prime(), table, nodes);
                        let s = SDDSerializer::serialize_helper(and.sub(), table, nodes);
                        SDDAnd { prime: p, sub: s }
                    })
                    .collect();
                nodes.push(SDDOr(o));
                let index = nodes.len() - 1;
                let r = SerSDDPtr::Ptr { index, compl };
                table.insert(SddPtr::Reg(or), index);
                r
            }
        }
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

    pub fn to_dot(&self) -> String {
        let mut graph = Graph::<_, _>::new();
        graph.add_node(String::from("F"));
        graph.add_node(String::from("T"));

        for node in self.nodes.iter() {
            graph.add_node(String::from("hi"));
        }

        fn node_petgraph_index(ptr: &SerSDDPtr) -> NodeIndex {
            match ptr {
                SerSDDPtr::True =>  NodeIndex::new(1),
                SerSDDPtr::False =>  NodeIndex::new(0),
                SerSDDPtr::Ptr { index, compl } => NodeIndex::new(index + 2),
                SerSDDPtr::Literal { label, polarity } => todo!(),
            }
        }

        graph.add_edge(NodeIndex::new(0), NodeIndex::new(1), String::from("hi"));

        format!("{:?}", Dot::with_config(&graph, &[]))
    }
}
