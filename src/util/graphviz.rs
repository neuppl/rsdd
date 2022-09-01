use core::cmp::max;
use graphviz_rust::dot_generator::*;
use graphviz_rust::dot_structures::*;
use graphviz_rust::{print, parse};
use graphviz_rust::printer::{PrinterContext};
use std::collections::VecDeque;

use crate::builder::bdd_builder::BddManager;
use crate::builder::bdd_plan::BddPlan;
use crate::builder::repr::builder_bdd::{BddPtr, PointerType};
use crate::builder::*;
use crate::repr::var_label::VarLabel;
use std::collections::HashMap;
use crate::util::graphviz::Id::Escaped;
use crate::util::graphviz::EdgeTy::Pair;

fn escaped_id(s : &str, lvl :u64) -> Id {
    Escaped(format!("\"{}_{}\"", s, lvl))
}
fn var_id(s: &str, lvl : u64, is_const : bool, bool_binsize: Option<u64>) -> NodeId {
    let lvl = match bool_binsize {
        None => if is_const { 0 } else { lvl },
        Some(divisor) => {
            if is_const {
              lvl / divisor
            } else {
                lvl
            }
        },
    };


    let id = escaped_id(s, lvl);
    NodeId(id, None)
}
fn bool_node(s: &str, lvl : u64, bool_binsize: Option<u64>) -> Node {
    let id = var_id(s, lvl, true, bool_binsize);
    Node { id, attributes: vec![attr!("label", s), attr!("shape", "rectangle")]}
}
fn var_node(s: &str, lvl : u64, polarity:bool) -> Node {
    let id = var_id(s, lvl, false, Some(1));
    Node { id, attributes: vec![attr!("label", s)]}
}

fn mk_edge(l: NodeId, r:NodeId, child: BddPtr, is_high : bool) -> Edge {
    let polarity = is_high;
    let is_compl = !child.is_const() && child.is_compl();
    let mut attributes = vec![];
    attributes.push(attr!("style", (if polarity { "solid" } else { "dotted" })));
    if is_compl {
        attributes.push(attr!("color", "red"));
        attributes.push(attr!("labeldistance", "0"));
        attributes.push(attr!("label", "o"));
    }
    Edge {
        ty: Pair(Vertex::N(l), Vertex::N(r)),
        attributes
    }
}

fn get_label(mgr: &BddManager, map: &HashMap<VarLabel, VarLabel>, ptr: BddPtr) -> String {
    match ptr.ptr_type() {
       PointerType::PtrTrue  => "T".to_string(),
       PointerType::PtrFalse => "F".to_string(),
       PointerType::PtrNode => {
           let lbl = ptr.label();
           map.get(&lbl).unwrap_or_else(|| &lbl).value().to_string()
       }
   }
}

/// Print a debug form of the BDD with the label remapping given by `map`
///
/// TODO: probably want a json-like IR for this one.
pub fn render_full(mgr: &BddManager, map: &HashMap<VarLabel, VarLabel>, bool_binsize: Option<u64>, ptr: BddPtr) -> Graph {
    fn print_bdd_helper(
        mgr: &BddManager,
        map: &HashMap<VarLabel, VarLabel>,
        bool_binsize: Option<u64>,

        optr: Option<(BddPtr, u64)>,
        queue: &mut VecDeque<(BddPtr, u64)>,


        nodes: &mut Vec<(Node, u64)>,
        edges: &mut Vec<Edge>,
    ) -> Vec<Stmt> {
        match optr {
            None => {
                nodes
                    .iter()
                    .map(|(n, _l)| Stmt::Node(n.clone()))
                    .chain(edges.iter().map(|e| Stmt::Edge(e.clone())))
                    .collect()
            },
            Some((ptr, lvl)) => {
                match ptr.ptr_type() {
                    PointerType::PtrTrue  => {
                        nodes.extend(vec![(bool_node("T", lvl, bool_binsize), lvl)]);
                        let nxt = queue.pop_front();
                        print_bdd_helper(mgr, map, bool_binsize, nxt, queue, nodes, edges)
                    },
                    PointerType::PtrFalse => {
                        nodes.extend(vec![(bool_node("F", lvl, bool_binsize,), lvl)]);
                        let nxt = queue.pop_front();
                        print_bdd_helper(mgr, map, bool_binsize, nxt, queue, nodes, edges)
                    },
                    PointerType::PtrNode => {
                        let lp = (mgr.low(ptr), lvl + 1);
                        let hp = (mgr.high(ptr), lvl + 1);
                        let nrender = get_label(mgr, map, ptr);
                        let cur_node = (var_node(&nrender, lvl, ptr.is_compl()), lvl);

                        nodes.extend(vec![cur_node.clone()]);
                        edges.extend(vec![
                            mk_edge(cur_node.clone().0.id, var_id(&get_label(mgr, map, lp.0), lvl+1, lp.0.is_const(), bool_binsize), lp.0, false,),
                            mk_edge(cur_node.clone().0.id, var_id(&get_label(mgr, map, hp.0), lvl+1, hp.0.is_const(), bool_binsize), hp.0, true,)
                        ]);
                        queue.push_back(hp);
                        print_bdd_helper(mgr, map, bool_binsize, Some(lp), queue, nodes, edges)
                    }
                }
            }
        }
    }
    let ss = print_bdd_helper(mgr, map, bool_binsize, Some((ptr, 0)), &mut VecDeque::new(), &mut vec![], &mut vec![]);
    let mut g = Graph::DiGraph {
        id: Id::Plain("bdd".to_string()),
        strict: true,
        stmts: ss,
    };
    g
}

pub fn render(mgr: &BddManager, map: &HashMap<VarLabel, VarLabel>, ptr: BddPtr) -> Graph {
    render_full(mgr, map, Some(1), ptr)
}

pub fn to_string(mgr: &BddManager, map: &HashMap<VarLabel, VarLabel>, ptr: BddPtr) -> String {
    print(render(mgr, map, ptr), &mut PrinterContext::default())
}
pub fn to_string_full(mgr: &BddManager, map: &HashMap<VarLabel, VarLabel>, bool_binsize: Option<u64>, ptr: BddPtr) -> String {
    print(render_full(mgr, map, bool_binsize, ptr), &mut PrinterContext::default())
}


mod test_graphviz {
    use super::*;
    #[test]
    fn test_dot_tf() {
        let mut mgr = BddManager::new_default_order(0);
        let map = HashMap::new();
        let tt = mgr.true_ptr();
        let ff = mgr.false_ptr();

        assert_eq!(render(&mgr, &map, tt), parse(r#"
        strict digraph bdd {
          "T_0"[label=T,shape=rectangle]
        }
        "#).unwrap());

        assert_eq!(render(&mgr, &map, ff), parse(r#"
        strict digraph bdd {
          "F_0"[label=F,shape=rectangle]
        }
        "#).unwrap());
    }

    #[test]
    fn test_dot_var() {
        let mut mgr = BddManager::new_default_order(0);
        let map = HashMap::new();

        let avid : VarLabel = mgr.new_var();
        let a = mgr.var(avid, true);
        let expected_str :&str = &format!(r#"
        strict digraph bdd {{
          "{0}_0"[label={0}]
          "F_1"[label=F,shape=rectangle]
          "T_1"[label=T,shape=rectangle]
          "{0}_0"->"F_1"[style=dotted]
          "{0}_0"->"T_1"[style=solid]
        }}
        "#, avid.value()).to_owned();
        let expected_dot : Graph = parse(expected_str).unwrap();

        let a_dot : Graph = render(&mgr, &map, a);
        println!("{}", to_string(&mgr, &map,a));
        assert_eq!(a_dot, expected_dot); // , "Got:\n{:?}\nExpected:\n{:?}\n", a_dot, expected_dot);
    }


    #[test]
    fn test_dot_3or() {
        let mut mgr = BddManager::new_default_order(3);
        let map = HashMap::new();
        let a = mgr.new_var();
        let a = mgr.var(a, true);
        let b = mgr.new_var();
        let b = mgr.var(b, true);
        let c = mgr.new_var();
        let c = mgr.var(c, true);
        let b_or_c = mgr.or(b, c);
        let a_or_b_or_c = mgr.or(a, b_or_c);

        let dot : Graph = render(&mgr, &map, a_or_b_or_c);

        println!("{}", to_string(&mgr,&map, a_or_b_or_c,));
        let expected_str = r#"strict digraph bdd {
            "3_0"[label=3]
            "4_1"[label=4]
            "5_2"[label=5]
            "F_3"[label=F,shape=rectangle]
            "T_1"[label=T,shape=rectangle]
            "T_2"[label=T,shape=rectangle]
            "T_3"[label=T,shape=rectangle]
            "3_0" -> "4_1" [style=dotted]
            "3_0" -> "T_1" [style=solid]
            "4_1" -> "5_2" [style=dotted]
            "4_1" -> "T_2" [style=solid]
            "5_2" -> "F_3" [style=dotted]
            "5_2" -> "T_3" [style=solid]
        }"#;
        let expected_dot : Graph = parse(expected_str).unwrap();

        assert_eq!(dot, expected_dot); // , "Got:\n{:?}\nExpected:\n{:?}\n", a_dot, expected_dot);

        let dot_str = to_string(&mgr,&map, a_or_b_or_c);
        let expected_str = print(expected_dot, &mut PrinterContext::default());

        assert_eq!(dot_str, expected_str);
    }

    #[test]
    fn test_dot_structured() {
        let mut mgr = BddManager::new_default_order(4);
        let map = HashMap::new();
        let a = mgr.new_var();
        let b = mgr.new_var();
        let c = mgr.new_var();
        let d = mgr.new_var();
        let a = mgr.var(a, true);
        let b = mgr.var(b, true);
        let c = mgr.var(c, true);
        let d = mgr.var(d, true);
        let ab = mgr.and(a, b);
        let cd = mgr.and(c, d);
        let acd = mgr.and(a, cd);
        let xor_ab_acd = mgr.xor(ab, acd);
        let xor_ab_acd_d = mgr.xor(xor_ab_acd, d);

        // graph is correct, but compliments are not addressed in a satisfactory manner.
        let expected = r#"strict digraph bdd {
          "4_0"[label=3]
          "7_1"[label=7]
          "5_1"[label=5]
          "6_2"[label=6]
          "6_2_compl"[label=6] // compliment? is this the right thing to do?
          "7_3"[label=7]
          "F_0"[label=F,shape=rectangle]
          "T_0"[label=T,shape=rectangle]

          "4_0" -> "7_1" [style=dotted]
          "4_0" -> "5_1"
          "7_1" -> "F_0" [style=dotted]
          "7_1" -> "T_0"

          // compliment
          "5_1" -> "6_2"
          "5_1" -> "6_2_compl" [style=dotted]

          "6_2" -> "7_3" [style=dotted]
          "6_2" -> "T_0"

          "7_3" -> "F_0" [style=dotted]
          "7_3" -> "T_0"

          // compliment?
          "6_2" -> "7_3" [style=dotted]
          "6_2" -> "T_0"

          // compliment
          "7_3" -> "T_0" [style=dotted]
          "7_3" -> "F_0"

          "6_2_compl" -> "7_1" [style=dotted]
          "6_2_compl" -> "F_0"
        }"#;

        println!("{}", to_string_full(&mgr,&map, None, xor_ab_acd_d));
        todo!();
    }
}
