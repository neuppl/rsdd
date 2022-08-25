use core::cmp::max;
use graphviz_rust::dot_generator::*;
use graphviz_rust::dot_structures::*;
use graphviz_rust::{print, parse};
use graphviz_rust::printer::{DotPrinter, PrinterContext};
use std::collections::VecDeque;

use crate::builder::bdd_builder::BddManager;
use crate::builder::bdd_plan::BddPlan;
use crate::builder::repr::builder_bdd::BddPtr;
use crate::builder::repr::builder_sdd::{SddPtr, VTree};
use crate::builder::*;
use crate::repr::var_label::VarLabel;
use std::collections::HashMap;
use crate::util::graphviz::Id::Escaped;
use crate::util::graphviz::EdgeTy::Pair;


struct NodeMeta {
    v: VarLabel,
    s: String,
}

fn escaped_id(s : &str, lvl :u64) -> Id {
    Escaped(format!("\"{}_{}\"", s, lvl))
}
fn var_id(s: &str, lvl : u64) -> NodeId {
    let id = escaped_id(s, lvl);
    NodeId(id, None)
}
fn bool_node(s: &str, lvl : u64) -> Node {
    let id = var_id(s, lvl);
    Node { id, attributes: vec![attr!("label", s), attr!("shape", "rectangle")]}
}
fn var_node(s: &str, lvl : u64, polarity:bool) -> Node {
    let id = var_id(s, lvl);
    // Node { id, attributes: vec![attr!("label", s), attr!("style", "fill"), attr!("style", (if polarity { "white" } else { "lightgray" }))]}
    Node { id, attributes: vec![attr!("label", s)]}
}
fn mk_edge(l: NodeId, r:NodeId, polarity:bool) -> Edge {
    Edge { ty: Pair(Vertex::N(l), Vertex::N(r)), attributes: vec![attr!("style", (if polarity { "solid" } else { "dotted" }))]}
}

fn get_label(mgr: &BddManager, map: &HashMap<VarLabel, VarLabel>, ptr: BddPtr) -> String {
    if mgr.is_var(ptr) {
        println!("[node] {} {} {} {}", mgr.is_true(ptr), mgr.is_false(ptr), mgr.is_var(ptr), ptr.label().value().to_string());
        let lbl = ptr.label();
        map.get(&lbl).unwrap_or_else(|| &lbl).value().to_string()

    } else {
        println!("[bool] {} {} {} {}", mgr.is_true(ptr), mgr.is_false(ptr), mgr.is_var(ptr), ptr.label().value().to_string());
        if mgr.is_true(ptr) {
            "T".to_string()
        } else {
            "F".to_string()
        }
    }
}

fn get_label_(mgr: &BddManager, map: &HashMap<VarLabel, VarLabel>, ptr: BddPtr) -> String {
    let x = ptr.ptr_type().clone();
    println!("[get_label_] {:?} {:?} {:?}", ptr, ptr.ptr_type(), x);

    match x {
       PtrTrue  => {
           println!("[get_label_true] {:?} {:?} {:?}", ptr, ptr.ptr_type(), x);
           "T".to_string()
       },
       PtrFalse => {
           println!("[get_label_false] {:?} {:?} {:?}", ptr, ptr.ptr_type(), x);
           "F".to_string()
       },
       PtrNode => {
           println!("[get_label_node] {:?} {:?} {:?}", ptr, ptr.ptr_type(), x);
           let lbl = ptr.label();
           map.get(&lbl).unwrap_or_else(|| &lbl).value().to_string()
       }
   }
}


/// Print a debug form of the BDD with the label remapping given by `map`
///
/// TODO: probably want a json-like IR for this one.
pub fn render(mgr: &BddManager, map: &HashMap<VarLabel, VarLabel>, ptr: BddPtr) -> Graph {
    use crate::builder::repr::builder_bdd::PointerType::*;
    fn print_bdd_helper(
        mgr: &BddManager,
        map: &HashMap<VarLabel, VarLabel>,
        optr: Option<(BddPtr, u64)>,
        queue: &mut VecDeque<(BddPtr, u64)>,

        nodes: &mut Vec<(Node, u64)>,
        edges: &mut Vec<Edge>,
    ) -> Vec<Stmt> {
        match optr {
            None => {
                println!("wrapup");
                nodes
                    .iter()
                    .map(|(n, _l)| Stmt::Node(n.clone()))
                    .chain(edges.iter().map(|e| Stmt::Edge(e.clone())))
                    .collect()
            },
            Some((ptr, lvl)) => {
        println!("level: {}", lvl);
                match ptr.ptr_type() {
                    PtrTrue  => {
                        nodes.extend(vec![(bool_node("T", lvl), lvl)]);
                        let nxt = queue.pop_front();
                        print_bdd_helper(mgr, map, nxt, queue, nodes, edges)
                    },
                    PtrFalse => {
                        nodes.extend(vec![(bool_node("F", lvl), lvl)]);
                        let nxt = queue.pop_front();
                        print_bdd_helper(mgr, map, nxt, queue, nodes, edges)
                    },
                    PtrNode => {
                        let lp = (mgr.low(ptr), lvl + 1);
                        let hp = (mgr.high(ptr), lvl + 1);
                        // let lbl = ptr.label();
                        // let nrender = map.get(&lbl).unwrap_or_else(|| &lbl).value().to_string();
                        let nrender = get_label_(mgr, map, ptr);

                        println!("{}", nrender);

                        let cur_node = (var_node(&nrender, lvl, ptr.is_compl()), lvl);

                        nodes.extend(vec![cur_node.clone()]);
                        edges.extend(vec![
                            mk_edge(cur_node.clone().0.id, var_id(&get_label_(mgr, map, lp.0), lvl+1), false),
                            mk_edge(cur_node.clone().0.id, var_id(&get_label_(mgr, map, hp.0), lvl+1), true)
                        ]);
                        queue.push_back(hp);
                        print_bdd_helper(mgr, map, Some(lp), queue, nodes, edges)


                        // let lpolarity = attr!(
                        //     "fillcolor",
                        //     if l_p.is_compl() { "white" } else { "lightgray" }
                        // );
                        // let lrender = l_s;
                        // let rpolarity = attr!(
                        //     "fillcolor",
                        //     if h_p.is_compl() { "white" } else { "lightgray" }
                        // );
                        // let rrender = r_s;
                        // let mut ss = vec![
                        //     Stmt::Node(node!(lrender; attr!("style","filled"), lpolarity)),
                        //     Stmt::Node(node!(rrender; attr!("style","filled"), rpolarity)),
                        //     Stmt::Edge(
                        //         edge!(node_id!(nrender) => node_id!(lrender); attr!("style", "dashed")),
                        //     ),
                        //     Stmt::Edge(edge!(node_id!(nrender) => node_id!(rrender))),
                        // ];
                        // ss.extend(l_es);
                        // ss.extend(r_es);
                        // ss
                    }
                }
            }
        }
    }
    let ss = print_bdd_helper(mgr, map, Some((ptr, 0)), &mut VecDeque::new(), &mut vec![], &mut vec![]);
    let mut g = Graph::DiGraph {
        id: Id::Plain("bdd".to_string()),
        strict: true,
        stmts: ss,
    };
    g
    // format!("{}{}", if ptr.is_compl() { "!" } else { "" }, s)
}
pub fn to_string(mgr: &BddManager, map: &HashMap<VarLabel, VarLabel>, ptr: BddPtr) -> String {
    print(render(mgr, map, ptr), &mut PrinterContext::default())
}

mod test_graphviz {
    use super::*;

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

        let a_dot : Graph = render(&mgr, &map, a);


        println!("{}", mgr.print_bdd_lbl(a_or_b_or_c,&map,));
        println!("{}", to_string(&mgr,&map, a_or_b_or_c,));
        todo!();
        let expected = "strict digraph  {
            3[style=filled,fillcolor=white]
            2 -> 3 [style=dashed]
            2 -> T
            4[style=filled,fillcolor=white]
            3 -> 4 [style=dashed]
            3 -> T
            T[shape=rectangle]
            4 -> F [style=dashed]
            4 -> T
            F[shape=rectangle]
        }";
    }

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
    #[ignore]
    fn test_dot_structured() {
        let mut mgr = BddManager::new_default_order(4);
        let map = HashMap::new();
        let a = mgr.new_var();
        let a = mgr.var(a, true);
        let b = mgr.new_var();
        let b = mgr.var(b, true);
        let c = mgr.new_var();
        let c = mgr.var(c, true);
        let d = mgr.new_var();
        let d = mgr.var(d, true);
        let ab = mgr.and(a, b);
        let cd = mgr.and(c, d);
        let acd = mgr.and(a, cd);
        let xor_ab_acd = mgr.xor(ab, acd);
        let xor_ab_acd_d = mgr.xor(xor_ab_acd, d);

        println!("{}", to_string(&mgr,&map, b));
        todo!();
        let expected = "strict digraph  {
            \"3t\"[label=\"3\",style=filled,fillcolor=white]
            \"2t\"[label=\"2\",style=filled,fillcolor=white]
            \"2f\"[label=\"2\",style=filled,fillcolor=lightgray]
            \"1t\"[label=\"1\",style=filled,fillcolor=white]
            \"1f\"[label=\"1\",style=filled,fillcolor=lightgray]
            \"3t\" -> \"2t\"[style=solid]
            \"3t\" -> \"2f\"[style=dotted]
            \"2t\" -> \"1t\"[style=solid]
            \"2t\" -> \"1f\"[style=dotted]
            \"2f\" -> \"1t\"[style=dotted]
            \"2f\" -> \"1f\"[style=solid]
            \"1t\" -> \"T\"[style=solid]
            \"1t\" -> \"T\"[style=dotted]
            \"1f\" -> \"T\"[style=dotted]
            \"1f\" -> \"F\"[style=solid]
            T[shape=rectangle]
            F[shape=rectangle]
        }";

        println!("{}", to_string(&mgr,&map, b));
        todo!();
        let expected = "strict digraph  {
            \"3t\"[style=filled,fillcolor=white]
            \"2t\"[style=filled,fillcolor=white]
            \"2f\"[style=filled,fillcolor=lightgray]
            \"1t\"[style=filled,fillcolor=white]
            \"1f\"[style=filled,fillcolor=lightgray]
            \"3t\" -> \"2t\"[style=solid]
            \"3t\" -> \"2f\"[style=dotted]
            \"2t\" -> \"1t\"[style=solid]
            \"2t\" -> \"1f\"[style=dotted]
            \"2f\" -> \"1t\"[style=dotted]
            \"2f\" -> \"1f\"[style=solid]
            \"1t\" -> \"T\"[style=solid]
            \"1t\" -> \"T\"[style=dotted]
            \"1f\" -> \"T\"[style=dotted]
            \"1f\" -> \"F\"[style=solid]
            T[shape=rectangle]
            F[shape=rectangle]
        }";
    }
}
