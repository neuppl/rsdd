//! A simple DAG representation used for rendering all DAG-like representations
//! (vtrees, dtrees, SDDs, BDDs, etc

use dot::Style;
use std::{io::Write, borrow::Cow};


/// An index that uniquely identifies a node in the graph
#[derive(Clone, Debug, Copy)]
pub struct NodeIndex(usize);

#[derive(Clone, Debug)]
pub struct Node {
    /// an HTML label
    label: String,
    idx: NodeIndex 
}

impl Node {
    pub fn new(label: String, idx: NodeIndex) -> Node {
        Node { label, idx }
    }
}

#[derive(Clone, Debug, Copy)]
pub struct Edge { 
    source: NodeIndex,
    target: NodeIndex,
    style: Style
}

impl Edge { 
    pub fn new(source: NodeIndex, target: NodeIndex, style: Style) -> Edge {
        Edge { source, target, style }
    }
}

#[derive(Clone, Debug)]
pub struct DAG { 
    edge_list: Vec<Edge>,
    node_list: Vec<Node>,
    num_nodes: usize
}

impl<'a> dot::Labeller<'a, Node, Edge> for DAG {
    fn graph_id(&'a self) -> dot::Id<'a> { dot::Id::new("").unwrap() }

    fn node_id(&'a self, n: &Node) -> dot::Id<'a> {
        dot::Id::new(format!("N{}", n.idx.0)).unwrap()
    }

    fn node_label<'b>(&'b self, n: &Node) -> dot::LabelText<'b> {
        dot::LabelText::HtmlStr(Cow::Owned(n.label.clone()))
    }

    fn edge_style(&'a self, e: &Edge) -> Style {
        e.style
    }
}

impl<'a> dot::GraphWalk<'a, Node, Edge> for DAG {
    fn nodes(&self) -> dot::Nodes<'a, Node> {
        Cow::Owned(self.node_list.clone())
    }

    fn edges(&'a self) -> dot::Edges<'a, Edge> {
        Cow::Borrowed(&self.edge_list[..])
    }

    fn source(&self, e: &Edge) -> Node { self.node_list[e.source.0].clone() }

    fn target(&self, e: &Edge) -> Node { self.node_list[e.target.0].clone() }
}



impl DAG {
    pub fn new() -> DAG { 
        DAG { edge_list: Vec::new(), node_list: Vec::new(), num_nodes: 0 }
    }

    pub fn new_node(&mut self, label: String) -> NodeIndex {
        let n = NodeIndex(self.num_nodes);
        self.node_list.push(Node::new(label, n));
        self.num_nodes += 1;
        return n;
    }

    pub fn add_edge(&mut self, source: NodeIndex, target: NodeIndex, style: Style) -> () {
        self.edge_list.push(Edge::new(source, target, style))
    }

    pub fn render_to<W: Write>(&self, output: &mut W) {
        dot::render(self, output).unwrap()
    }
}