// use super::btree::BTree;
use crate::util::hypergraph::cover::partitions;
use core::fmt::Debug;
use indexmap::set::IndexSet;
use itertools::Itertools;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashSet;
use std::convert::TryInto;
use std::fmt;
use std::hash::{Hash, Hasher};

pub mod clustergraph;
pub use clustergraph::*;
pub mod cover;
pub use cover::*;
pub mod hgraph;
pub use hgraph::*;

use cover::{AllCovers, Cover};

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

#[derive(Clone, PartialEq, Eq)]
pub struct Edge<V>(pub IndexSet<V>)
where
    V: Eq + Hash;

impl<V> Hash for Edge<V>
where
    V: Eq + Hash,
{
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        let mut hashes = self.0.iter().map(calculate_hash).collect_vec();
        hashes.sort();
        let hashstr = hashes.into_iter().map(|x| x.to_string()).join("");
        hashstr.hash(state);
    }
}

impl<V: Eq + Hash + fmt::Debug> Debug for Edge<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str("E")?;
        f.debug_set().entries(self.0.iter()).finish()
    }
}

impl<T, const N: usize> From<[T; N]> for Edge<T>
where
    T: Eq + Hash,
{
    fn from(arr: [T; N]) -> Self {
        Edge(IndexSet::from(arr))
    }
}
impl<V> Edge<V>
where
    V: Debug + Eq + Hash + Clone,
{
    pub fn show_compact(&self) -> String {
        self.0.iter().map(|x| format!("{:?}", x)).join("")
    }

    pub fn cover(edges: &IndexSet<Edge<V>>) -> IndexSet<V> {
        edges.iter().map(|x| x.0.clone()).flatten().collect()
    }
}

#[derive(Clone, Copy, Hash, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Rank(pub usize);

pub trait Hypergraph
where
    <Self as Hypergraph>::Vertex: Clone + Debug + Eq + Hash,
{
    type Vertex;
    fn new(vs: IndexSet<Self::Vertex>, es: IndexSet<Edge<Self::Vertex>>) -> Self;
    fn vertices(&self) -> &IndexSet<Self::Vertex>;
    fn hyperedges(&self) -> std::vec::IntoIter<Edge<Self::Vertex>>;
    fn insert_edge(&mut self, edge: Edge<Self::Vertex>) -> bool;
    fn insert_edges(&mut self, es: IndexSet<Edge<Self::Vertex>>) {
        for e in es {
            self.insert_edge(e);
        }
    }

    fn insert_vertex(&mut self, v: Self::Vertex) -> bool;
    fn insert_vertices(&mut self, vs: IndexSet<Self::Vertex>) {
        for v in vs {
            self.insert_vertex(v);
        }
    }

    /// cut a vertex out of the hypergraph
    fn edgecuts_ranked(&self) -> Vec<(Edge<Self::Vertex>, Rank)> {
        let all_covers = self.covers();
        self.hyperedges()
            .map(|edge| {
                let mut simulation = all_covers.clone();
                simulation.remove_edge(&edge);
                (edge.clone(), Rank(simulation.size()))
            })
            .collect()
    }

    fn edgecuts_sorted(&self) -> Vec<(Edge<Self::Vertex>, Rank)> {
        let mut sorted_edges = self.edgecuts_ranked();
        sorted_edges.sort_by(|(_, a), (_, b)| b.cmp(&a));
        sorted_edges
    }
    fn covers(&self) -> AllCovers<Self::Vertex> {
        AllCovers::from_edges(self.hyperedges())
    }
    fn from_cover(c: Cover<Self::Vertex>) -> Self
    where
        Self: Sized,
    {
        <Self as Hypergraph>::new(c.cover, c.edges)
    }

    fn size(&self) -> usize {
        self.hyperedges().count()
    }

    fn order(&self) -> usize {
        self.vertices().len()
    }
    /// return the (min) width of intersecting edges in the hypergraph
    fn width(&self) -> usize {
        self.widths().0
    }
    /// return (min, max) of sizes of the set of intersecting edges in the hypergraph
    fn widths(&self) -> (usize, usize) {
        self.covers()
            .covers
            .iter()
            .fold((usize::MAX, usize::MIN), |(mn, mx), cover| {
                let w = cover.edges.len();
                (if mn > w { w } else { mn }, if mx > w { mx } else { w })
            })
    }

    // TODO: all following commented code comes from the pre-trait implementation:
    // -------------------------------------------------------------------------------------
    // /// finds all edges that are in both part1 and part2
    // pub fn get_cut_edges(&self, part1: &Vec<T>, part2: &Vec<T>) -> Vec<IndexSet<T>> {
    //     let mut r = Vec::new();
    //     for e in self.hyperedges.iter() {
    //         let contains_part1 = part1.iter().any(|i| e.contains(i));
    //         let contains_part2 = part2.iter().any(|i| e.contains(i));
    //         if contains_part1 && contains_part2 {
    //             r.push(e.clone())
    //         }
    //     }
    //     r
    // }
    // /// cut a vertex out of the hypergraph
    // pub fn cut_vertex(&mut self, v: &T) -> bool {
    //     let v_in_vertices = self.vertices.remove(v);
    //     if !v_in_vertices {
    //         false
    //     } else {
    //         let cache = Self::cache_from(&self.vertices, &self.hyperedges);
    //         for ix in (*cache.get(v).unwrap()).iter() {
    //             let v_in_edge = self.hyperedges[*ix].remove(v); // FIXME: this can leave empty and duplicate edges! Probably Vec is the wrong datastructure.
    //             assert!(v_in_edge, "A vertex was removed that was not in the edgeset?? Impossible! Definitely, file this as a bug");
    //         }
    //         let new_edges = dedupe_indexsets(self.hyperedges.clone())
    //             .into_iter()
    //             .filter(|s| !s.is_empty())
    //             .collect();
    //         self.hyperedges = new_edges;
    //         // self.assoc_cache.remove(v);
    //         true
    //     }
    // }
    // pub fn count_cut_edges(&self, part1: &Vec<T>, part2: &Vec<T>) -> usize {
    //     let mut r = 0;
    //     for e in self.edges().iter() {
    //         let contains_part1 = part1.iter().any(|i| e.contains(i));
    //         let contains_part2 = part2.iter().any(|i| e.contains(i));
    //         if contains_part1 && contains_part2 {
    //             r += 1;
    //         }
    //     }
    //     r
    // }
    // pub fn get_dtree(&self) -> BTree<(), T> {
    // follow Kernighan-Lin algorithm for finding a minimum-cut 2-partition
    // todo!()
    // }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ADTree<V>
where
    V: Eq + Hash + Clone,
{
    Node {
        l: Box<ADTree<V>>,
        r: Box<ADTree<V>>,
        edges: IndexSet<Edge<V>>,
    },
    Leaf {
        vars: IndexSet<V>,
    },
}
impl<V> ADTree<V>
where
    V: Eq + Hash + Clone,
{
    pub fn leaf(vars: IndexSet<V>) -> Self {
        Self::Leaf { vars }
    }
    pub fn left(&self) -> Option<&ADTree<V>> {
        match &self {
            Self::Node { l, .. } => Some(l),
            Self::Leaf { .. } => None,
        }
    }
    pub fn right(&self) -> Option<&ADTree<V>> {
        match &self {
            Self::Node { r, .. } => Some(r),
            Self::Leaf { .. } => None,
        }
    }
}

fn difference<T: Hash + Eq + Clone>(l: &IndexSet<T>, r: &IndexSet<T>) -> IndexSet<T> {
    l.difference(r).map(|x| x.clone()).collect()
}
pub fn smallest_partition_of<H: Hypergraph<Vertex = V>, V: Eq + Hash + Clone + Debug + 'static>(
    nparts: usize,
    covers: AllCovers<V>,
    e: &IndexSet<Edge<V>>,
) -> Option<(H, IndexSet<Edge<V>>, H)> {
    // println!("{:?}", covers);
    let mut estar = vec![];
    for (cutset, sim) in partitions(&covers, nparts) {
        if cutset.is_disjoint(e) {
            let partition_sizes = sim.covers.iter().map(|x| x.cover.len()).collect_vec();
            let total = partition_sizes.iter().sum::<usize>() as f64;
            let n = nparts as f64;
            let var = partition_sizes
                .iter()
                .map(|x| (1.0 / n) * (((*x as f64) / total) - (total / n)).powf(2.0))
                .sum::<f64>();
            // println!("psize: {:?}", partition_sizes);
            // println!("total: {:?}", total);
            // println!("n    : {:?}", n);
            // println!("var  : {:?}", var);
            estar.push((cutset, sim, var));
        }
    }
    estar.sort_by(|(_, _, a), (_, _, b)| a.total_cmp(b));
    match &estar[..] {
        [] => None,
        [(cs, sim, _), ..] => {
            let [cleft, cright]: [&Cover<V>; 2] =
                sim.covers.iter().collect_vec().try_into().unwrap();
            let left = <H as Hypergraph>::from_cover(cleft.clone());
            let mut right = <H as Hypergraph>::from_cover(cright.clone());

            // anything missing has to go somewhere, just set this as the right side for now.
            let all_vtxs: IndexSet<V> = covers.cover();
            let cvr_vtxs: IndexSet<V> = sim
                .covers
                .iter()
                .map(|c| c.cover.clone())
                .flatten()
                .collect();
            for v in all_vtxs.difference(&cvr_vtxs) {
                right.insert_vertex(v.clone());
            }
            Some((left, cs.clone(), right))
        }
    }
}

pub fn remaining_cover_over<H: Hypergraph<Vertex = V>, V: Eq + Hash + Clone + Debug + 'static>(
    h: &H,
    seen_edges: &IndexSet<Edge<V>>,
) -> (H, IndexSet<Edge<V>>, H) {
    let nodes = h.vertices();
    let all_edges = h.hyperedges().collect::<IndexSet<_>>();
    let remaining_edges = difference(&all_edges, seen_edges);
    // do a best-effort for finding a cover with the remaining edges
    let mut cuts = IndexSet::new();
    for edge in &remaining_edges {
        cuts.insert(edge.clone());
        let cover = Edge::cover(&cuts);
        let diff: IndexSet<_> = nodes.difference(&cover).collect();
        if diff.is_empty() {
            break;
        }
    }
    let n = nodes.len();

    // ultimately, just split the vars between left and right
    let left_nodes = nodes.iter().take(n / 2).cloned().collect();
    let left_edges: IndexSet<_> = difference(&remaining_edges, &cuts)
        .into_iter()
        .take(n / 2)
        .collect();
    let left = <H as Hypergraph>::new(left_nodes, left_edges);

    let right_nodes = nodes.iter().skip(n / 2).cloned().collect();
    let right_edges: IndexSet<_> = difference(&remaining_edges, &cuts)
        .into_iter()
        .skip(n / 2)
        .collect();
    let right = <H as Hypergraph>::new(right_nodes, right_edges);

    (left, cuts, right)
}

pub fn hg2dt_h<H: Hypergraph<Vertex = V> + Debug, V: Eq + Hash + Clone + Debug + 'static>(
    h: H,
    seen_edges: IndexSet<Edge<V>>,
    depth: usize,
) -> ADTree<V> {
    let indent = " ".repeat(depth * 2);
    let vertices: IndexSet<V> = h.vertices().clone();
    let all_edges: IndexSet<Edge<V>> = h.hyperedges().map(|x| x.clone()).collect();
    // println!("{}", "-".repeat(depth*3));
    // println!("{}> vertices: {:?}", indent, vertices);
    println!("{}> edges    : {:?}", indent, all_edges);

    if vertices.len() == 1 {
        let vars: IndexSet<V> = vertices;
        // println!("{}==> leaf: {:?}", indent, vars);
        ADTree::Leaf { vars }
    } else {
        let candidates = AllCovers::from_edges(all_edges.difference(&seen_edges).cloned());
        // println!("");
        // println!("candidates: {:?}", candidates);
        let (left, estar, right) = match smallest_partition_of::<H, V>(2, candidates, &seen_edges) {
            Some(xs) => xs,
            None => remaining_cover_over(&h, &seen_edges),
        };
        // println!("{}estar: {:?}", indent, estar);
        // println!("{}left : {:?}", indent, left);
        // println!("{}right: {:?}", indent, right);

        let mut next_seen = seen_edges.clone();
        next_seen.extend(estar.clone());
        let tleft = hg2dt_h(left, next_seen.clone(), depth + 1);
        let tright = hg2dt_h(right, next_seen, depth + 1);
        ADTree::Node {
            l: Box::new(tleft),
            r: Box::new(tright),
            edges: estar.clone(),
        }
    }
}
/// FIXME: assumes a we are starting from a single cover.
pub fn hg2dt<H: Hypergraph<Vertex = V> + Debug + Clone, V: Eq + Hash + Clone + Debug + 'static>(
    h: &H,
) -> ADTree<V> {
    let howned: H = h.clone();
    hg2dt_h(howned, Default::default(), 0)
}
