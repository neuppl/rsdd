// use super::btree::BTree;
use core::fmt::Debug;
use itertools::Itertools;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};

pub mod clustergraph;
pub mod cover;
pub mod hgraph;

use cover::AllCovers;

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Edge<V>(HashSet<V>)
where
    V: Clone + Debug + PartialEq + Eq + Hash;

impl<V> Hash for Edge<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
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

impl<V> Edge<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    pub fn from(i: &[V]) -> Self {
        Edge(i.iter().cloned().collect())
    }
}

#[derive(Clone, Copy, Hash, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Rank(pub usize);

pub trait Hypergraph
where
    <Self as Hypergraph>::Vertex: Clone + Debug + PartialEq + Eq + Hash,
{
    type Vertex;
    fn vertices(&self) -> &HashSet<Self::Vertex>;
    fn hyperedges(&self) -> std::vec::IntoIter<Edge<Self::Vertex>>;
    fn insert_edge(&mut self, edge: Edge<Self::Vertex>) -> bool;
    fn insert_vertex(&mut self, v: Self::Vertex) -> bool;

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
    // pub fn get_cut_edges(&self, part1: &Vec<T>, part2: &Vec<T>) -> Vec<HashSet<T>> {
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
    //         let new_edges = dedupe_hashsets(self.hyperedges.clone())
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
