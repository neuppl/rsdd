use core::fmt::Debug;
use core::hash::Hash;
use std::collections::HashSet;

use crate::builder::var_order::VarOrder;

use super::btree::BTree;

#[derive(Clone, Debug)]
struct Hypergraph<T: Clone + Debug + PartialEq + Eq + Hash> {
    vertices: Vec<T>,
    hyper_edges: Vec<HashSet<T>>
}

impl<T: Clone + Debug + PartialEq + Eq + Hash> Hypergraph<T> {
    pub fn new(vertices: Vec<T>, hyper_edges: Vec<HashSet<T>>) -> Hypergraph<T> {
        Hypergraph { vertices, hyper_edges }
    }

    /// finds all edges that are in both part1 and part2
    fn get_cut_edges(&self, part1: &Vec<T>, part2: &Vec<T>) -> Vec<HashSet<T>> {
        let mut r = Vec::new();
        for e in self.hyper_edges.iter() {
            let contains_part1 = part1.iter().any(|i| e.contains(i));
            let contains_part2 = part2.iter().any(|i| e.contains(i));
            if contains_part1 && contains_part2 {
                r.push(e.clone())
            }
        }
        r
    }

    fn count_cut_edges(&self, part1: &Vec<T>, part2: &Vec<T>) -> usize {
        let mut r = 0;
        for e in self.hyper_edges.iter() {
            let contains_part1 = part1.iter().any(|i| e.contains(i));
            let contains_part2 = part2.iter().any(|i| e.contains(i));
            if contains_part1 && contains_part2 {
                r = r + 1;
            }
        }
        r
    }

    // pub fn get_dtree(&self) -> BTree<(), T> {
        // follow Kernighan-Lin algorithm for finding a minimum-cut 2-partition
        // todo!()
    // }
}