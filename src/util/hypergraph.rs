use core::fmt::Debug;
use core::hash::Hash;
use std::collections::HashSet;

use super::btree::BTree;

// struct SwapIter<T> {
//     r: Vec<T>,
//     l: Vec<T>,
//     l_idx: usize, 
//     r_idx: usize
// }

// // Implement `Iterator` for `Fibonacci`.
// // The `Iterator` trait only requires a method to be defined for the `next` element.
// impl<'a, T> Iterator for SwapIter<'a, T> {
//     // We can refer to this type using Self::Item
//     type Item = (Vec<T>, Vec<T>);
    
//     fn next(&mut self) -> Option<Self::Item> {
//         if l_idx >= self.l.len() {
//             return None;
//         }
//         let mut r = self.r.clone();
//         let mut l = self.l.clone();

//         self.curr = self.next;
//         self.next = new_next;

//         // Since there's no endpoint to a Fibonacci sequence, the `Iterator` 
//         // will never return `None`, and `Some` is always returned.
//         Some()
//     }
// }



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

    pub fn get_dtree(&self) -> BTree<(), T> {
        // follow Kernighan-Lin algorithm for finding a minimum-cut 2-partition

        // choose an initial arbitrary equal partitioning of the vertices
        let mut l = self.vertices.clone();
        let r = l.split_off(l.len() / 2);

        let mut best_l = l.clone();
        let mut best_r = r.clone();
        let cur_e = self.get_cut_edges(&best_l, &best_r);

        // consider all swaps


        todo!()
    }
}