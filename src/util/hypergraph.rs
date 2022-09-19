// TODO: remove crate-level disable
#![allow(dead_code, unused_imports, unused_variables)]

use crate::repr::cnf::Cnf;
use crate::repr::var_label::{Literal, VarLabel};
use core::fmt::{Debug, Formatter, Result};
use core::hash::Hash;
use petgraph::graph::{Node, UnGraph};
use std::collections::{HashMap, HashSet};

use crate::builder::var_order::VarOrder;

use super::btree::BTree;

#[derive(Clone)]
struct Hypergraph<T: Clone + Debug + PartialEq + Eq + Hash> {
    vertices: HashSet<T>,
    hyperedges: Vec<HashSet<T>>,
    assoc_cache: HashMap<T, HashSet<usize>>,
}

impl<T: Clone + Debug + PartialEq + Eq + Hash> Debug for Hypergraph<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("Hypergraph")
            .field("vertices", &self.vertices)
            .field("hyperedges", &self.hyperedges)
            .finish()
    }
}

impl<T: Clone + Debug + PartialEq + Eq + Hash> Hypergraph<T> {
    pub fn new_with_cache(
        vertices: HashSet<T>,
        hyperedges: Vec<HashSet<T>>,
        assoc_cache: HashMap<T, HashSet<usize>>,
    ) -> Hypergraph<T> {
        Hypergraph {
            vertices,
            hyperedges,
            assoc_cache,
        }
    }
    pub fn new(vertices: HashSet<T>, hyperedges: Vec<HashSet<T>>) -> Hypergraph<T> {
        let cache = Self::cache_from(&vertices, &hyperedges);
        Self::new_with_cache(vertices, hyperedges, cache)
    }
    /// assumes that hyperedges only includes elements from vertices
    fn cache_from(
        vertices: &HashSet<T>,
        hyperedges: &Vec<HashSet<T>>,
    ) -> HashMap<T, HashSet<usize>> {
        let mut assoc_cache: HashMap<T, HashSet<usize>> = HashMap::new();
        for (ix, vs) in hyperedges.iter().enumerate() {
            for v in vs.clone() {
                match assoc_cache.get_mut(&v) {
                    None => {
                        assoc_cache.insert(v, HashSet::from([ix]));
                    }
                    Some(ixs) => {
                        ixs.insert(ix);
                    }
                }
            }
        }
        assoc_cache
    }
    pub fn edges(&self) -> &Vec<HashSet<T>> {
        &self.hyperedges
    }
    pub fn edges_for(&self, node: &T) -> Option<Vec<&HashSet<T>>> {
        let edge_ixs = self.assoc_cache.get(node)?;
        Some(
            self.hyperedges
                .iter()
                .enumerate()
                .filter(|(i, _)| edge_ixs.contains(i))
                .map(|(_, e)| e)
                .collect::<Vec<&HashSet<T>>>(),
        )
    }
    pub fn size(&self) -> usize {
        self.hyperedges.len()
    }
    pub fn order(&self) -> usize {
        self.vertices.len()
    }
    /// finds all edges that are in both part1 and part2
    fn get_cut_edges(&self, part1: &Vec<T>, part2: &Vec<T>) -> Vec<HashSet<T>> {
        let mut r = Vec::new();
        for e in self.hyperedges.iter() {
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
        for e in self.hyperedges.iter() {
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

fn dedupe_hashsets<T: Hash + Eq>(hss: Vec<HashSet<T>>) -> Vec<HashSet<T>> {
    let mut cache: HashMap<usize, Vec<HashSet<T>>> = HashMap::new();
    for cur in hss {
        let len = cur.len();
        match cache.get_mut(&len) {
            None => {
                cache.insert(len, vec![cur]);
            }
            Some(cached_hss) => {
                let mut add = true;
                for hs in cached_hss.iter() {
                    if hs.symmetric_difference(&cur).next().is_none() {
                        add = false;
                        break;
                    }
                }
                if add {
                    cached_hss.push(cur);
                }
            }
        }
    }
    cache.into_values().flatten().collect()
}

fn from_cnf(cnf: &Cnf) -> Hypergraph<VarLabel> {
    let mut vars: HashSet<VarLabel> = HashSet::new();
    let mut hedges: Vec<HashSet<VarLabel>> = vec![];

    for clause in cnf.clauses() {
        let hedge = clause
            .iter()
            .map(|l| {
                let var = l.get_label();
                vars.insert(var);
                var
            })
            .collect::<HashSet<VarLabel>>();
        hedges.push(hedge);
    }

    Hypergraph::new(vars, dedupe_hashsets(hedges))
}

mod test {
    use super::*;
    use petgraph::dot::{Config, Dot};

    #[test]
    fn cnf_to_ig() {
        let v = vec![
            vec![
                Literal::new(VarLabel::new(0), true),
                Literal::new(VarLabel::new(1), false),
                Literal::new(VarLabel::new(2), true),
            ],
            vec![
                Literal::new(VarLabel::new(1), false),
                Literal::new(VarLabel::new(2), true),
            ],
            vec![
                Literal::new(VarLabel::new(2), true),
                Literal::new(VarLabel::new(3), false),
            ],
            vec![
                Literal::new(VarLabel::new(2), true),
                Literal::new(VarLabel::new(4), false),
            ],
            vec![Literal::new(VarLabel::new(5), true)],
        ];
        let cnf = Cnf::new(v);
        let ig = cnf.interaction_graph();
        println!("{:?}", Dot::with_config(&ig, &[Config::EdgeNoLabel]));
        let nodes = ig
            .raw_nodes()
            .iter()
            .map(|n| n.weight.clone())
            .collect::<Vec<VarLabel>>();

        fn var(x: u64) -> VarLabel {
            VarLabel::new(x)
        }
        let expected: Vec<HashSet<VarLabel>> = vec![
            HashSet::from([var(5)]),
            HashSet::from([var(1), var(2)]),
            HashSet::from([var(0), var(1), var(2)]),
            HashSet::from([var(2), var(3)]),
            HashSet::from([var(2), var(4)]),
        ];

        let g = from_cnf(&cnf);
        assert_eq!(
            expected.len(),
            g.hyperedges.len(),
            "Expected {} edges, but found {} in {:?}",
            expected.len(),
            g.hyperedges.len(),
            g.hyperedges
        );
        for es in expected {
            assert!(
                g.hyperedges
                    .iter()
                    .any(|hes| hes.symmetric_difference(&es).next().is_none()),
                "Did not find {es:?} in {:?}",
                g.hyperedges
            );
        }
    }
}
