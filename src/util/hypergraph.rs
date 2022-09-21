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

#[derive(Clone, Debug)]
pub struct Hypergraph<T: Clone + Debug + PartialEq + Eq + Hash> {
    vertices: HashSet<T>,
    hyperedges: Vec<HashSet<T>>,
}

impl<T: Clone + Debug + PartialEq + Eq + Hash> Hypergraph<T> {
    pub fn new(vertices: HashSet<T>, hyperedges: Vec<HashSet<T>>) -> Hypergraph<T> {
        Hypergraph {
            vertices,
            hyperedges,
        }
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
    pub fn vertices(&self) -> &HashSet<T> {
        &self.vertices
    }
    pub fn edges(&self) -> Vec<&HashSet<T>> {
        self.hyperedges.iter().filter(|hs| hs.len() > 0).collect()
    }
    pub fn edges_for(&self, node: &T) -> Option<Vec<&HashSet<T>>> {
        let cache = Self::cache_from(&self.vertices, &self.hyperedges);
        let edge_ixs = cache.get(node)?;
        Some(
            self.hyperedges
                .iter()
                .enumerate()
                .filter(|(i, _)| edge_ixs.contains(i))
                .map(|(_, e)| e)
                .collect::<Vec<&HashSet<T>>>(),
        )
    }

    pub fn edges_to_covers(es: Vec<&HashSet<T>>) -> Vec<(HashSet<T>, Vec<&HashSet<T>>)> {
        let mut overlaps: Vec<(HashSet<T>, Vec<&HashSet<T>>)> = vec![];
        for e in es {
            let os: Vec<(usize, (HashSet<T>, Vec<&HashSet<T>>))> = overlaps
                .iter()
                .enumerate()
                .filter(|(_, o)| !o.0.is_disjoint(&e))
                .map(|(i, (l, r))| (i, (l.clone(), r.clone())))
                .collect();

            if os.len() == 0 {
                overlaps.push((e.clone(), vec![e]))
            } else {
                let mut newc = e.clone();
                let mut newes = vec![e];
                let mut diff = 0;
                for (i, (c, ess)) in os {
                    for v in c.iter() {
                        newc.insert(v.clone());
                    }
                    for es in ess {
                        newes.push(es);
                    }
                    overlaps.remove(i - diff);
                    diff += 1;
                }
                overlaps.push((
                    newc,
                    dedupe_hashset_refs(newes.into_iter().filter(|s| s.len() > 0).collect()),
                ));
            }
        }
        overlaps
    }

    pub fn covers(&self) -> Vec<(HashSet<T>, Vec<&HashSet<T>>)> {
        Self::edges_to_covers(self.edges())
    }

    pub fn size(&self) -> usize {
        self.edges().len()
    }
    pub fn order(&self) -> usize {
        self.vertices().len()
    }
    pub fn width(&self) -> usize {
        self.widths().0
    }
    /// return (min, max) of sizes of the set of intersecting edges in the hypergraph
    pub fn widths(&self) -> (usize, usize) {
        self.covers()
            .iter()
            .fold((usize::MAX, usize::MIN), |(mn, mx), (_, es)| {
                let w = es.len();
                (if mn > w { w } else { mn }, if mx > w { mx } else { w })
            })
    }

    /// finds all edges that are in both part1 and part2
    pub fn get_cut_edges(&self, part1: &Vec<T>, part2: &Vec<T>) -> Vec<HashSet<T>> {
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
    /// add an edge to the hypergraph. Returns false if the edge is already in the hypergraph
    pub fn insert_edge(&mut self, edge: &HashSet<T>) -> bool {
        let new_verts: HashSet<T> = edge.difference(&self.vertices).cloned().collect();
        if new_verts.len() > 0 {
            let vertices = self.vertices().union(&new_verts).cloned().collect();
            self.vertices = vertices;
        }

        let next_ix = self.hyperedges.len();
        match self.hyperedges.clone().iter().find(|e| e == &edge) {
            Some(_) => return false,
            None => self.hyperedges.push(edge.clone()),
        }
        true
    }

    /// cut a vertex out of the hypergraph
    pub fn cut_vertex(&mut self, v: &T) -> bool {
        let v_in_vertices = self.vertices.remove(v);
        if !v_in_vertices {
            false
        } else {
            let cache = Self::cache_from(&self.vertices, &self.hyperedges);
            for ix in (*cache.get(v).unwrap()).iter() {
                let v_in_edge = self.hyperedges[*ix].remove(v); // FIXME: this can leave empty and duplicate edges! Probably Vec is the wrong datastructure.
                assert!(v_in_edge, "A vertex was removed that was not in the edgeset?? Impossible! Definitely, file this as a bug");
            }
            let new_edges = dedupe_hashsets(self.hyperedges.clone())
                .into_iter()
                .filter(|s| s.len() > 0)
                .collect();
            self.hyperedges = new_edges;
            // self.assoc_cache.remove(v);
            true
        }
    }
    pub fn count_cut_edges(&self, part1: &Vec<T>, part2: &Vec<T>) -> usize {
        let mut r = 0;
        for e in self.edges().iter() {
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

fn dedupe_hashset_refs<T: Hash + Eq>(hss: Vec<&HashSet<T>>) -> Vec<&HashSet<T>> {
    let mut cache: HashMap<usize, Vec<&HashSet<T>>> = HashMap::new();
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

pub fn from_cnf(cnf: &Cnf) -> Hypergraph<VarLabel> {
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
    fn cnf_to_hg() {
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

    #[test]
    fn insert_edge_cut_vertex() {
        let mut hg: Hypergraph<u64> = Hypergraph::new(HashSet::new(), Vec::new());
        #[rustfmt::skip]
        let e1 = HashSet::from([   3,       0, 1]);
        let e2 = HashSet::from([2, 3, 5, 7]);
        let e3 = HashSet::from([2]);

        for e in &[e1.clone(), e2.clone(), e3.clone()] {
            hg.insert_edge(e);
        }
        assert_eq!(hg.vertices, HashSet::from([0, 1, 3, 5, 7, 2]));
        assert_eq!(
            hg.hyperedges,
            Vec::from([e1.clone(), e2.clone(), e3.clone()])
        );

        hg.insert_edge(&e2);
        assert_eq!(
            hg.hyperedges,
            Vec::from([e1.clone(), e2.clone(), e3.clone()])
        );

        hg.cut_vertex(&1); // order gets messed up by dedupe_hashsets
        for s in [HashSet::from([0, 3]), e2.clone(), e3.clone()] {
            assert!(hg.hyperedges.iter().any(|e| e == &s));
        }

        hg.cut_vertex(&3); // order gets messed up by dedupe_hashsets
        for s in [HashSet::from([0]), HashSet::from([2, 5, 7]), e3.clone()] {
            assert!(hg.hyperedges.iter().any(|e| e == &s));
        }

        hg.cut_vertex(&0); // order gets messed up by dedupe_hashsets
        for s in [HashSet::from([2, 5, 7]), HashSet::from([2])] {
            assert!(hg.hyperedges.iter().any(|e| e == &s));
        }
        assert_eq!(hg.vertices, HashSet::from([2, 5, 7]));

        let edges_api = hg.edges();
        for s in [HashSet::from([2, 5, 7]), HashSet::from([2])] {
            assert!(hg.hyperedges.iter().any(|e| e == &s));
        }
        hg.cut_vertex(&5);
        hg.cut_vertex(&7);
        assert_eq!(hg.hyperedges, Vec::from([HashSet::from([2])]));
        assert_eq!(hg.vertices, HashSet::from([2]));
    }

    #[test]
    fn test_width() {
        let mut hg: Hypergraph<u64> = Hypergraph::new(HashSet::new(), Vec::new());
        // cover 1
        #[rustfmt::skip]
        let e1 = HashSet::from([          3, 1, 0]);
        let e2 = HashSet::from([21, 5, 7, 3]);
        let e3 = HashSet::from([21]);
        let e4 = HashSet::from([21, 5]);
        let cover1 = vec![e1.clone(), e2.clone(), e3.clone(), e4.clone()];

        // cover 2
        let e5 = HashSet::from([11, 10]);
        let e6 = HashSet::from([11]);
        let e7 = HashSet::from([11, 12]);
        let cover2 = vec![e5.clone(), e6.clone(), e7.clone()];

        for e in [cover1.clone(), cover2.clone()].iter().flatten() {
            hg.insert_edge(e);
        }

        let (min_width, max_width) = hg.widths();

        println!("vertex\t: edges cut\t: mn width\t: mx width");
        println!("none\t: {}\t\t: {}\t\t: {}", 0_usize, min_width, max_width);

        // uncomment when you have verify these by hand
        let stats: HashMap<u64, (usize, (usize, usize))> = HashMap::from([
            (0, (1, (3, 4))),
            (1, (1, (3, 4))),
            (3, (2, (1, 3))),
            (5, (2, (3, 3))), // thinks this should be (5, 2, 3, 3)
            (7, (1, (3, 4))),
            (21, (3, (3, 3))),
            (11, (3, (1, 4))),
            (10, (1, (2, 4))),
            (12, (1, (2, 4))),
        ]);
        for v in hg.vertices() {
            let mut tmp = hg.clone();
            let cut_edges_for_v = tmp.edges_for(v).unwrap();
            let edges_cut = cut_edges_for_v.len();
            tmp.cut_vertex(v);

            let (min_width, max_width) = tmp.widths();
            println!("{v}\t: {}\t\t: {}\t\t: {}", edges_cut, min_width, max_width);
            let left = (edges_cut, (min_width, max_width));
            let right = *stats.get(v).unwrap();
            assert_eq!(left, right, "cut {}, new edges: {:?}", v, tmp.edges());
        }
    }
    #[test]
    fn test_heuristic() {
        // in this heuristic, we essentially want to take the cut with the max "potential of vertex cover".

        // cover 1
        #[rustfmt::skip]
        let e1 = HashSet::from([          3, 1, 0]);
        let e2 = HashSet::from([21, 5, 7, 3]);
        let e3 = HashSet::from([21]);
        let e4 = HashSet::from([21, 5]);
        let cover1 = vec![e1.clone(), e2.clone(), e3.clone(), e4.clone()];

        // cover 2
        let e5 = HashSet::from([11, 10]);
        let e6 = HashSet::from([11]);
        let e7 = HashSet::from([11, 12]);
        let cover2 = vec![e5.clone(), e6.clone(), e7.clone()];

        // Ideally, we want to split the graph into two partitions that are equivalently balanced.
        //
        // In these "glue" scenarios we have an edge which glues the two covers above:
        // - glue1 uses a vertex which spans all of cover 1
        // - glue2 uses a vertex which spans all of cover 2
        // - glue3 uses a vertex which minimally spans both covers
        let glue1 = HashSet::from([21, 10]);
        let glue2 = HashSet::from([11, 0]);
        let glue3 = HashSet::from([7, 10]);

        // In the "bridge" scenario, we include two edges which bridges both covers at 55.
        // Ideally, the heuristic can uncover that selecting 55 will produce the largest partition
        let br1pt1 = HashSet::from([21, 55]);
        let br1pt2 = HashSet::from([12, 55]);

        /// our heuristic should search the hypergraph in a single pass and construct an ordering on variables.
        fn heuristic(g: &Hypergraph<u64>) -> Vec<(u64, f64)> {
            let mut order: Vec<(u64, f64)> = vec![];
            for v in g.vertices() {
                let mut tmp = g.clone();
                tmp.cut_vertex(&v);
                // naive take 1
                let mx_width: f64 = tmp.widths().1 as f64;
                // naive take 2
                let mx_edge_size = tmp.edges().iter().map(|e| e.len()).max().unwrap() as f64;
                // expectation over edge sizes
                let sum_edge_size: f64 = tmp.edges().iter().map(|e| e.len() as f64).sum();
                let mean_edge_size = sum_edge_size / tmp.size() as f64;

                // for each variable
                // iterate through each node it would cut
                let edges_for_v = g.edges_for(v);
                let max_edge_size = edges_for_v.iter().map(|e| e.len()).max().unwrap() as f64;

                let (new_num_edges, total_acc) =
                    g.edges().iter().fold((0_f64, 0_f64), |(count, acc), e| {
                        let l = e.len() as f64;
                        if e.contains(&v) {
                            if l - 1.0 > 0.0 {
                                (count + 1.0, acc + 2_f64.powf(l - 1.0))
                            } else {
                                (count, acc)
                            }
                        } else {
                            (count + 1.0, acc + 2_f64.powf(l))
                        }
                    });
                let max_potential_combinations = total_acc / new_num_edges;

                // largest cover after cut heuristic
                let cut_cover_widths = g
                    .covers()
                    .iter()
                    .flat_map(|(cover, es)| {
                        let l = cover.len() as f64;
                        if !cover.contains(&v) {
                            vec![l]
                        } else {
                            let newes: Vec<HashSet<u64>> = es
                                .iter()
                                .cloned()
                                .map(|e| e.difference(&HashSet::from([*v])).cloned().collect())
                                .filter(|e: &HashSet<u64>| e.len() > 0)
                                .collect();
                            let cs = Hypergraph::edges_to_covers(newes.clone().iter().collect())
                                .iter()
                                .map(|(c, es)| es.len() as f64)
                                .collect();
                            cs
                        }
                    })
                    .collect::<Vec<f64>>();

                let avg_cut_cover_widths =
                    (-1.0) * cut_cover_widths.iter().sum::<f64>() / cut_cover_widths.len() as f64;
                let max_cut_cover_widths = (-1.0)
                    * cut_cover_widths
                        .iter()
                        .fold(f64::MIN, |acc, w| if acc > *w { acc } else { *w });
                let h = max_cut_cover_widths;
                order.push((v.clone(), h));
            }
            order.sort_by(|l, r| r.1.partial_cmp(&l.1).unwrap());
            order
        }
        fn topscore(ordering: &Vec<(u64, f64)>) -> Vec<(u64, f64)> {
            ordering
                .iter()
                .filter(|(_, s)| *s == ordering[0].1)
                .cloned()
                .collect()
        }

        // beginning with an empty hypergraph, we add in the two covers:
        let mut hg: Hypergraph<u64> = Hypergraph::new(HashSet::new(), Vec::new());
        for e in [cover1.clone(), cover2.clone()].iter().flatten() {
            hg.insert_edge(e);
        }

        println!("\nScenario: glue1 (want: 21 or 10)");
        let mut hg1 = hg.clone();
        hg1.insert_edge(&glue1.clone());
        let ordering = heuristic(&hg1);
        println!("{:?}", ordering);
        println!("{:?}", topscore(&ordering));
        for v in topscore(&ordering) {
            let mut tmp = hg1.clone();
            tmp.cut_vertex(&v.0);
            println!("{}: {:?}", v.0, tmp.covers());
        }
        let topvars: HashSet<u64> = topscore(&ordering).into_iter().map(|(v, _)| v).collect();
        assert!(!topvars.is_disjoint(&HashSet::from([21, 10])));

        println!("\nScenario: glue2 (want: 11 or 0)");
        let mut hg2 = hg.clone();
        hg2.insert_edge(&glue2.clone());
        let ordering = heuristic(&hg2);
        println!("{:?}", ordering);
        println!("{:?}", topscore(&ordering));
        for v in topscore(&ordering) {
            let mut tmp = hg1.clone();
            tmp.cut_vertex(&v.0);
            println!("{}: {:?}", v.0, tmp.covers());
        }
        let topvars: HashSet<u64> = topscore(&ordering).into_iter().map(|(v, _)| v).collect();
        assert!(!topvars.is_disjoint(&HashSet::from([11, 0])));

        println!("\nScenario: glue3 (want: 7 or 10)");
        let mut hg3 = hg.clone();
        hg3.insert_edge(&glue3.clone());
        let ordering = heuristic(&hg3);
        println!("{:?}", ordering);
        println!("{:?}", topscore(&ordering));
        for v in topscore(&ordering) {
            let mut tmp = hg1.clone();
            tmp.cut_vertex(&v.0);
            println!("{}: {:?}", v.0, tmp.covers());
        }
        let topvars: HashSet<u64> = topscore(&ordering).into_iter().map(|(v, _)| v).collect();
        assert!(!topvars.is_disjoint(&HashSet::from([10, 7])));

        println!("\nScenario: bridge (want: 21 or 55 or 12)");
        let mut hg4 = hg.clone();
        hg4.insert_edge(&br1pt1.clone());
        hg4.insert_edge(&br1pt2.clone());
        let ordering = heuristic(&hg4);
        println!("{:?}", ordering);
        println!("{:?}", topscore(&ordering));
        for v in topscore(&ordering) {
            let mut tmp = hg1.clone();
            tmp.cut_vertex(&v.0);
            println!("{}: {:?}", v.0, tmp.covers());
        }
        let topvars: HashSet<u64> = topscore(&ordering).into_iter().map(|(v, _)| v).collect();
        assert!(!topvars.is_disjoint(&HashSet::from([21, 55, 12])));
    }
}
