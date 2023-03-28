use crate::util::hypergraph::calculate_hash;
use crate::util::hypergraph::Edge;
use core::fmt::Debug;
use itertools::Either;
use itertools::Itertools;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cover<V>
where
    V: Clone + Eq + Hash,
{
    pub(in crate::util::hypergraph) cover: HashSet<V>,
    pub(in crate::util::hypergraph) edges: HashSet<Edge<V>>,
}
impl<V> Hash for Cover<V>
where
    V: Clone + Eq + Hash,
{
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        let mut hashes = vec![];
        let cover_hashes = self.cover.iter().map(calculate_hash).collect_vec();
        hashes.extend(cover_hashes);
        let edge_hashes = self.edges.iter().map(calculate_hash).collect_vec();
        hashes.extend(edge_hashes);
        hashes.sort();
        let hashstr = hashes.into_iter().map(|x| x.to_string()).join("");
        hashstr.hash(state);
    }
}

impl<V> Cover<V>
where
    V: Clone + Eq + Hash,
{
    pub fn empty() -> Self {
        Self {
            cover: Default::default(),
            edges: Default::default(),
        }
    }
    pub fn singleton(v: V) -> Self {
        Self {
            cover: HashSet::from([v.clone()]),
            edges: HashSet::from([Edge::from([v.clone()])]),
        }
    }
    pub fn from_edge(e: &Edge<V>) -> Self {
        Self {
            cover: e.0.clone(),
            edges: HashSet::from([e.clone()]),
        }
    }
    pub fn merge(cs: HashSet<Cover<V>>) -> Self {
        let ret = cs.into_iter().fold(Self::empty(), |mut ret, cover| {
            ret.cover.extend(cover.cover);
            ret.edges.extend(cover.edges);
            ret
        });
        ret
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AllCovers<V>
where
    V: Clone + Eq + Hash,
{
    pub(in crate::util::hypergraph) covers: HashSet<Cover<V>>,
    pub(in crate::util::hypergraph) vertices: HashSet<V>,
}
impl<V> AllCovers<V>
where
    V: Clone + Eq + Hash,
{
    pub fn size(&self) -> usize {
        self.covers.len()
    }

    pub fn new(covers: HashSet<Cover<V>>) -> Self {
        let vertices: HashSet<_> = covers.iter().map(|c| c.cover.clone()).flatten().collect();
        AllCovers { covers, vertices }
    }
    pub fn from_edges(es: impl Iterator<Item = Edge<V>>) -> Self {
        let empty = AllCovers {
            covers: Default::default(),
            vertices: Default::default(),
        };
        let seen: HashSet<V> = HashSet::new();
        es.fold((empty, seen), |(mut all, mut seen), edge| {
            let e = Cover::from_edge(&edge);
            if seen.is_disjoint(&e.cover) {
                seen.extend(e.cover.clone());
                all.covers.insert(e);
                (all, seen)
            } else {
                let (mut to_merge, mut rest): (HashSet<Cover<V>>, HashSet<Cover<V>>) =
                    all.covers.into_iter().partition_map(|cover| {
                        if cover.cover.is_disjoint(&e.cover) {
                            Either::Right(cover)
                        } else {
                            Either::Left(cover)
                        }
                    });
                seen.extend(e.cover.clone());
                to_merge.insert(e);
                rest.insert(Cover::merge(to_merge));
                (AllCovers::new(rest), seen)
            }
        })
        .0
    }
    pub fn remove_edge(&mut self, e: &Edge<V>) {
        let mut to_split = self
            .covers
            .iter()
            .find(|c| c.edges.contains(e))
            .expect("edge not found in covers")
            .clone();
        self.covers.remove(&to_split);
        to_split.edges.remove(e);
        let new_covers = Self::from_edges(to_split.edges.into_iter());
        self.covers.extend(new_covers.covers);
    }
    pub fn remove_edges(&mut self, edges: &HashSet<Edge<V>>) {
        for e in edges {
            self.remove_edge(e);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_remove_edges_2x2() {
        let e1 = Edge::from([1, 2, 3]);
        let e2 = Edge::from([2, 4]);
        let e3 = Edge::from([3, 4]);
        let e4 = Edge::from([4]);
        let cover = HashSet::from([1, 2, 3, 4]);
        let edges = HashSet::from([e1.clone(), e2.clone(), e3.clone(), e4.clone()]);
        let cover = Cover { cover, edges };
        let mut allcs = AllCovers {
            covers: HashSet::from([cover]),
            vertices: HashSet::from([1, 2, 3, 4]),
        };
        allcs.remove_edges(&HashSet::from([e1.clone()]));
        assert_eq!(allcs.size(), 2); // should be cover over singleton 1 and 2 3 4
    }
}
