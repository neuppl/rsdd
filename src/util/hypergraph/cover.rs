use crate::util::hypergraph::calculate_hash;
use crate::util::hypergraph::Edge;
use core::fmt::Debug;
use itertools::{Either, Itertools, Powerset};
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
    V: Debug + Clone + Eq + Hash,
{
    pub fn show_compact(&self) -> String {
        let cover = self
            .edges
            .iter()
            .map(|e| format!("E{}", e.show_compact()))
            .join(", ");
        format!("{{ {} }}", cover)
    }
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
        Self::from_edges(HashSet::from([e.clone()]))
    }
    pub fn from_edges(es: HashSet<Edge<V>>) -> Self {
        let edges = es.clone();
        let cover = Edge::cover(&es);
        Self { cover, edges }
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
}

impl<V> IntoIterator for AllCovers<V>
where
    V: Clone + Eq + Hash,
{
    type Item = Cover<V>;
    type IntoIter = std::collections::hash_set::IntoIter<Cover<V>>;
    fn into_iter(self) -> <Self as IntoIterator>::IntoIter {
        self.covers.into_iter()
    }
}

impl<V> AllCovers<V>
where
    V: Clone + Eq + Hash + Debug,
{
    pub fn size(&self) -> usize {
        self.covers.len()
    }
    pub fn cover(&self) -> HashSet<V> {
        self.covers
            .iter()
            .map(|c| c.cover.clone())
            .flatten()
            .collect()
    }
    pub fn new(covers: HashSet<Cover<V>>) -> Self {
        AllCovers { covers }
    }
    pub fn from_edges(es: impl Iterator<Item = Edge<V>>) -> Self {
        let empty = AllCovers {
            covers: Default::default(),
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
        self.remove_edges(&HashSet::from([e.clone()]));
    }
    pub fn remove_edges(&mut self, edges: &HashSet<Edge<V>>) {
        let to_split: HashSet<_> = self
            .covers
            .iter()
            .filter(|c| !c.edges.is_disjoint(&edges))
            .cloned()
            .collect();
        for c in &to_split {
            self.covers.remove(&c);
        }
        let new_covers = to_split
            .into_iter()
            .map(|mut c| {
                for e in c.edges.clone() {
                    if edges.contains(&e) {
                        c.edges.remove(&e);
                    }
                }
                c
            })
            .map(|pruned| Self::from_edges(pruned.edges.into_iter()))
            .map(|ac| ac.covers)
            .flatten();

        self.covers.extend(new_covers);
    }
    // remove edges, but turn uncovered vertices into singleton edges
    pub fn _remove_edges_with_singletons(&mut self, edges: &HashSet<Edge<V>>) {
        let cover = self.cover(); // compare before and after
        self.remove_edges(edges);
        let next_cover = self.cover(); // compare before and after
        let remainder: HashSet<Cover<V>> = cover
            .difference(&next_cover)
            .cloned()
            .map(|v| Cover::singleton(v))
            .collect();
        self.covers.extend(remainder);
    }
}
pub struct PartitionIter<'a, V>
where
    V: Clone + Eq + Hash + Debug,
{
    over: Box<dyn Iterator<Item = (HashSet<Edge<V>>, AllCovers<V>, usize)> + 'a>,
    allcovers: AllCovers<V>,

    npartitions: usize,
    last_cutset_size: usize,
    completed: bool,
}
impl<'a, V> PartitionIter<'a, V>
where
    V: Clone + Eq + Hash + Debug + 'a,
{
    fn rage(
        covers: AllCovers<V>,
        npartitions: usize,
        with_singletons: bool,
    ) -> impl Fn(Vec<Edge<V>>) -> (HashSet<Edge<V>>, AllCovers<V>, usize) {
        move |cutset: Vec<Edge<V>>| {
            let mut sim = covers.clone();
            let cutset: HashSet<Edge<V>> = cutset.into_iter().map(|x| x.clone()).collect();
            if with_singletons {
                sim._remove_edges_with_singletons(&cutset);
            } else {
                sim.remove_edges(&cutset);
            }
            (cutset, sim, npartitions)
        }
    }

    /// brute-force partitioning
    pub fn new(covers: &AllCovers<V>, npartitions: usize, with_singletons: bool) -> Self {
        let edges: HashSet<_> = covers
            .covers
            .iter()
            .map(|c| c.edges.clone())
            .flatten()
            .collect();

        let iter: _ = edges
            .into_iter()
            .powerset()
            .skip(1) // skip the empty set
            .map(Self::rage(covers.clone(), npartitions, with_singletons))
            .collect_vec()
            .into_iter();
        Self {
            over: Box::new(iter),
            allcovers: covers.clone(),
            npartitions,
            last_cutset_size: usize::MAX,
            completed: false,
        }
    }
}
impl<'a, V> Iterator for PartitionIter<'a, V>
where
    V: Clone + Eq + Hash + Debug,
{
    type Item = (HashSet<Edge<V>>, AllCovers<V>);
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        if !self.completed {
            let (cutset, sim, _) = self
                .over
                .find(|(_, sim, npartitions)| sim.size() == *npartitions)?;
            if cutset.len() <= self.last_cutset_size {
                self.last_cutset_size = cutset.len();
                Some((cutset, sim))
            } else {
                self.completed = true;
                None
            }
        } else {
            None
        }
    }
}
pub fn partitions<V: Debug + Eq + Hash + Clone + 'static>(
    covers: &AllCovers<V>,
    npartitions: usize,
) -> impl Iterator<Item = (HashSet<Edge<V>>, AllCovers<V>)> {
    PartitionIter::new(covers, npartitions, false)
}
#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_remove_edges_2x2() {
        // add a precise graph of a 2x2 grid with clusters
        let e1 = Edge::from([1, 2, 3]);
        let e2 = Edge::from([2, 4]);
        let e3 = Edge::from([3, 4]);
        let e4 = Edge::from([4]);
        let cover = HashSet::from([1, 2, 3, 4]);
        let edges = HashSet::from([e1.clone(), e2.clone(), e3.clone(), e4.clone()]);
        let cover = Cover { cover, edges };
        let allcs = AllCovers {
            covers: HashSet::from([cover]),
        };
        let mut rm1 = allcs.clone();
        rm1.remove_edges(&HashSet::from([e1.clone()]));
        assert_eq!(rm1.size(), 1);
        let mut rm23 = allcs.clone();
        rm23.remove_edges(&HashSet::from([e2.clone(), e3.clone()]));
        assert_eq!(rm23.size(), 2); // should be cover over singleton 1 and 2 3 4
    }

    #[test]
    fn test_remove_edges_3x3() {
        // add a precise graph of a 3x3 grid with clusters
        let e1 = Edge::from([1, 2, 3]);
        let e2 = Edge::from([2, 4, 5]);
        let e3 = Edge::from([3, 5, 6]);
        let e4 = Edge::from([4, 7]);
        let e5 = Edge::from([5, 7, 8]);
        let e6 = Edge::from([6, 8]);
        let e7 = Edge::from([7, 9]);
        let e8 = Edge::from([8, 9]);
        let e9 = Edge::from([9]);
        let es = [&e1, &e2, &e3, &e4, &e5, &e6, &e7, &e8, &e9];

        let cover = HashSet::from([1, 2, 3, 4, 5, 6, 7, 8, 9]);
        let edges: HashSet<Edge<usize>> = es.iter().map(|x| x.clone()).cloned().collect();
        let cover = Cover { cover, edges };
        let allcs = AllCovers {
            covers: HashSet::from([cover]),
        };
        for cutset in es.iter().powerset() {
            let mut sim = allcs.clone();
            let cutset: HashSet<Edge<usize>> =
                cutset.into_iter().map(|x| x.clone()).cloned().collect();
            sim.remove_edges(&cutset);
            let show_cutset = cutset.into_iter().map(|e| e.show_compact()).join(", ");
            let size = sim.size();
            let partition_sizes = sim.covers.iter().map(|x| x.cover.len()).collect_vec();
            println!("{} @ {:?}: {}", size, partition_sizes, show_cutset);
        }
        println!("-------------");
        let nparts = 2;

        let mut cuts_for_2 = vec![];
        for (cutset, sim) in partitions(&allcs, nparts) {
            let partition_sizes = sim.covers.iter().map(|x| x.cover.len()).collect_vec();
            let show_cutset = cutset
                .clone()
                .into_iter()
                .map(|e| e.show_compact())
                .join(", ");
            let total = partition_sizes.iter().sum::<usize>() as f64;
            let n = nparts as f64;
            let var = partition_sizes
                .iter()
                .map(|x| (1.0 / n) * (((*x as f64) / total) - (total / n)).powf(2.0))
                .sum::<f64>();
            println!(
                "{} @ {:?} var: {:.2}: {}",
                sim.size(),
                partition_sizes,
                var,
                show_cutset
            );
            cuts_for_2.push((cutset, var));
        }
        cuts_for_2.sort_by(|(_, a), (_, b)| a.total_cmp(b));
        println!("{:?}", cuts_for_2);
        assert_eq!(cuts_for_2[0].0, HashSet::from([e2, e3]));
    }
}
