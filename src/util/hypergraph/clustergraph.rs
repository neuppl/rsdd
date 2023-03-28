use super::hgraph::HGraph;
use super::*;
use core::fmt::Debug;
use itertools::{max, Itertools};
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cluster<V>(HashSet<V>)
where
    V: Eq + Hash;
impl<V> Hash for Cluster<V>
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

impl<T, const N: usize> From<[T; N]> for Cluster<T>
where
    T: Eq + Hash,
{
    fn from(arr: [T; N]) -> Self {
        Cluster(HashSet::from(arr))
    }
}

#[derive(Clone, Debug)]
pub struct ClusterGraph<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    graph: HGraph<Cluster<V>>,
    intersections_inv: HashMap<Edge<Cluster<V>>, HashSet<V>>,
    intersections: HashMap<V, HashSet<Edge<Cluster<V>>>>,
}
impl<V> Default for ClusterGraph<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    fn default() -> Self {
        Self {
            graph: Default::default(),
            intersections: Default::default(),
            intersections_inv: Default::default(),
        }
    }
}
impl<V> Hypergraph for ClusterGraph<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    type Vertex = Cluster<V>;
    fn vertices(&self) -> &HashSet<Self::Vertex> {
        self.graph.vertices()
    }

    fn hyperedges<'a>(&self) -> std::vec::IntoIter<Edge<Self::Vertex>> {
        self.graph.hyperedges()
    }

    /// add an edge to the hypergraph. Returns false if the edge is already in the hypergraph
    fn insert_edge(&mut self, edge: Edge<Self::Vertex>) -> bool {
        self.graph.insert_edge(edge)
    }

    /// add a vertex to the hypergraph. Returns false if the vertex is already in the hypergraph
    fn insert_vertex(&mut self, v: Self::Vertex) -> bool {
        self.graph.insert_vertex(v)
    }
}

impl<V> ClusterGraph<V>
where
    V: Clone + Debug + PartialEq + Eq + Hash,
{
    pub fn variable_intersection(edge: &Edge<Cluster<V>>) -> V
    where
        V: Debug + PartialEq + Clone + Eq + Hash,
    {
        let common = Self::common_variables(&edge.0);
        // debug!("{:?} common variables: {:?}", edge, common);
        assert_eq!(common.len(), 1);
        common.iter().nth(0).unwrap().clone()
    }

    pub fn common_variables(clusters: &HashSet<Cluster<V>>) -> HashSet<V>
    where
        V: Debug + PartialEq + Clone + Eq + Hash,
    {
        let mut common = clusters
            .iter()
            .map(|x| x.0.clone())
            .nth(0)
            .expect("clusters should be non-empty");
        for c in clusters.iter() {
            common = common.intersection(&c.0).cloned().collect();
        }
        common
    }
    pub fn rebuild_intersections(&mut self) {
        let ret: HashMap<Edge<Cluster<V>>, HashSet<V>> = self
            .hyperedges()
            .map(|edge| (edge.clone(), Self::common_variables(&edge.0)))
            .collect();
        self.intersections_inv = ret.clone();
        self.intersections = ret
            .clone()
            .into_iter()
            .fold(HashMap::new(), |mut inv, (e, vs)| {
                for v in vs {
                    match inv.get_mut(&v) {
                        None => {
                            inv.insert(v.clone(), HashSet::from([e.clone()]));
                        }
                        Some(es) => {
                            es.insert(e.clone());
                        }
                    }
                }
                inv
            });
    }

    /// cut a vertex out of the hypergraph
    pub fn edgecuts_ranked(&self) -> Vec<(V, Rank)> {
        <Self as Hypergraph>::edgecuts_ranked(self).into_iter().fold(HashMap::new(),
            |mut ranks, (e, r)| {
                match self.intersections_inv.get(&e) {
                    None => panic!("expected all edges intersection map, perhaps you need to rebuild intersections"),
                    Some(vs) => {
                        for v in vs {
                            match ranks.get(&v.clone()) {
                                None => {ranks.insert(v.clone(), r.clone()); },
                                Some(rs) => {ranks.insert(v.clone(), max(vec![r, rs.clone()]).unwrap()); },
                            }
                        }
                    }
                }
                ranks
            }
        ).into_iter().collect()
    }

    pub fn edgecuts_sorted(&self) -> Vec<(V, Rank)> {
        let mut sorted_edges = self.edgecuts_ranked();
        sorted_edges.sort_by(|(_, a), (_, b)| b.cmp(&a));
        sorted_edges
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_grid2x2_cutset_construction() {
        let mut g: ClusterGraph<usize> = Default::default();
        let n1 = Cluster::from([1]);
        let n2 = Cluster::from([1, 2]);
        let n3 = Cluster::from([1, 3]);
        let n4 = Cluster::from([2, 3, 4]);
        g.insert_vertex(n1.clone());
        g.insert_vertex(n2.clone());
        g.insert_vertex(n3.clone());
        g.insert_vertex(n4.clone());
        let e1 = Edge::from([n1.clone(), n2.clone(), n3.clone()]);
        let e2 = Edge::from([n2.clone(), n4.clone()]);
        let e3 = Edge::from([n3.clone(), n4.clone()]);
        let e4 = Edge::from([n4.clone()]);
        g.insert_edge(e1.clone());
        g.insert_edge(e2.clone());
        g.insert_edge(e3.clone());
        g.insert_edge(e4.clone());
        let cs = g.covers();
        assert_eq!(cs.size(), 1);
        todo!();
    }
}
