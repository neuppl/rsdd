use super::hgraph::HGraph;
use super::*;
use core::fmt::Debug;
use itertools::{max, Itertools};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(Clone, PartialEq, Eq)]
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

impl<V: Eq + Hash + fmt::Debug> Debug for Cluster<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str("C[")?;
        for v in &self.0 {
            f.write_fmt(format_args!("{:?}", v))?;
        }
        f.write_str("]")
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
    fn new(vertices: HashSet<Cluster<V>>, hyperedges: HashSet<Edge<Cluster<V>>>) -> Self {
        Self {
            graph: HGraph::new(vertices, hyperedges),
            ..Default::default()
        }
    }

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
    #[ignore]
    fn test_2x2_grid_to_dtree() {
        // add a precise graph of a 2x2 grid with clusters
        let mut g: ClusterGraph<usize> = Default::default();
        let n1 = Cluster::from([1]);
        let n2 = Cluster::from([1, 2]);
        let n3 = Cluster::from([1, 3]);
        let n4 = Cluster::from([2, 3, 4]);
        g.insert_vertices(
            [&n1, &n2, &n3, &n4]
                .iter()
                .map(|x| x.clone())
                .cloned()
                .collect(),
        );

        let e1 = Edge::from([n1.clone(), n2.clone(), n3.clone()]);
        let e2 = Edge::from([n2.clone(), n4.clone()]);
        let e3 = Edge::from([n3.clone(), n4.clone()]);
        let e4 = Edge::from([n4.clone()]);
        g.insert_edges(HashSet::from([
            e1.clone(),
            e2.clone(),
            e3.clone(),
            e4.clone(),
        ]));
        let dt = hg2dt(&g);
        println!("{:?}", dt);
        todo!()
    }

    #[test]
    fn test_2x2_grid_plus_minus_to_dtree() {
        // add a precise graph of a 2x2 grid with clusters
        let mut g: ClusterGraph<usize> = Default::default();
        let n1 = Cluster::from([1]);
        let n2 = Cluster::from([1, 2]);
        let n3 = Cluster::from([1, 3]);
        let n4 = Cluster::from([2, 3, 4]);
        let n5 = Cluster::from([4, 5]);
        g.insert_vertices(
            [&n1, &n2, &n3, &n4, &n5]
                .iter()
                .map(|x| x.clone())
                .cloned()
                .collect(),
        );
        let e1 = Edge::from([n1.clone(), n2.clone(), n3.clone()]);
        let e2 = Edge::from([n2.clone(), n4.clone()]);
        let e3 = Edge::from([n3.clone(), n4.clone()]);
        let e4 = Edge::from([n4.clone(), n5.clone()]);
        let e5 = Edge::from([n5.clone()]);
        g.insert_edges(HashSet::from([
            e1.clone(),
            e2.clone(),
            e3.clone(),
            e4.clone(),
            e5.clone(),
        ]));
        let dt = hg2dt(&g);
        println!("{:?}", dt);
        todo!()
    }

    #[test]
    #[ignore]
    fn test_3x3_grid_to_dtree() {
        // add a precise graph of a 2x2 grid with clusters
        let mut g: ClusterGraph<usize> = Default::default();
        let n1 = Cluster::from([1]);
        let n2 = Cluster::from([1, 2]);
        let n3 = Cluster::from([1, 3]);
        let n4 = Cluster::from([2, 4]);
        let n5 = Cluster::from([2, 3, 5]);
        let n6 = Cluster::from([3, 6]);
        let n7 = Cluster::from([4, 5, 7]);
        let n8 = Cluster::from([5, 6, 8]);
        let n9 = Cluster::from([7, 8, 9]);
        g.insert_vertices(
            [&n1, &n2, &n3, &n4, &n5, &n6, &n7, &n8, &n9]
                .iter()
                .map(|x| x.clone())
                .cloned()
                .collect(),
        );

        let e1 = Edge::from([n1.clone(), n2.clone(), n3.clone()]);
        let e2 = Edge::from([n2.clone(), n4.clone(), n5.clone()]);
        let e3 = Edge::from([n3.clone(), n5.clone(), n6.clone()]);
        let e4 = Edge::from([n4.clone(), n7.clone()]);
        let e5 = Edge::from([n5.clone(), n7.clone(), n8.clone()]);
        let e6 = Edge::from([n6.clone(), n8.clone()]);
        let e7 = Edge::from([n7.clone(), n9.clone()]);
        let e8 = Edge::from([n8.clone(), n9.clone()]);
        let e9 = Edge::from([n9.clone()]);
        g.insert_edges(HashSet::from([
            e1.clone(),
            e2.clone(),
            e3.clone(),
            e4.clone(),
            e5.clone(),
            e6.clone(),
            e7.clone(),
            e8.clone(),
            e9.clone(),
        ]));
        let dt = hg2dt(&g);
        println!("{:#?}", dt);
        todo!()
    }
}
