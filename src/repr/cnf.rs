//! A representation of a conjunctive normal form (CNF)

use crate::{
    repr::{
        model::PartialModel,
        var_label::{Literal, VarLabel},
        VarOrder, WmcParams,
    },
    util::semirings::Semiring,
};
use petgraph::graph::NodeIndex;
use petgraph::prelude::UnGraph;
use quickcheck::{Arbitrary, Gen};
use rand::{self, rngs::ThreadRng, Rng};
use std::{
    cmp::{max, min},
    collections::HashSet,
    fmt,
};

// number of primes to consider during CNF hashing
const NUM_PRIMES: usize = 2;

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct HashedCNF {
    v: [u128; NUM_PRIMES],
}

/// Probabilistically hashes a partially instantiated CNF, used primarily for
/// component caching during bottom-up compilation (a component is a partially
/// instantiated CNF).
///
/// The hash function satisfies the invariant that, with (arbitrarily) high
/// probability, two components are syntactically equal if and only if they have
/// the same hash.
///
/// It works by associating each literal in the CNF with a distinct prime
/// number. To compute the hash function, one takes the product (modulo a prime
/// field, the base of which is specified in PRIMES) of all unset literals in
/// clauses that are not satisfied.
///
/// Example: Assume we have the following CNF, with its literals annotated with
/// distinct primes:
///
/// ```text
/// // `(a \/ b) /\ (!a \/ c)`
/// //   ^    ^      ^     ^
/// //   2    3      5     7
/// ```
/// Then, if we were to hash this CNF with the partial model (a = T), would
/// get the value 7
#[derive(Eq, PartialEq, Debug, Clone)]
pub struct CnfHasher {
    weighted_cnf: Vec<Vec<(usize, Literal)>>,
    /// state\[x\] contains a list of unsatisfied clauses in the CNF (indexed into weighted_cnf)
    state: Vec<HashSet<usize>>,
    /// pos_lits\[l\] contains the list of indexes of clauses that contain the positive literal l
    pos_lits: Vec<Vec<usize>>,
    /// neg_lits\[l\] contains the list of indexes of clauses that contain the negative literal l
    neg_lits: Vec<Vec<usize>>,
}

impl CnfHasher {
    pub fn new(clauses: &[Vec<Literal>], num_vars: usize) -> CnfHasher {
        let mut primes = primal::Primes::all();
        let weighted_cnf = clauses
            .iter()
            .map({
                |clause| {
                    clause
                        .iter()
                        .map(|lit| (primes.next().unwrap(), *lit))
                        .collect()
                }
            })
            .collect();

        let sat_clauses: HashSet<usize> = clauses
            .iter()
            .enumerate()
            .filter_map({
                |(clause_idx, clause)| {
                    if clause.len() > 1 {
                        Some(clause_idx)
                    } else {
                        None
                    }
                } // ignore units
            })
            .collect();

        let pos_lits: Vec<Vec<usize>> = (0..num_vars)
            .map(|lit_idx| {
                clauses
                    .iter()
                    .enumerate()
                    .filter_map(|(clause_idx, clause)| {
                        if clause.contains(&Literal::new(VarLabel::new_usize(lit_idx), true)) {
                            Some(clause_idx)
                        } else {
                            None
                        }
                    })
                    .collect()
            })
            .collect();

        let neg_lits: Vec<Vec<usize>> = (0..num_vars)
            .map(|lit_idx| {
                clauses
                    .iter()
                    .enumerate()
                    .filter_map(|(clause_idx, clause)| {
                        if clause.contains(&Literal::new(VarLabel::new_usize(lit_idx), false)) {
                            Some(clause_idx)
                        } else {
                            None
                        }
                    })
                    .collect()
            })
            .collect();

        CnfHasher {
            weighted_cnf,
            state: vec![sat_clauses],
            pos_lits,
            neg_lits,
        }
    }

    pub fn decide(&mut self, lit: Literal) {
        if lit.polarity() {
            for clause_idx in self.pos_lits[lit.label().value_usize()].iter() {
                self.state.last_mut().unwrap().remove(clause_idx);
            }
        } else {
            for clause_idx in self.neg_lits[lit.label().value_usize()].iter() {
                self.state.last_mut().unwrap().remove(clause_idx);
            }
        }
    }

    pub fn push(&mut self) {
        let n = self.state.last().unwrap().clone();
        self.state.push(n);
    }

    pub fn pop(&mut self) {
        self.state.pop();
    }

    pub fn hash(&self, m: &PartialModel) -> HashedCNF {
        let mut v: [u128; NUM_PRIMES] = [1; NUM_PRIMES];
        'outer: for clause_idx in self.state.last().unwrap().iter() {
            let mut cur_clause_v: u128 = 1;
            let cur_clause = &self.weighted_cnf[*clause_idx];
            for (ref weight, ref lit) in cur_clause.iter() {
                if m.lit_implied(*lit) {
                    // move onto the next clause without updating the
                    // accumulator
                    continue 'outer;
                } else if m.lit_neg_implied(*lit) {
                    // skip this literal and move onto the next one
                    continue;
                } else {
                    // cur_clause_v = cur_clause_v * (*weight as u128);
                    cur_clause_v = cur_clause_v.wrapping_mul(*weight as u128);
                }
            }
            for i in v.iter_mut().take(NUM_PRIMES) {
                // TODO at the moment this modular multiplication is very slow; we should use a
                // crate that supports fast modular multiplication for fixed prime fields
                // using just 128-bit for now, but this is a hack
                // v[i] = (v[i]* (cur_clause_v)) % PRIMES[i];
                *i = i.wrapping_mul(cur_clause_v);
            }
        }
        HashedCNF { v }
    }
}

/// Let G be a graph and X be a node in G. The result of eliminating node X from
/// graph G is another graph obtained from G by first adding an edge between
/// every pair of nonadjacent neighbors of X and then deleting node X from G.
/// The edges that are added during the elimination process are called fill-in
/// edges.
fn eliminate_node(g: &mut UnGraph<VarLabel, ()>, v: NodeIndex) {
    let neighbor_vec: Vec<NodeIndex> = g.neighbors_undirected(v).collect();
    for n1 in 0..neighbor_vec.len() {
        for n2 in (n1 + 1)..neighbor_vec.len() {
            if g.find_edge(neighbor_vec[n1], neighbor_vec[n2]).is_none() {
                g.add_edge(neighbor_vec[n1], neighbor_vec[n2], ());
            }
        }
    }
    g.remove_node(v);
}

/// gets the number of fill edges that are added on eliminating v
fn num_fill(g: &UnGraph<VarLabel, ()>, v: NodeIndex) -> usize {
    let mut count = 0;
    let neighbor_vec: Vec<NodeIndex> = g.neighbors_undirected(v).collect();
    for n1 in 0..neighbor_vec.len() {
        for n2 in (n1 + 1)..neighbor_vec.len() {
            if g.find_edge(neighbor_vec[n1], neighbor_vec[n2]).is_none() {
                count += 1;
            }
        }
    }
    count
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Cnf {
    clauses: Vec<Vec<Literal>>,
    num_vars: usize,
    hasher: CnfHasher,
}

pub struct AssignmentIter {
    cur: Option<Vec<bool>>,
    num_vars: usize,
}

impl AssignmentIter {
    pub fn new(num_vars: usize) -> AssignmentIter {
        AssignmentIter {
            cur: None,
            num_vars,
        }
    }
}

impl Iterator for AssignmentIter {
    type Item = Vec<bool>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.cur.is_none() {
            self.cur = Some((0..self.num_vars).map(|_| false).collect());
            self.cur.clone()
        } else {
            // attempt to do a binary increment of the current state
            let (new_c, carry) = self.cur.as_ref().unwrap().iter().fold(
                (Vec::new(), true),
                |(mut cur_l, carry), cur_assgn| {
                    // half-adder
                    let new_itm = cur_assgn ^ carry;
                    let new_carry = *cur_assgn && carry;
                    cur_l.push(new_itm);
                    (cur_l, new_carry)
                },
            );

            self.cur = Some(new_c);
            if carry {
                None
            } else {
                self.cur.clone()
            }
        }
    }
}

impl Cnf {
    pub fn new(clauses: &[Vec<Literal>]) -> Cnf {
        let clauses: Vec<Vec<Literal>> = clauses
            .iter()
            .filter(|clause| !clause.is_empty())
            .map(|clause| {
                let mut clause = clause.clone();
                clause.sort_by_key(|a| a.label().value());
                clause.dedup();
                clause
            })
            .collect();

        let num_vars = clauses
            .iter()
            .map(|clause| {
                clause
                    .iter()
                    .map(|lit| lit.label().value() + 1)
                    .max()
                    .unwrap_or(0)
            })
            .max()
            .unwrap_or(0) as usize;

        Cnf {
            hasher: CnfHasher::new(&clauses, num_vars),
            clauses,
            num_vars,
        }
    }

    pub fn from_dimacs(input: &str) -> Cnf {
        use dimacs::*;
        let (_, cvec) = match parse_dimacs(input).unwrap() {
            Instance::Cnf { num_vars, clauses } => (num_vars, clauses),
            Instance::Sat {
                num_vars: _,
                extensions: _,
                formula: _,
            } => panic!("Received (valid) SAT input, not CNF"),
        };
        let mut clause_vec: Vec<Vec<Literal>> = Vec::new();
        let mut m = 0;
        for itm in cvec.iter() {
            let mut lit_vec: Vec<Literal> = Vec::new();
            for l in itm.lits().iter() {
                let b = match l.sign() {
                    Sign::Neg => false,
                    Sign::Pos => true,
                };
                // subtract 1, we are 0-indexed
                let lbl = VarLabel::new(l.var().to_u64() - 1);
                m = max(l.var().to_u64() as usize, m);
                lit_vec.push(Literal::new(lbl, b));
            }
            clause_vec.push(lit_vec);
        }
        Cnf::new(&clause_vec)
    }

    /// Parses a CNF string into a CNF
    ///
    /// Format: (-1 || 0 || 2) && (1)
    /// - Each clause in its own parenthesis and separated by &&
    /// - Each literal separated by ||
    /// - Literal negation implied by "-"
    pub fn from_string(s: &str) -> Cnf {
        let clauses = s.split("&&");
        let mut clause_vec: Vec<Vec<Literal>> = Vec::new();
        for clause in clauses {
            let mut c: Vec<Literal> = Vec::new();
            // filter the parens off
            let mut chars = clause.trim().chars();
            chars.next();
            chars.next_back();
            for lit in chars.as_str().split("||") {
                if lit.trim().is_empty() {
                    continue;
                }
                let parsed: i64 = lit
                    .trim()
                    .parse()
                    .unwrap_or_else(|_| panic!("failed to parse literal {}", lit));
                let neg = parsed <= 0;
                c.push(Literal::new(
                    VarLabel::new_usize(i64::abs(parsed) as usize),
                    !neg,
                ));
            }
            clause_vec.push(c);
        }
        Cnf::new(&clause_vec)
    }

    pub fn rand_cnf(rng: &mut ThreadRng, num_vars: usize, num_clauses: usize) -> Cnf {
        assert!(num_clauses > 2, "requires at least 2 clauses in CNF");
        let vars: Vec<Literal> = (1..num_vars)
            .map(|x| Literal::new(VarLabel::new(x as u64), rand::random()))
            .collect();
        // let range = rand::distributions iRange::new(0, vars.len());
        let clause_size = 3;
        // we generate a random cnf
        let mut clause_vec: Vec<Vec<Literal>> = Vec::new();
        for _ in 0..num_clauses {
            let num_vars = clause_size;
            if num_vars > 1 {
                let mut var_vec: Vec<Literal> = Vec::new();
                for _ in 0..clause_size {
                    let var = *vars.get(rng.gen_range(0..vars.len())).unwrap();
                    var_vec.push(var);
                }
                clause_vec.push(var_vec);
            } else {
                let var = *vars.get(rng.gen_range(0..vars.len())).unwrap();
                clause_vec.push(vec![var]);
            }
        }
        Cnf::new(&clause_vec)
    }

    pub fn num_vars(&self) -> usize {
        self.num_vars
    }

    pub fn clauses(&self) -> &[Vec<Literal>] {
        self.clauses.as_slice()
    }

    /// evaluate this CNF on an assignment
    /// assignment\[x\] is the assignment to VarLabel(x)
    pub fn eval(&self, assignment: &Vec<bool>) -> bool {
        assert!(assignment.len() >= self.num_vars());
        for clause in self.clauses.iter() {
            let mut clause_sat = false;
            for lit in clause.iter() {
                let assgn = assignment[lit.label().value() as usize];
                if lit.polarity() == assgn {
                    clause_sat = true;
                }
            }
            if !clause_sat {
                return false;
            }
        }
        // no unsat clauses
        true
    }

    /// true if the partial model implies the CNF
    pub fn is_sat_partial(&self, partial_assignment: &PartialModel) -> bool {
        for clause in self.clauses.iter() {
            let mut clause_sat = false;
            for lit in clause.iter() {
                if let Some(assgn) = partial_assignment.get(lit.label()) {
                    if lit.polarity() == assgn {
                        clause_sat = true;
                    }
                }
            }
            if !clause_sat {
                return false;
            }
        }
        true
    }

    /// compute the average span of the clauses with the ordering given by
    /// `lbl_to_pos`, which is a mapping from variable labels to their position
    /// in the ordering
    fn average_span(&self, lbl_to_pos: &[usize]) -> f64 {
        let mut total = 0;
        for clause in self.clauses.iter() {
            let mut min_pos = lbl_to_pos.len();
            let mut max_pos = 0;
            // find the two variables in the clause which are farthest
            // from each other in the order
            for lit in clause.iter() {
                let this_pos = lbl_to_pos[lit.label().value() as usize];
                min_pos = min(this_pos, min_pos);
                max_pos = max(this_pos, max_pos);
            }
            total += max_pos - min_pos;
        }
        (total as f64) / (self.clauses.len() as f64)
    }

    /// computes the center of gravity of a particular clause for a given order
    /// Aloul, Fadi A., Igor L. Markov, and Karem A. Sakallah. "FORCE: a fast
    /// and easy-to-implement variable-ordering heuristic." Proceedings of the
    /// 13th ACM Great Lakes symposium on VLSI. 2003.
    fn center_of_gravity(&self, clause: &[Literal], lbl_to_pos: &[usize]) -> f64 {
        let sum = clause.iter().fold(0, |acc, &lbl| {
            lbl_to_pos[lbl.label().value() as usize] + acc
        });

        (sum as f64) / (clause.len() as f64)
    }

    /// compute a weighted model count of a CNF
    /// Note: not efficient! this is exponential in #variables
    /// mostly for internal testing purposes
    pub fn wmc<T: Semiring + std::ops::Mul<Output = T> + std::ops::Add<Output = T>>(
        &self,
        weights: &WmcParams<T>,
    ) -> T {
        let mut total: T = T::zero();
        let mut weight_vec = Vec::new();
        for i in 0..self.num_vars() {
            weight_vec.push(weights.var_weight(VarLabel::new(i as u64)));
        }
        for assgn in AssignmentIter::new(self.num_vars()) {
            if assgn.is_empty() {
                break;
            };
            if self.eval(&assgn) {
                let assgn_w = assgn
                    .iter()
                    .enumerate()
                    .fold(T::one(), |v, (idx, &polarity)| {
                        let (loww, highw) = weight_vec[idx];
                        v.mul(if polarity { *highw } else { *loww })
                    });
                total = total + assgn_w;
            }
        }
        total
    }

    pub fn linear_order(&self) -> VarOrder {
        let v = (0..(self.num_vars))
            .map(|x| VarLabel::new(x as u64))
            .collect();
        VarOrder::new(v)
    }

    /// heuristically generate a variable ordering which minimizes the average
    /// clause span
    pub fn force_order(&self) -> VarOrder {
        // map from position -> label (i.e., first element is the first in the
        // order)
        let mut lbl_to_pos: Vec<usize> = (0..(self.num_vars)).collect();
        // let mut rng = rand::thread_rng();
        // lbl_to_pos.shuffle(&mut rng);
        // perform 100 iterations of force-update
        let mut cur_span: f64 = self.average_span(&lbl_to_pos);
        let mut prev_span;
        loop {
            prev_span = cur_span;
            let mut cog: Vec<f64> = Vec::with_capacity(self.clauses.len());
            for clause in self.clauses.iter() {
                cog.push(self.center_of_gravity(clause, &lbl_to_pos));
            }
            // compute average centers of gravity for each variable
            // a vector which holds (1) the running CoG, (2) the number of clauses
            // which contain a given variable
            let mut update: Vec<(f64, usize)> = Vec::with_capacity(self.num_vars);
            for _ in 0..self.num_vars {
                update.push((0.0, 0));
            }
            for (idx, clause) in self.clauses.iter().enumerate() {
                for &lit in clause.iter() {
                    let (cur_total, num_edges) = update[lit.label().value() as usize];
                    update[lit.label().value() as usize] = (cur_total + cog[idx], num_edges + 1);
                }
            }
            let avg_cog: Vec<f64> = update
                .into_iter()
                .map(|(total, cnt)| if cnt == 0 { 0.0 } else { total / (cnt as f64) })
                .collect();

            let l = avg_cog.len();
            let mut avg_cog: Vec<(f64, usize)> = avg_cog.into_iter().zip(0..l).collect();
            // now sort avg_cog on the centers of gravity
            avg_cog.sort_by(|(c1, _), (c2, _)| c1.partial_cmp(c2).unwrap());
            // update positions
            let pos_to_lbl: Vec<usize> = avg_cog.into_iter().map(|(_, p)| p).collect();
            // now convert to lbl_to_pos
            for (idx, lbl) in pos_to_lbl.into_iter().enumerate() {
                lbl_to_pos[lbl] = idx;
            }
            cur_span = self.average_span(&lbl_to_pos);
            if prev_span - cur_span < 1.0 {
                break;
            }
        }
        let final_order: Vec<VarLabel> = lbl_to_pos
            .into_iter()
            .map(|v| VarLabel::new(v as u64))
            .collect();
        VarOrder::new(final_order)
    }

    /// Updates the CNF to a new CNF that results from conditioning on the supplied literal
    pub fn condition(&mut self, lit: Literal) -> Cnf {
        let new_cnf: Vec<Vec<Literal>> = self
            .clauses()
            .iter()
            .filter_map(|clause| {
                // first, check if there is a true literal -- if there is, filter out this clause
                if clause
                    .iter()
                    .any(|outer| outer.label() == lit.label() && outer.polarity() == lit.polarity())
                {
                    None
                } else {
                    // next, filter out clauses with false literals
                    let filtered: Vec<Literal> = clause
                        .iter()
                        .filter(|outer| {
                            !(lit.label() == outer.label() && lit.polarity() != outer.polarity())
                        })
                        .copied()
                        .collect();
                    Some(filtered)
                }
            })
            .collect();
        Cnf::new(&new_cnf)
    }

    pub fn interaction_graph(&self) -> UnGraph<VarLabel, ()> {
        let mut g: UnGraph<VarLabel, ()> = UnGraph::new_undirected();
        for v in 0..self.num_vars {
            g.add_node(VarLabel::new(v as u64));
        }
        // add a clique for each clause
        for c in self.clauses.iter() {
            // for every pair of literals in the clause, add an edge
            for i in 0..c.len() {
                for j in i..c.len() {
                    let a = NodeIndex::new(c[i].label().value_usize());
                    let b = NodeIndex::new(c[j].label().value_usize());
                    if !g.contains_edge(a, b) {
                        g.add_edge(a, b, ());
                    }
                }
            }
        }
        g
    }

    /// get a min-fill elimination order for this CNF
    pub fn min_fill_order(&self) -> VarOrder {
        let mut ord: Vec<VarLabel> = Vec::new();
        let mut ig = self.interaction_graph();
        while ig.node_count() > 0 {
            // find the min-fill node, eliminate it, and add it to v
            let (idx, _) = ig
                .node_indices()
                .map(|x| (x, num_fill(&ig, x)))
                .min_by(|(_, a), (_, b)| a.cmp(b))
                .unwrap();
            ord.push(ig[idx]);
            eliminate_node(&mut ig, idx);
        }
        // assert that ord contains each variable exactly once.
        debug_assert!((0..(self.num_vars())).all(|v| ord.contains(&VarLabel::new_usize(v))));
        debug_assert!(ord.len() == self.num_vars);
        VarOrder::new(ord)
    }

    pub fn to_dimacs(&self) -> String {
        let mut r = String::new();
        for clause in self.clauses.iter() {
            let mut clause_str = String::new();
            for lit in clause.iter() {
                let lit_str = format!(
                    "{}{}",
                    if lit.polarity() { "" } else { "-" },
                    lit.label().value_usize() + 1
                );
                if clause_str.is_empty() {
                    clause_str = lit_str;
                } else {
                    clause_str = format!("{} {}", clause_str, lit_str);
                }
            }
            r = format!("{}\n{} 0", r, clause_str);
        }
        r
    }

    /// get a hasher for this CNF
    /// may be expensive on first call; future calls are amortized
    pub fn hasher(&self) -> &CnfHasher {
        &self.hasher
    }

    // Checks if v a variable is in the CNF
    pub fn var_in_cnf(&self, v: VarLabel) -> bool {
        let list_of_lits: Vec<Literal> = self.clauses.clone().into_iter().flatten().collect();
        for l in &list_of_lits {
            if l.label() == v {
                return true;
            }
        }
        false
    }
}

impl Arbitrary for Cnf {
    /// generate an arbitrary CNF with at most 9 variables and at most 16 clauses
    fn arbitrary(g: &mut Gen) -> Cnf {
        let num_vars = (u64::arbitrary(g) % 8) + 1;
        let num_clauses = (usize::arbitrary(g) % 16) + 1;
        let mut clauses = Vec::new();
        for _ in 0..num_clauses {
            let clause_size = (usize::arbitrary(g) % 3) + 1;
            let mut clause: Vec<Literal> = Vec::new();
            for _ in 0..clause_size {
                clause.push(Literal::new(
                    VarLabel::new(u64::arbitrary(g) % num_vars),
                    bool::arbitrary(g),
                ));
            }
            clauses.push(clause);
        }
        Cnf::new(&clauses)
    }
}

impl fmt::Display for Cnf {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut r = String::new();
        for clause in self.clauses.iter() {
            let mut clause_str = String::new();
            for lit in clause.iter() {
                let lit_str = format!(
                    "{}{}",
                    if lit.polarity() { "" } else { "!" },
                    lit.label().value()
                );
                if clause_str.is_empty() {
                    clause_str = lit_str;
                } else {
                    clause_str = format!("{} || {}", clause_str, lit_str);
                }
            }
            if r.is_empty() {
                r = format!("({})", clause_str);
            } else {
                r = format!(" {} && ({})", r, clause_str);
            }
        }
        write!(f, "{}", r)
    }
}

#[test]
fn test_cnf_wmc() {
    use crate::constants::primes;
    use crate::util::semirings::FiniteField;
    use std::collections::HashMap;

    let v = vec![vec![
        Literal::new(VarLabel::new(0), true),
        Literal::new(VarLabel::new(1), false),
    ]];
    let cnf = Cnf::new(&v);
    let weights: std::collections::HashMap<
        VarLabel,
        (
            FiniteField<{ primes::U32_TINY }>,
            FiniteField<{ primes::U32_TINY }>,
        ),
    > = HashMap::from_iter([
        (VarLabel::new(0), (FiniteField::new(1), FiniteField::new(1))),
        (VarLabel::new(1), (FiniteField::new(1), FiniteField::new(1))),
    ]);
    assert_eq!(cnf.wmc(&WmcParams::new(weights)), FiniteField::new(3));
}
