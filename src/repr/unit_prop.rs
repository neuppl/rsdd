//! A generic unit propagator for CNFs
//! Probabilistically hashes a partially instantiated CNF, used primarily for
//! component caching during bottom-up compilation (a component is a partially
//! instantiated CNF).
//
//! The hash function satisfies the invariant that, with (arbitrarily) high
//! probability, two components are syntactically equal if and only if they have
//! the same hash.
//
//! It works by associating each literal in the CNF with a distinct prime
//! number. To compute the hash function, one takes the product (modulo a prime
//! field, the base of which is specified in PRIMES) of all unset literals in
//! clauses that are not satisfied.
//
//! Example: Assume we have the following CNF, with its literals annotated with
//! distinct primes:
//! ```text
//! // `(a \/ b) /\ (!a \/ c)`
//! //   ^    ^      ^     ^
//! //   2    3      5     7
//! ```
//! Then, if we were to hash this CNF with the partial model (a = T), would
//! get the value 7

use crate::repr::{
    cnf::Cnf,
    model::PartialModel,
    var_label::{Literal, VarLabel},
};
use bit_set::BitSet;

type ClauseIdx = usize;
type LitIdx = usize;

/// A data-structure for efficient implementation of unit propagation with CNFs.
/// It implements a two-literal watching scheme.
/// For instance, for the CNF:
///     c0             c1
/// (A \/ !B) /\ (C \/ !A \/ B)
///  ^     ^      ^    ^
/// The literals A and !B are watched in c0, and C and !A  are watched in c1
/// This is stored in the watch lists as:
/// watch_list_pos:
///    A: \[ c0 \]
///    B: \[ \]
///    C: \[ c1 \]
/// watch_list_neg:
///    A: \[ c1 \]
///    B: \[ c0 \]
///    C: \[ \]
///
/// The invariant maintained by the watched literals is that, for a given
/// partial model state, the watched literal is either (1) satisfied by the
/// model; or (2) unset in the model.
///
/// Every clause (except for unit clauses) watch exactly 2 unique literals at all times.
/// When a variable is decided, we decide how to update the watched literals according
/// to the following cases
///
/// Case 1. We decide C=False. In this case, we deduce no units, and update the literal watches
/// as:
///     c0             c1
/// (A \/ !B) /\ (C \/ !A \/ B)
///  ^     ^            ^    ^
/// I.e., we pick a new literal to watch in c1
/// Case 2. Decide C=True, In this case, the clause c1 is satisfied, so  we do not make
/// any changes to its watched literals.
/// Case 3. Decide A=False. In this case there are no remaining literals to
/// watch in c0, so we deduce a unit !B and propagate. The resulting watcher
/// state unchanged in this case.
#[derive(Debug, Clone)]
pub struct UnitPropagate {
    // watch_list_pos[i] is a list of the clauses that are watching the positive
    // literal of varlabel i
    watch_list_pos: Vec<Vec<ClauseIdx>>,
    // similar to the above, but for negative label
    watch_list_neg: Vec<Vec<ClauseIdx>>,
    cnf: Cnf,
}

#[allow(clippy::upper_case_acronyms)]
enum UnitPropResult {
    UNSAT,
    PartialSAT(PartialModel),
}

impl UnitPropagate {
    /// Returns None if UNSAT discovered during initial unit propagation
    pub fn new(cnf: Cnf) -> Option<(UnitPropagate, PartialModel)> {
        let mut watch_list_pos: Vec<Vec<usize>> = Vec::new();
        let mut watch_list_neg: Vec<Vec<usize>> = Vec::new();
        for _ in 0..cnf.num_vars() {
            watch_list_pos.push(Vec::new());
            watch_list_neg.push(Vec::new());
        }

        // do initial unit propagation
        let mut implied: Vec<Literal> = Vec::new();
        for (idx, c) in cnf.clauses().iter().enumerate() {
            if c.is_empty() {
                return None;
            }
            if c.len() == 1 {
                implied.push(c[0]);
                continue;
            }
            if c[1].polarity() {
                watch_list_pos[c[1].label().value() as usize].push(idx)
            } else {
                watch_list_neg[c[1].label().value() as usize].push(idx)
            }
            if c[0].polarity() {
                watch_list_pos[c[0].label().value() as usize].push(idx)
            } else {
                watch_list_neg[c[0].label().value() as usize].push(idx)
            }
        }

        let mut cur = UnitPropagate {
            watch_list_pos,
            watch_list_neg,
            cnf,
        };

        let mut cur_state = PartialModel::new(cur.cnf.num_vars());

        for i in implied {
            match cur.decide(cur_state, i) {
                UnitPropResult::UNSAT => return None,
                UnitPropResult::PartialSAT(r) => {
                    cur_state = r;
                }
            }
        }

        Some((cur, cur_state))
    }

    /// Set a variable to a particular value and propagates
    /// returns true if success, false if UNSAT
    fn decide(&mut self, mut cur_state: PartialModel, new_assignment: Literal) -> UnitPropResult {
        // if already assigned, check if consistent -- if not, return unsat
        match cur_state.get(new_assignment.label()) {
            None => (),
            Some(v) => {
                if v == new_assignment.polarity() {
                    return UnitPropResult::PartialSAT(cur_state);
                } else {
                    return UnitPropResult::UNSAT;
                }
            }
        };

        // update the value of the decided variable in the partial model
        cur_state.set(new_assignment.label(), new_assignment.polarity());

        let var_idx = new_assignment.label().value() as usize;

        // track a list of all discovered implications
        // indexes over the watchers for the current literal
        let mut watcher_idx = 0;
        // some of the internal structure here is weird to accommodate the
        // borrow checker.
        loop {
            // first check if there are any watchers; if no watchers left, break

            if new_assignment.polarity() {
                if watcher_idx >= self.watch_list_neg[var_idx].len() {
                    break;
                }
            } else if watcher_idx >= self.watch_list_pos[var_idx].len() {
                break;
            }
            let clause = if new_assignment.polarity() {
                &self.cnf.clauses()[self.watch_list_neg[var_idx][watcher_idx]]
            } else {
                &self.cnf.clauses()[self.watch_list_pos[var_idx][watcher_idx]]
            };

            // if clause is satisfied, do not change its watched literals and
            // move onto the next watcher
            let mut is_sat = false;
            for lit in clause.iter() {
                match cur_state.get(lit.label()) {
                    Some(v) if lit.polarity() == v => {
                        watcher_idx += 1;
                        is_sat = true;
                        break;
                    }
                    _ => (),
                }
            }
            if is_sat {
                continue;
            }

            // gather a list of all remaining unassigned literals in the current clause
            let mut remaining_lits = clause.iter().filter(|x| cur_state.get(x.label()).is_none());

            let num_remaining = remaining_lits.clone().count();
            if num_remaining == 0 {
                // UNSAT -- need to move a watcher and there are no remaining
                // unassigned literals to watch
                return UnitPropResult::UNSAT;
            } else if num_remaining == 1 {
                // just found a unit. propagate it and move onto the next watcher
                let new_unit = remaining_lits.next().unwrap();
                match self.decide(cur_state, *new_unit) {
                    UnitPropResult::UNSAT => return UnitPropResult::UNSAT,
                    UnitPropResult::PartialSAT(new_state) => {
                        cur_state = new_state;
                        watcher_idx += 1;
                    }
                }
            } else {
                // num_remaining > 1, find a new literal to watch
                // first, find a new literal to watch
                let candidate_unwatched: LitIdx =
                    remaining_lits.clone().next().unwrap().label().value_usize();
                // check if candidate_unwatched is already being watched; if it
                // is, pick another literal to watch
                let prev_watcher: ClauseIdx = if new_assignment.polarity() {
                    self.watch_list_neg[var_idx][watcher_idx]
                } else {
                    self.watch_list_pos[var_idx][watcher_idx]
                };

                let new_lit: &Literal = if new_assignment.polarity() {
                    if self.watch_list_pos[candidate_unwatched].contains(&prev_watcher) {
                        remaining_lits.nth(1).unwrap()
                    } else {
                        remaining_lits.next().unwrap()
                    }
                } else if self.watch_list_neg[candidate_unwatched].contains(&prev_watcher) {
                    remaining_lits.nth(1).unwrap()
                } else {
                    remaining_lits.next().unwrap()
                };

                let new_loc = new_lit.label().value_usize();

                if new_assignment.polarity() {
                    self.watch_list_neg[var_idx].swap_remove(watcher_idx);
                } else {
                    self.watch_list_pos[var_idx].swap_remove(watcher_idx);
                };

                // now add the new one
                if new_lit.polarity() {
                    self.watch_list_pos[new_loc].push(prev_watcher);
                } else {
                    self.watch_list_neg[new_loc].push(prev_watcher);
                }
                // do not increment watcher_idx (since we decreased the total number of watchers, we have made progress)
            }
        }
        UnitPropResult::PartialSAT(cur_state)
    }
}

pub enum DecisionResult {
    SAT,
    UNSAT,
    Unknown,
}

#[derive(Clone, Debug)]
struct SatState {
    model: PartialModel,
    hash: u128,
    sat_clauses: BitSet,
}

pub struct SATSolver {
    up: UnitPropagate,
    clauses: Vec<Vec<(Literal, u128)>>,
    // contains_pos_lit[i] is the set of clauses that contains positive
    // varlabel i
    contains_pos_lit: Vec<BitSet>,
    contains_neg_lit: Vec<BitSet>,
    state_stack: Vec<SatState>,
}

impl SATSolver {
    fn top_state(&self) -> &SatState {
        self.state_stack.last().unwrap()
    }

    /// given a new_model, computes the resulting updated hash state and set of
    /// satisfied clauses by multiplying in the contribution of all subsumed
    /// literals between the top state of the sat state and the new model
    fn update_hash_and_sat_set(&self, new_model: &PartialModel) -> (u128, BitSet) {
        let mut hash = self.top_state().hash;
        let mut new_set = self.top_state().sat_clauses.clone();
        // multiply in the hash of all subsumed literals
        // step through every clause that contains this literal. three cases:
        //  1. clause is already satisfied. skip its contribution.
        //  2. lit subsumes the clause. multiply in contribution of all variables in that clause
        //     that are not already assigned.
        //  3. lit does not subsume clause. multiply in only this literal's contribution

        // first handle subsumed clause case (case 2)
        for lit in new_model.difference(&self.top_state().model) {
            let matching_polarity = if lit.polarity() {
                &self.contains_pos_lit
            } else {
                &self.contains_neg_lit
            };
            for clause_idx in matching_polarity[lit.label().value_usize()].iter() {
                if new_set.contains(clause_idx) {
                    continue;
                }
                new_set.insert(clause_idx);
                for (clause_lit, weight) in self.clauses[clause_idx].iter() {
                    if !self.top_state().model.is_set(clause_lit.label()) {
                        hash = hash.wrapping_mul(*weight);
                    }
                }
            }
        }

        // now handle case 3
        for lit in new_model.difference(&self.top_state().model) {
            let opposite_polarity = if lit.polarity() {
                &self.contains_neg_lit
            } else {
                &self.contains_pos_lit
            };
            for clause_idx in opposite_polarity[lit.label().value_usize()].iter() {
                if new_set.contains(clause_idx) {
                    // avoid double-counting
                    continue;
                }
                for (clause_lit, weight) in self.clauses[clause_idx].iter() {
                    if clause_lit.label() == lit.label() {
                        hash = hash.wrapping_mul(*weight);
                        break;
                    }
                }
            }
        }

        (hash, new_set)
    }

    /// returns a new SATSolver
    /// None if initially UNSAT
    pub fn new(cnf: Cnf) -> Option<SATSolver> {
        match UnitPropagate::new(cnf.clone()) {
            None => None,
            Some((up, state)) => {
                // normalize the clauses by (1) deduplicating and (2) filtering
                // out all clauses that contain a literal and its negation
                let clauses: Vec<Vec<Literal>> = cnf
                    .clauses()
                    .iter()
                    .map(|clause| {
                        let mut c = clause.clone();
                        c.sort();
                        c.dedup();
                        c
                    })
                    .collect();

                let i = clauses.iter().filter(|clause| {
                    for i in 0..clause.len() {
                        for j in (i + 1)..clause.len() {
                            if clause[i].label() == clause[j].label()
                                && clause[i].polarity() != clause[j].polarity()
                            {
                                return false;
                            }
                        }
                    }
                    true
                });

                // weight each literal
                let mut primes = primal::Primes::all();
                let clauses: Vec<Vec<(Literal, u128)>> = i
                    .map({
                        |clause| {
                            clause
                                .iter()
                                .map(|lit| (*lit, primes.next().unwrap() as u128))
                                .collect()
                        }
                    })
                    .collect();

                // initialize pos_lit and neg_lit
                let mut pos_lit = Vec::new();
                let mut neg_lit = Vec::new();
                for _ in 0..(cnf.num_vars()) {
                    pos_lit.push(BitSet::new());
                    neg_lit.push(BitSet::new());
                }

                for (clause_idx, clause) in clauses.iter().enumerate() {
                    for lit in clause.iter() {
                        if lit.0.polarity() {
                            pos_lit[lit.0.label().value_usize()].insert(clause_idx);
                        } else {
                            neg_lit[lit.0.label().value_usize()].insert(clause_idx);
                        }
                    }
                }

                let top_state = SatState {
                    model: PartialModel::new(cnf.num_vars()),
                    hash: 1,
                    sat_clauses: BitSet::new(),
                };
                let mut solver = SATSolver {
                    up,
                    clauses,
                    contains_pos_lit: pos_lit,
                    contains_neg_lit: neg_lit,
                    state_stack: vec![top_state],
                };

                let (new_hash, new_sat_set) = solver.update_hash_and_sat_set(&state);

                solver.state_stack.push(SatState {
                    model: state,
                    hash: new_hash,
                    sat_clauses: new_sat_set,
                });

                Some(solver)
            }
        }
    }

    /// Pops the SAT state
    ///
    /// Restores the set of clause and decisions to the point at which it was previously pushed
    /// Panics if there is no prior pushed state.
    pub fn pop(&mut self) {
        self.state_stack.pop();
    }

    /// Sets a literal in the SAT context
    /// pushes this decision onto the stack
    pub fn decide(&mut self, assignment: Literal) -> DecisionResult {
        match self.up.decide(self.top_state().model.clone(), assignment) {
            UnitPropResult::UNSAT => DecisionResult::UNSAT,
            UnitPropResult::PartialSAT(new_model) => {
                let (new_hash, new_sat) = self.update_hash_and_sat_set(&new_model);
                let num_set = new_sat.len();
                self.state_stack.push(SatState {
                    model: new_model,
                    hash: new_hash,
                    sat_clauses: new_sat,
                });
                if num_set == self.clauses.len() {
                    DecisionResult::SAT
                } else {
                    DecisionResult::Unknown
                }
            }
        }
    }

    /// Get an iterator over the difference between the units implied at
    /// the top and second-to-top decision
    pub fn difference_iter(&self) -> impl Iterator<Item = Literal> + '_ {
        self.top_state()
            .model
            .difference(&self.state_stack[self.state_stack.len() - 2].model)
    }

    pub fn cur_hash(&self) -> u128 {
        self.top_state().hash
    }

    pub fn is_sat(&self) -> bool {
        return self.top_state().sat_clauses.len() == self.clauses.len();
    }

    pub fn is_set(&self, var: VarLabel) -> bool {
        self.top_state().model.is_set(var)
    }
}

#[test]
fn test_unit_propagate_1() {
    let v = vec![
        vec![Literal::new(VarLabel::new(0), false)],
        vec![
            Literal::new(VarLabel::new(0), true),
            Literal::new(VarLabel::new(1), false),
        ],
        vec![
            Literal::new(VarLabel::new(1), true),
            Literal::new(VarLabel::new(2), true),
        ],
    ];

    let cnf = Cnf::new(&v);
    match UnitPropagate::new(cnf) {
        None => panic!("test failed - no partial model generated"),
        Some((_, assgn)) => {
            assert_eq!(assgn.get(VarLabel::new(0)), Some(false));
            assert_eq!(assgn.get(VarLabel::new(1)), Some(false));
            assert_eq!(assgn.get(VarLabel::new(2)), Some(true));
        }
    }
}

#[test]
fn test_unit_propagate_2() {
    let v = vec![
        vec![
            Literal::new(VarLabel::new(0), true),
            Literal::new(VarLabel::new(1), false),
        ],
        vec![
            Literal::new(VarLabel::new(1), true),
            Literal::new(VarLabel::new(2), true),
        ],
    ];

    let cnf = Cnf::new(&v);
    match UnitPropagate::new(cnf) {
        None => panic!("test failed - no partial model generated"),
        Some((_, assgn)) => {
            assert_eq!(assgn.get(VarLabel::new(0)), None);
            assert_eq!(assgn.get(VarLabel::new(1)), None);
            assert_eq!(assgn.get(VarLabel::new(2)), None);
        }
    }
}

#[test]
fn test_unit_propagate_3() {
    let v = vec![
        vec![Literal::new(VarLabel::new(0), false)],
        vec![
            Literal::new(VarLabel::new(0), false),
            Literal::new(VarLabel::new(1), false),
        ],
        vec![
            Literal::new(VarLabel::new(0), false),
            Literal::new(VarLabel::new(1), true),
        ],
        vec![
            Literal::new(VarLabel::new(1), true),
            Literal::new(VarLabel::new(2), true),
        ],
    ];

    let cnf = Cnf::new(&v);
    match UnitPropagate::new(cnf) {
        None => panic!("test failed - no partial model generated"),
        Some((_, assgn)) => {
            assert_eq!(assgn.get(VarLabel::new(0)), Some(false));
            assert_eq!(assgn.get(VarLabel::new(1)), None);
            assert_eq!(assgn.get(VarLabel::new(2)), None);
        }
    }
}

// #[test]
// fn test_unit_propagate_3() {
//     let v = vec![
//         vec![Literal::new(VarLabel::new(0), false)],
//         vec![
//             Literal::new(VarLabel::new(0), false),
//             Literal::new(VarLabel::new(1), false),
//         ],
//         vec![
//             Literal::new(VarLabel::new(0), false),
//             Literal::new(VarLabel::new(1), true),
//         ],
//         vec![
//             Literal::new(VarLabel::new(1), true),
//             Literal::new(VarLabel::new(2), true),
//         ],
//     ];

//     let cnf = Cnf::new(&v);
//     let up = UnitPropagate::new(&cnf).unwrap();
//     let assgn = up.get_assgn().get_vec();
//     assert_eq!(assgn[0], Some(false));
//     assert_eq!(assgn[1], None);
//     assert_eq!(assgn[2], None);
// }

// #[test]
// fn test_unit_propagate_4() {
//     let v = vec![
//         vec![
//             Literal::new(VarLabel::new(0), false),
//             Literal::new(VarLabel::new(1), false),
//         ],
//     ];

//     let cnf = Cnf::new(&v);
//     let mut up = UnitPropagate::new(&cnf).unwrap();
//     let v1 = up.decide(Literal::new(VarLabel::new(0), false));
//     assert!(v1);
//     let v = up.decide(Literal::new(VarLabel::new(1), true));
//     assert_eq!(v, true);
//     up.pop();
//     let v = up.decide(Literal::new(VarLabel::new(1), false));
//     assert_eq!(v, true);
// }

// #[test]
// fn test_unit_propagate_5() {
//     let v = vec![vec![Literal::new(VarLabel::new(1), true), Literal::new(VarLabel::new(3), true)],
//                  vec![Literal::new(VarLabel::new(3), false), Literal::new(VarLabel::new(2), true), Literal::new(VarLabel::new(4), true)]];
//     let cnf = Cnf::new(&v);
//     let mut up = UnitPropagate::new(&cnf).unwrap();
//     let v1 = up.decide(Literal::new(VarLabel::new(3), true));
//     assert!(v1);
// }
