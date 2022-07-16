//! A generic SAT solver for CNFs
//! This SAT solver supports incremental solving via a push/pop interface


use picorust::picosat;
use picosat::PicoSAT;

use super::{var_label::{Literal, VarLabel}, model::PartialModel, cnf::Cnf};

type clause_idx = usize;
type lit_idx = usize;



/// A data-structure for efficient implementation of unit propagation with CNFs.
/// It implements a two-literal watching scheme.
/// For instance, for the CNF:
///     c0             c1
/// (A \/ !B) /\ (C \/ !A \/ B)
///  ^     ^      ^    ^
/// The literals A and !B are watched in c0, and C and !A  are watched in c1
/// This is stored in the watch lists as:
/// watch_list_pos:
///    A: [c0]
///    B: []
///    C: [c1]
/// watch_list_neg:
///    A: [c1]
///    B: [c0] 
///    C: []
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
pub struct UnitPropagate<'a> {
    // watch_list_pos[i] is a list of the clauses that are watching the positive
    // literal of varlabel i
    watch_list_pos: Vec<Vec<clause_idx>>,
    // similar to the above, but for negative label
    watch_list_neg: Vec<Vec<clause_idx>>,
    // stack of assignment states (all implied and decided literals)
    state: Vec<PartialModel>,
    cnf: &'a Cnf,
}

impl<'a> UnitPropagate<'a> {
    /// Returns None if UNSAT discovered during initial unit propagation
    pub fn new(cnf: &'a Cnf) -> UnitPropagate<'a> {
        let mut watch_list_pos: Vec<Vec<usize>> = Vec::new();
        let mut watch_list_neg: Vec<Vec<usize>> = Vec::new();
        let mut assignments: Vec<Option<bool>> = Vec::new();
        for _ in 0..cnf.num_vars() {
            watch_list_pos.push(Vec::new());
            watch_list_neg.push(Vec::new());
            assignments.push(None);
        }

        // do initial unit propagation
        let mut implied: Vec<Literal> = Vec::new();
        for (idx, c) in cnf.clauses().iter().enumerate() {
            if c.len() == 0 {
                continue;
                // return None;
            }
            if c.len() == 1 {
                implied.push(c[0]);
                continue;
            }
            if c[1].get_polarity() {
                watch_list_pos[c[1].get_label().value() as usize].push(idx)
            } else {
                watch_list_neg[c[1].get_label().value() as usize].push(idx)
            }
            if c[0].get_polarity() {
                watch_list_pos[c[0].get_label().value() as usize].push(idx)
            } else {
                watch_list_neg[c[0].get_label().value() as usize].push(idx)
            }
        }

        let mut cur = UnitPropagate {
            watch_list_pos,
            watch_list_neg,
            state: vec![PartialModel::from_vec(assignments)],
            cnf,
        };
        return cur;
    }
    
    fn cur_state(&self) -> &PartialModel {
        &self.state.last().unwrap()
    }

    fn cur_state_mut(&mut self) -> &mut PartialModel {
        let n = self.state.len();
        &mut self.state[n - 1]
    }

    /// Sets a variable to a particular value
    pub fn decide(&mut self, new_assignment: Literal) -> bool {
        // push a fresh state onto the stack and propagate
        self.state.push(self.cur_state().clone());
        // run until no new discovered units
        // let mut prev_num_assigned = self.cur_state().assignment_iter().count();
        // loop {
            if !self.set(new_assignment) {
                return false;
            }
            // let new_num_assigned = self.cur_state().assignment_iter().count();
            // if new_num_assigned == prev_num_assigned {
            //     return true;
            // }
            // prev_num_assigned = new_num_assigned;
        // }
        return true
    }

    /// Returns an iterator over the literals that were decided at the previous decision step
    /// Panics if no previous decision step was made
    pub fn get_decided_literals(&self) -> impl Iterator<Item=Literal> + '_ {
        // the most recently decided literals is the diff between the new
        let n = self.state.len();
        let cur_state_i = self.state[n-1].get_vec().iter();
        let prev_state_i = self.state[n-2].get_vec().iter();
        let iter = cur_state_i.zip(prev_state_i).enumerate().filter_map(|(idx, (cur, prev))| {
            if *cur == *prev {
                None
            } else {
                Some(Literal::new(VarLabel::new_usize(idx), cur.unwrap()))
            }
        });
        iter
    }

    /// Backtracks to the previous decision
    pub fn backtrack(&mut self) -> () {
        assert!(self.state.len() > 1, "Unit Propagate cannot backtrack past first decision");
        self.state.pop();
    }

    /// Set a variable to a particular value and propagates
    /// returns true if success, false if UNSAT
    fn set(&mut self, new_assignment: Literal) -> bool {
        // if already assigned, check if consistent -- if not, return unsat
        match self.cur_state().get(new_assignment.get_label()) {
            None => (),
            Some(v) => {
                if new_assignment.get_polarity() != v {
                    return false;
                } else {
                    return true;
                }
            }
        };

        // update the value of the decided variable in the partial model
        self.cur_state_mut()
            .set(new_assignment.get_label(), new_assignment.get_polarity());

        let var_idx = new_assignment.get_label().value() as usize;

        // track a list of all discovered implications
        // indexes over the watchers for the current literal
        let mut watcher_idx = 0;
        // some of the internal structure here is weird to accommodate the
        // borrow checker.
        loop {
            // first check if there are any watchers; if no watchers left, break 

            if new_assignment.get_polarity() {
                if watcher_idx >= self.watch_list_neg[var_idx].len() {
                    break;
                }
            } else {
                if watcher_idx >= self.watch_list_pos[var_idx].len() {
                    break;
                }
            }
            let clause = if new_assignment.get_polarity() {
                &self.cnf.clauses()[self.watch_list_neg[var_idx][watcher_idx]]
            } else {
                &self.cnf.clauses()[self.watch_list_pos[var_idx][watcher_idx]]
            };

            // if clause is satisfied, do not change its watched literals and
            // move onto the next watcher
            let mut is_sat = false;
            for lit in clause.iter() {
                match self.cur_state().get(lit.get_label()) {
                    Some(v) if lit.get_polarity() == v => {
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
            let mut remaining_lits =
                clause
                    .iter()
                    .filter(|x| {
                        match self.cur_state().get(x.get_label()) {
                            None => true,
                            Some(_) => false,
                        }
                    });

            let num_remaining = remaining_lits.clone().count();
            if num_remaining == 0 {
                // UNSAT -- need to move a watcher and there are no remaining
                // unassigned literals to watch
                return false;
            } else if num_remaining == 1 {
                // unit propagate and move onto the next watcher
                let new_unit = remaining_lits.nth(0).unwrap();
                if !self.set(*new_unit) {
                    return false;
                }
                watcher_idx += 1;
            } else {
                // num_remaining > 1, find a new literal to watch
                // first, find a new literal to watch
                let candidate_unwatched : lit_idx = remaining_lits.clone().nth(0).unwrap().get_label().value_usize();
                // check if candidate_unwatched is already being watched; if it
                // is, pick another literal to watch
                let prev_watcher : clause_idx = if new_assignment.get_polarity() {
                    self.watch_list_neg[var_idx][watcher_idx]
                } else {
                    self.watch_list_pos[var_idx][watcher_idx]
                };

                let new_lit : &Literal = if new_assignment.get_polarity() {
                    if self.watch_list_pos[candidate_unwatched].contains(&prev_watcher) {
                        remaining_lits.nth(1).unwrap()
                    } else {
                        remaining_lits.nth(0).unwrap()
                    }
                } else {
                    if self.watch_list_neg[candidate_unwatched].contains(&prev_watcher) {
                        remaining_lits.nth(1).unwrap()
                    } else {
                        remaining_lits.nth(0).unwrap()
                    }
                };

                let new_loc = new_lit.get_label().value_usize();

                if new_assignment.get_polarity() {
                    self.watch_list_neg[var_idx].swap_remove(watcher_idx);
                } else {
                    self.watch_list_pos[var_idx].swap_remove(watcher_idx);
                };

                // now add the new one
                if new_lit.get_polarity() {
                    self.watch_list_pos[new_loc].push(prev_watcher);
                } else {
                    self.watch_list_neg[new_loc].push(prev_watcher);
                }
                // do not increment watcher_idx (since we decreased the total number of watchers, we have made progress)
            }
        }
        return true;
    }

    // pub fn assume(&mut self, )

    pub fn get_assgn(&self) -> &PartialModel {
        &self.cur_state()
    }
}


pub struct SATSolver<'a> {
    sat_state: PicoSAT,
    num_vars: usize,
    up: UnitPropagate<'a>
}

/// Converts a literal into the representation picosat expects
fn lit_to_pico(lit: Literal) -> i32 {
    if lit.get_polarity() {
        (lit.get_label().value() + 1) as i32
    } else {
        -((lit.get_label().value() + 1) as i32)
    }
}


impl<'a> SATSolver<'a> {
    pub fn new(cnf: &'a Cnf) -> SATSolver<'a> {
        let mut state = picosat::init();
        for _ in 0..cnf.num_vars() {
            picosat::inc_max_var(&mut state);
        }
        

        let mut r = SATSolver { sat_state: state, num_vars: cnf.num_vars(), up: UnitPropagate::new(cnf) };
        // add all initial clauses to the SAT state
        for c in cnf.clauses() {
            r.add_clause(c)
        }
        r
    }

    /// Pushes the SAT context 
    /// 
    /// Saves all current clauses and decisions
    pub fn push(&mut self) -> () {
        picosat::push(&mut self.sat_state);
    }

    /// Pops the SAT state
    /// 
    /// Restores the set of clause and decisions to the point at which it was previously pushed
    /// Panics if there is no prior pushed state.
    pub fn pop(&mut self) -> () {
        self.up.backtrack();
        picosat::pop(&mut self.sat_state);
    }

    /// Adds a clause to the current SAT state
    pub fn add_clause(&mut self, clause: &[Literal]) -> () {
        for lit in clause.iter() {
            picosat::add(&mut self.sat_state, lit_to_pico(*lit));
        }
        picosat::add(&mut self.sat_state, 0);
    }

    /// Sets a literal in the SAT context
    pub fn decide(&mut self, lit: Literal) -> () {
        self.up.decide(lit);
        self.add_clause(&[lit])
    }

    /// True if the formula is UNSAT
    pub fn unsat(&mut self) -> bool {
        let res = picosat::sat(&mut self.sat_state, 10);
        return res == 20;
    }

    /// Get the set of currently implied units
    pub fn get_implied_units(&mut self) -> PartialModel {
        self.up.get_assgn().clone()
    }
}

// #[test]
// fn test_unit_propagate_1() {
//     let v = vec![
//         vec![Literal::new(VarLabel::new(0), false)],
//         vec![
//             Literal::new(VarLabel::new(0), true),
//             Literal::new(VarLabel::new(1), false),
//         ],
//         vec![
//             Literal::new(VarLabel::new(1), true),
//             Literal::new(VarLabel::new(2), true),
//         ],
//     ];

//     let mut cnf = Cnf::new(v);
//     let mut up = UnitPropagate::new(&cnf).unwrap();
//     let assgn = up.get_assgn().get_vec();
//     assert_eq!(assgn[0], Some(false));
//     assert_eq!(assgn[1], Some(false));
//     assert_eq!(assgn[2], Some(true));
// }

// #[test]
// fn test_unit_propagate_2() {
//     let v = vec![
//         vec![
//             Literal::new(VarLabel::new(0), true),
//             Literal::new(VarLabel::new(1), false),
//         ],
//         vec![
//             Literal::new(VarLabel::new(1), true),
//             Literal::new(VarLabel::new(2), true),
//         ],
//     ];

//     let cnf = Cnf::new(v);
//     let up = UnitPropagate::new(&cnf).unwrap();
//     let assgn = up.get_assgn().get_vec();
//     assert_eq!(assgn[0], None);
//     assert_eq!(assgn[1], None);
//     assert_eq!(assgn[2], None);
// }

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

//     let cnf = Cnf::new(v);
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

//     let cnf = Cnf::new(v);
//     let mut up = UnitPropagate::new(&cnf).unwrap();
//     let v1 = up.decide(Literal::new(VarLabel::new(0), false));
//     assert!(v1);
//     let v = up.decide(Literal::new(VarLabel::new(1), true));
//     assert_eq!(v, true);
//     up.backtrack();
//     let v = up.decide(Literal::new(VarLabel::new(1), false));
//     assert_eq!(v, true);
// }

// #[test]
// fn test_unit_propagate_5() {
//     let v = vec![vec![Literal::new(VarLabel::new(1), true), Literal::new(VarLabel::new(3), true)],
//                  vec![Literal::new(VarLabel::new(3), false), Literal::new(VarLabel::new(2), true), Literal::new(VarLabel::new(4), true)]];
//     let cnf = Cnf::new(v);
//     let mut up = UnitPropagate::new(&cnf).unwrap();
//     let v1 = up.decide(Literal::new(VarLabel::new(3), true));
//     assert!(v1);
// }

