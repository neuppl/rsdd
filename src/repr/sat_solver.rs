//! A generic SAT solver for CNFs


use picorust::picosat;
use picosat::PicoSAT;

use super::{var_label::Literal, model::PartialModel, cnf::{Cnf, UnitPropagate}};

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
    pub fn push(&mut self) -> () {
        picosat::push(&mut self.sat_state);
    }

    /// Pops the SAT state
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

    /// Queries the current top SAT state to get the currently implied 
    /// partial model
    pub fn get_partial_model(&mut self) -> PartialModel {
        self.up.get_assgn().clone()
        // query for every variable
        // let mut r : Vec<Option<bool>> = vec![None; self.num_vars];
        // for i in (0 as i32)..(self.num_vars as i32) {
        //     match picosat::deref_toplevel(&mut self.sat_state, i+1) {
        //         0 => (),
        //         l if l > 0 => {
        //             r[i as usize] = Some(true);
        //         }, 
        //         l if l < 0 => {
        //             r[i as usize] = Some(false);
        //         }
        //         _ => panic!()
        //     }
        // }
        // return PartialModel::from_vec(r);
    }
}