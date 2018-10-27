use repr::var_label::*;

struct Clause {
    lits: Vec<Literal>
}

struct SatSolver {
    decision_sequence: Vec<Literal>,
    /// Knowledge-base which holds the entire CNF and all learned clauses
    kb: Vec<Clause>,
    polarity: Vec<Literal>
}

impl SatSolver {

}
