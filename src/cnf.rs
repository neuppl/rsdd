use bdd::{VarLabel, Op, BddPtr};
use sdd_manager::SddManager;
use manager::{BddManager};
use ref_table::ExternalRef;
use sdd::SddPtr;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Cnf {
    clauses: Vec<Vec<(VarLabel, bool)>>,
}

impl Cnf {
    pub fn from_file(fname: String) -> Cnf {
        use dimacs::*;
        let r = parse_dimacs(&fname).unwrap();
        let (_, cvec) = match r {
            Instance::Cnf { num_vars, clauses } => (num_vars, clauses),
            _ => panic!(),
        };
        let mut clause_vec: Vec<Vec<(VarLabel, bool)>> = Vec::new();
        for itm in cvec.iter() {
            let mut lit_vec: Vec<(VarLabel, bool)> = Vec::new();
            for l in itm.lits().iter() {
                let b = match l.sign() {
                    Sign::Neg => false,
                    Sign::Pos => true,
                };
                lit_vec.push((VarLabel::new(l.var().to_u64()), b));
            }
            clause_vec.push(lit_vec);
        }
        Cnf { clauses: clause_vec }
    }

    pub fn into_sdd(&self, manager: &mut SddManager) -> ExternalRef {
        let mut cvec : Vec<ExternalRef> = Vec::with_capacity(self.clauses.len());
        for lit_vec in self.clauses.iter() {
            assert!(lit_vec.len() > 0, "empty cnf");
            let (vlabel, val) = lit_vec[0];
            let mut sdd = manager.var(vlabel, val);
            for i in 1..lit_vec.len() {;
                let (vlabel, val) = lit_vec[i];
                let var = manager.var(vlabel, val);
                sdd = manager.apply(Op::BddOr, sdd, var);
            }
            cvec.push(sdd);
        }
        // now cvec has a list of all the clauses; collapse it down
        fn helper(vec: &[ExternalRef], man: &mut SddManager) -> Option<ExternalRef> {
            if vec.len() == 0 {
                None
            } else if vec.len() == 1 {
                return Some(vec[0])
            } else {
                let (l, r) = vec.split_at(vec.len() / 2);
                let sub_l = helper(l, man);
                let sub_r = helper(r, man);
                match (sub_l, sub_r) {
                    (None, None) => None,
                    (Some(v), None) | (None, Some(v)) => Some(v),
                    (Some(l), Some(r)) => {
                        Some(man.apply(Op::BddAnd, l, r))
                    }
                }
            }
        }
        helper(&cvec, manager).unwrap()
    }


    pub fn into_bdd(&self, manager: &mut BddManager) -> BddPtr {
        let mut cvec : Vec<BddPtr> = Vec::with_capacity(self.clauses.len());
        for lit_vec in self.clauses.iter() {
            assert!(lit_vec.len() > 0, "empty cnf");
            let (vlabel, val) = lit_vec[0];
            let mut bdd = manager.var(vlabel, val);
            for i in 1..lit_vec.len() {;
                let (vlabel, val) = lit_vec[i];
                let var = manager.var(vlabel, val);
                bdd = manager.or(bdd, var);
            }
            cvec.push(bdd);
        }
        // now cvec has a list of all the clauses; collapse it down
        fn helper(vec: &[BddPtr], man: &mut BddManager) -> Option<BddPtr> {
            if vec.len() == 0 {
                None
            } else if vec.len() == 1 {
                return Some(vec[0])
            } else {
                let (l, r) = vec.split_at(vec.len() / 2);
                let sub_l = helper(l, man);
                let sub_r = helper(r, man);
                match (sub_l, sub_r) {
                    (None, None) => None,
                    (Some(v), None) | (None, Some(v)) => Some(v),
                    (Some(l), Some(r)) => {
                        Some(man.and(l, r))
                    }
                }
            }
        }
        helper(&cvec, manager).unwrap()
    }
}
