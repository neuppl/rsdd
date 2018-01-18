use bdd::{VarLabel, Op, BddPtr};
use sdd_manager::SddManager;
use manager::BddManager;
use std::cmp::{min, max};
use rand::{Rng, thread_rng};
use std::collections::HashSet;
use var_order::VarOrder;
use ref_table::ExternalRef;
use sdd::SddPtr;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Cnf {
    clauses: Vec<Vec<(VarLabel, bool)>>,
    num_vars: usize,
}

impl Cnf {
    pub fn from_file(v: String) -> Cnf {
        use dimacs::*;
        let r = parse_dimacs(&v).unwrap();
        let (_, cvec) = match r {
            Instance::Cnf { num_vars, clauses } => (num_vars, clauses),
            _ => panic!(),
        };
        let mut clause_vec: Vec<Vec<(VarLabel, bool)>> = Vec::new();
        let mut m = 0;
        for itm in cvec.iter() {
            let mut lit_vec: Vec<(VarLabel, bool)> = Vec::new();
            for l in itm.lits().iter() {
                let b = match l.sign() {
                    Sign::Neg => false,
                    Sign::Pos => true,
                };
                // subtract 1, we are 0-indexed
                let lbl = VarLabel::new(l.var().to_u64() - 1);
                m = max(l.var().to_u64() as usize, m);
                lit_vec.push((lbl, b));
            }
            clause_vec.push(lit_vec);
        }
        Cnf {
            clauses: clause_vec,
            num_vars: m + 1,
        }
    }

    pub fn num_vars(&self) -> usize {
        self.num_vars
    }

    pub fn new(clauses: Vec<Vec<(VarLabel, bool)>>) -> Cnf {
        let mut m = 0;
        for clause in clauses.iter() {
            for lit in clause.iter() {
                m = max(lit.0.value(), m);
            }
        }
        Cnf { clauses: clauses, num_vars: (m + 1) as usize}
    }

    pub fn into_sdd(&self, manager: &mut SddManager) -> ExternalRef {
        let mut cvec: Vec<ExternalRef> = Vec::with_capacity(self.clauses.len());
        for lit_vec in self.clauses.iter() {
            assert!(lit_vec.len() > 0, "empty cnf");
            let (vlabel, val) = lit_vec[0];
            let mut sdd = manager.var(vlabel, val);
            for i in 1..lit_vec.len() {;
                let (vlabel, val) = lit_vec[i];
                let var = manager.var(vlabel, val);
                sdd = manager.apply(Op::BddOr, sdd, var);
            }
//            println!("clause {:?}:\n{}", lit_vec, manager.print_sdd(sdd));
            cvec.push(sdd);
        }
        // now cvec has a list of all the clauses; collapse it down
        fn helper(vec: &[ExternalRef], man: &mut SddManager) -> Option<ExternalRef> {
            if vec.len() == 0 {
                None
            } else if vec.len() == 1 {
                return Some(vec[0]);
            } else {
                let (l, r) = vec.split_at(vec.len() / 2);
                let sub_l = helper(l, man);
                let sub_r = helper(r, man);
                match (sub_l, sub_r) {
                    (None, None) => None,
                    (Some(v), None) | (None, Some(v)) => Some(v),
                    (Some(l), Some(r)) => Some(man.apply(Op::BddAnd, l, r)),
                }
            }
        }
        helper(&cvec, manager).unwrap()
    }


    pub fn into_bdd(&self, manager: &mut BddManager) -> BddPtr {
        let mut cvec: Vec<BddPtr> = Vec::with_capacity(self.clauses.len());
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
                return Some(vec[0]);
            } else {
                let (l, r) = vec.split_at(vec.len() / 2);
                let sub_l = helper(l, man);
                let sub_r = helper(r, man);
                match (sub_l, sub_r) {
                    (None, None) => None,
                    (Some(v), None) | (None, Some(v)) => Some(v),
                    (Some(l), Some(r)) => Some(man.and(l, r)),
                }
            }
        }
        helper(&cvec, manager).unwrap()
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
                let this_pos = lbl_to_pos[lit.0.value() as usize];
                min_pos = min(this_pos, min_pos);
                max_pos = max(this_pos, max_pos);
            }
            total += max_pos - min_pos;
        }
        (total as f64) / (self.clauses.len() as f64)
    }

    /// computes the center of gravity of a particular clause for a given order
    fn center_of_gravity(&self, clause: &[(VarLabel, bool)], lbl_to_pos: &[usize]) -> f64 {
        let sum = clause.iter().fold(
            0,
            |acc, &(ref lbl, _)| lbl_to_pos[lbl.value() as usize] + acc,
        );
        let r = (sum as f64) / (clause.len() as f64);
        // println!("   clause: {:?}, lbl: {:?}, cog: {}",
        //          clause, lbl_to_pos, r
        // );
        r
    }

    /// heuristically generate a variable ordering which minimizes the average
    /// clause span
    pub fn force_order(&self) -> VarOrder {
        // map from position -> label (i.e., first element is the first in the
        // order)
        println!("num vars: {}", self.num_vars);
        let mut lbl_to_pos: Vec<usize> = (0..(self.num_vars)).collect();
        let mut rng = thread_rng();
        rng.shuffle(&mut lbl_to_pos);
        println!("span before order: {}", self.average_span(&lbl_to_pos));
        println!("initial order: {:?}", lbl_to_pos);
        // perform 20 iterations of force-update
        for _ in 0..10 {
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
                for &(ref lit, _) in clause.iter() {
                    let (cur_total, num_edges) = update[lit.value() as usize];
                    update[lit.value() as usize] = (cur_total + cog[idx], num_edges + 1);
                }
            }
            let avg_cog: Vec<f64> = update
                .into_iter()
                .map(|(total, cnt)| if cnt == 0 {
                    0.0
                } else {
                    total / (cnt as f64)
                })
                .collect();

            // println!("  avg cog: {:?}", avg_cog);
            let l = avg_cog.len();
            let mut avg_cog: Vec<(f64, usize)> = avg_cog.into_iter().zip(0..l).collect();
            // now sort avg_cog on the centers of gravity
            avg_cog.sort_by(|&(ref c1, _), &(ref c2, _)| c1.partial_cmp(c2).unwrap());
            // update positions
            let pos_to_lbl : Vec<usize> = avg_cog.into_iter().map(|(_, p)| p).collect();
            // now convert to lbl_to_pos
            for (idx, lbl) in pos_to_lbl.into_iter().enumerate() {
                lbl_to_pos[lbl] = idx;
            }
            // println!("new order: {:?}", lbl_to_pos);
        }
        println!("span after order: {}", self.average_span(&lbl_to_pos));
        println!("final order: {:?}", lbl_to_pos);
        let final_order: Vec<VarLabel> = lbl_to_pos
            .into_iter()
            .map(|v| VarLabel::new(v as u64))
            .collect();
        VarOrder::new(final_order)
    }
}

#[test]
fn test_force_order() {
    use std::fs::File;
    use std::io::prelude::*;
    // let cnf = Cnf {
    //     clauses: vec![
    //         vec![(VarLabel::new(0), true), (VarLabel::new(1), true)],
    //         vec![(VarLabel::new(1), true), (VarLabel::new(2), true)],
    //         vec![(VarLabel::new(2), true), (VarLabel::new(3), true)],
    //     ],
    //     num_vars: 4,
    // };
    let file_contents = File::open("/Users/sholtzen/Downloads/sdd-1.1.1/cnf/c8.cnf");
    let mut string = String::new();
    file_contents.unwrap().read_to_string(&mut string).unwrap();
    let cnf = Cnf::from_file(string);
    let new_order = cnf.force_order();
}
