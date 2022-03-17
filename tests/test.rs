//! Contains all randomized and integration tests for the RSDD library
//! Many of these are property based tests that are created using QuickCheck

extern crate rsdd;
#[macro_use]
extern crate quickcheck;
use manager::rsbdd_manager::{BddManager, BddWmc};
use manager::sdd_manager::{even_split, SddManager, SddWmc};
use manager::var_order::VarOrder;
use repr::boolexpr::BoolExpr;
use repr::cnf::Cnf;
use repr::var_label::VarLabel;
use rsdd::*;
use std::collections::HashMap;
extern crate rand;
use rand::SeedableRng;

/// A convenient wrapper for generating maps
macro_rules! map(
    { $($key:expr => $value:expr),+ } => {
        {
            let mut m = ::std::collections::HashMap::new();
            $(
                m.insert($key, $value);
            )+
                m
        }
    };
);

/// A list of canonical forms in DIMACS form. The goal of these tests is to ensure that caching
/// and application are working as intended
static C1_A: &'static str = "
p cnf 5 3
1 2 0
-1 2 0
";

static C1_B: &'static str = "
p cnf 2 1
2 0
";

static C2_A: &'static str = "
p cnf 5 3
1 2 3 0
1 2 0
-1 2 0
";

static C2_B: &'static str = "
p cnf 2 1
2 0
";

static C3_A: &'static str = "
p cnf 5 3
1 2 3 4 5 0
1 2 0
-1 2 0
";

static C3_B: &'static str = "
p cnf 2 1
2 0
";

static C4_A: &'static str = "
p cnf 5 3
-1 2 0
1
";

static C4_B: &'static str = "
p cnf 2 1
1 0
2 0
";

static C5_A: &'static str = "
p cnf 5 3
-1 2 0
-2 3 0
-3 4 0
-4 5 0
-5 6 0
-6 7 0
-7 8 0
1
";

static C5_B: &'static str = "
p cnf 2 1
1 0
2 0
3 0
4 0
5 0
6 0
7 0
8 0
";

static C6_A: &'static str = "
p cnf 8 3
1 2 3 4 5 6 7 8 0
1 8 0
";

static C6_B: &'static str = "
p cnf 2 1
1 8 0
";

static C7_A: &'static str = "
p cnf 8 3
1 2 3 4 5 6 7 8 0
3 5 0
";

static C7_B: &'static str = "
p cnf 2 1
3 5 0
";

static C8_A: &'static str = "
p cnf 8 3
1 2 3 4 5 6 7 8 0
1 5 0
";

static C8_B: &'static str = "
p cnf 2 1
1 5 0
";

static C9_A: &'static str = "
p cnf 8 3
1 2 3 4 5 6 7 8 0
2 6 0
";

static C9_B: &'static str = "
p cnf 2 1
2 6 0
";

static C10_A: &'static str = "
p cnf 8 3
1 2 3 4 5 6 7 8 0
2 7 0
";

static C10_B: &'static str = "
p cnf 2 1
2 7 0
";

static C11_A: &'static str = "
p cnf 8 3
1 2 3 4 5 6 7 8 0
1 3 5 0
";

static C11_B: &'static str = "
p cnf 2 1
1 3 5 0
";

static C12_A: &'static str = "
p cnf 8 3
1 2 3 4 5 6 7 8 0
2 4 8 0
";

static C12_B: &'static str = "
p cnf 2 1
2 4 8 0
";

static C13_A: &'static str = "
p cnf 8 3
1 2 3 4 5 6 7 8 0
1 2 3 4 5 6 7 0
";

static C13_B: &'static str = "
p cnf 2 1
1 2 3 4 5 6 7 0
";

static C14_A: &'static str = "
p cnf 8 3
1 2 3 4 5 6 7 8 0
2 3 4 5 6 7 8 0
";

static C14_B: &'static str = "
p cnf 2 1
2 3 4 5 6 7 8 0
";

fn get_canonical_forms() -> Vec<(Cnf, Cnf)> {
    vec![
        (
            Cnf::from_file(String::from(C1_A)),
            Cnf::from_file(String::from(C1_B)),
        ),
        (
            Cnf::from_file(String::from(C2_A)),
            Cnf::from_file(String::from(C2_B)),
        ),
        (
            Cnf::from_file(String::from(C3_A)),
            Cnf::from_file(String::from(C3_B)),
        ),
        (
            Cnf::from_file(String::from(C4_A)),
            Cnf::from_file(String::from(C4_B)),
        ),
        (
            Cnf::from_file(String::from(C5_A)),
            Cnf::from_file(String::from(C5_B)),
        ),
        (
            Cnf::from_file(String::from(C6_A)),
            Cnf::from_file(String::from(C6_B)),
        ),
        (
            Cnf::from_file(String::from(C7_A)),
            Cnf::from_file(String::from(C7_B)),
        ),
        (
            Cnf::from_file(String::from(C8_A)),
            Cnf::from_file(String::from(C8_B)),
        ),
        (
            Cnf::from_file(String::from(C9_A)),
            Cnf::from_file(String::from(C9_B)),
        ),
        (
            Cnf::from_file(String::from(C10_A)),
            Cnf::from_file(String::from(C10_B)),
        ),
        (
            Cnf::from_file(String::from(C11_A)),
            Cnf::from_file(String::from(C11_B)),
        ),
        (
            Cnf::from_file(String::from(C12_A)),
            Cnf::from_file(String::from(C12_B)),
        ),
        (
            Cnf::from_file(String::from(C13_A)),
            Cnf::from_file(String::from(C13_B)),
        ),
        (
            Cnf::from_file(String::from(C14_A)),
            Cnf::from_file(String::from(C14_B)),
        ),
    ]
}

fn random_assignment(num_vars: usize) -> HashMap<VarLabel, bool> {
    let mut init = HashMap::new();
    for i in 0..num_vars {
        init.insert(VarLabel::new(i as u64), rand::random());
    }
    init
}

#[test]
fn test_bdd_canonicity() -> () {
    for (cnf1, cnf2) in get_canonical_forms().into_iter() {
        let mut man = BddManager::new_default_order(cnf1.num_vars());
        let r1 = man.from_cnf(&cnf1);
        let r2 = man.from_cnf(&cnf2);
        assert!(
            man.eq_bdd(r1, r2),
            "Not eq\nCNF 1: {:?}\nCNF 2: {:?}\nBDD 1:{}\n BDD 2: {}",
            cnf1,
            cnf2,
            man.print_bdd(r1),
            man.print_bdd(r2)
        );
    }
}

#[test]
fn test_sdd_canonicity() -> () {
    for (cnf1, cnf2) in get_canonical_forms().into_iter() {
        let v: Vec<VarLabel> = (0..cnf1.num_vars())
            .map(|x| VarLabel::new(x as u64))
            .collect();
        let vtree = even_split(&v, 3);
        let mut man = SddManager::new(vtree);
        let r1 = man.from_cnf(&cnf1);
        let r2 = man.from_cnf(&cnf2);
        assert!(
            man.sdd_eq(r1, r2),
            "Not eq\nCNF 1: {:?}\nCNF 2: {:?}\nSDD 1:{}\n SDD 2: {}",
            cnf1,
            cnf2,
            man.print_sdd(r1),
            man.print_sdd(r2)
        );
    }
}


#[cfg(test)]
mod test_bdd_manager {
    use quickcheck::TestResult;
    use repr::cnf::Cnf;
    use repr::var_label::Literal;
    use repr::var_label::VarLabel;
    use std::collections::HashMap;
    use std::iter::FromIterator;

    quickcheck! {
        fn test_cond_and(c: Cnf) -> bool {
            let mut mgr = super::BddManager::new_default_order(16);
            let cnf = mgr.from_cnf(&c);
            let v1 = VarLabel::new(0);
            let bdd1 = mgr.exists(cnf, v1);

            let bdd2 = mgr.condition(cnf, v1, true);
            let bdd3 = mgr.condition(cnf, v1, false);
            let bdd4 = mgr.or(bdd2, bdd3);
            bdd4 == bdd1
        }
    }

    quickcheck! {
        fn bdd_ite_iff(c1: Vec<Vec<Literal>>, c2: Vec<Vec<Literal>>) -> TestResult {
            let c1 = Cnf::new(c1);
            let c2 = Cnf::new(c2);

            if c1.num_vars() == 0 || c1.num_vars() > 8 { return TestResult::discard() }
            if c1.clauses().len() > 12 { return TestResult::discard() }
            let mut mgr = super::BddManager::new_default_order(16);
            let cnf1 = mgr.from_cnf(&c1);
            let cnf2 = mgr.from_cnf(&c2);
            let iff1 = mgr.iff(cnf1, cnf2);

            let clause1 = mgr.or(cnf1, cnf2.neg());
            let clause2 = mgr.or(cnf1.neg(), cnf2);
            let and = mgr.and(clause1, clause2);

            TestResult::from_bool(and == iff1)
        }
    }

    quickcheck! {
        fn wmc_eq(clauses: Vec<Vec<Literal>>) -> TestResult {
            let c1 = Cnf::new(clauses);

            // constrain the size
            if c1.num_vars() == 0 || c1.num_vars() > 8 { return TestResult::discard() }
            if c1.clauses().len() > 16 { return TestResult::discard() }

            let mut mgr = super::BddManager::new_default_order(c1.num_vars());
            let weight_map : HashMap<VarLabel, (usize, usize)> = HashMap::from_iter(
                (0..16).map(|x| (VarLabel::new(x as u64), (2, 3))));
            let cnf1 = mgr.from_cnf(&c1);
            let bddwmc = super::BddWmc::new_with_default(0, 1, weight_map.clone());
            let bddres = mgr.wmc(cnf1, &bddwmc);
            let cnfres = c1.wmc(&weight_map);
            if bddres != cnfres {
              println!("error on input {}: bddres {}, cnfres {}", c1.to_string(), bddres, cnfres);
            }
            TestResult::from_bool(bddres == cnfres)
        }
    }
}


#[cfg(test)]
mod test_sdd_manager {
    use repr::cnf::Cnf;
    use manager::rsbdd_manager::{BddManager, BddWmc};
    use repr::var_label::{VarLabel, Literal};
    use quickcheck::TestResult;
    use std::collections::HashMap;
    use std::iter::FromIterator;

  quickcheck! {
      fn test_cond_and(c: Cnf) -> bool {
          let order : Vec<VarLabel> = (0..16).map(|x| VarLabel::new(x)).collect();
          let mut mgr = super::SddManager::new(super::even_split(&order, 4));
          let cnf = mgr.from_cnf(&c);
          let v1 = VarLabel::new(0);
          let bdd1 = mgr.exists(cnf, v1);

          let bdd2 = mgr.condition(cnf, v1, true);
          let bdd3 = mgr.condition(cnf, v1, false);
          let bdd4 = mgr.or(bdd2, bdd3);
          bdd4 == bdd1
      }
  }

  quickcheck! {
      fn ite_iff(c1: Cnf, c2: Cnf) -> bool {
          let order : Vec<VarLabel> = (0..16).map(|x| VarLabel::new(x)).collect();
          let mut mgr = super::SddManager::new(super::even_split(&order, 4));
          let cnf1 = mgr.from_cnf(&c1);
          let cnf2 = mgr.from_cnf(&c2);
          let iff1 = mgr.iff(cnf1, cnf2);

          let clause1 = mgr.or(cnf1, cnf2.neg());
          let clause2 = mgr.or(cnf1.neg(), cnf2);
          let and = mgr.and(clause1, clause2);

          and == iff1
      }
  }


  quickcheck! {
      fn sdd_wmc_eq(clauses: Vec<Vec<Literal>>) -> TestResult {

          let cnf = Cnf::new(clauses);
          if cnf.num_vars() < 9 || cnf.num_vars() > 16 { return TestResult::discard() }
          if cnf.clauses().len() > 16 { return TestResult::discard() }

         let weight_map : HashMap<VarLabel, (f64, f64)> = HashMap::from_iter(
              (0..cnf.num_vars()).map(|x| (VarLabel::new(x as u64), (0.5, 0.5))));

          let order : Vec<VarLabel> = (0..cnf.num_vars()).map(|x| VarLabel::new(x as u64)).collect();
          let mut mgr = super::SddManager::new(super::even_split(&order, 3));
          let cnf_sdd = mgr.from_cnf(&cnf);
          let sdd_wmc = super::SddWmc::new_with_default(0.0, 1.0, &mut mgr, &weight_map);
          let sdd_res = mgr.unsmoothed_wmc(cnf_sdd, &sdd_wmc);


          let mut bddmgr = BddManager::new_default_order(cnf.num_vars());
          let cnf_bdd = bddmgr.from_cnf(&cnf);
          let bdd_res = bddmgr.wmc(cnf_bdd, &BddWmc::new_with_default(0.0, 1.0, weight_map));

          TestResult::from_bool(sdd_res == bdd_res)
      }
  }


}

