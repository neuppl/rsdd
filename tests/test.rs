//! Contains all randomized and integration tests for the RSDD library
//! Many of these are property based tests that are created using QuickCheck

extern crate rsdd;
#[macro_use]
extern crate quickcheck;
use crate::builder::bdd_builder::{BddManager, BddWmc};
use crate::builder::sdd_builder::{even_split, SddManager, SddWmc};
use crate::repr::cnf::Cnf;
use crate::repr::var_label::VarLabel;
use rsdd::*;
extern crate rand;

/// A list of canonical forms in DIMACS form. The goal of these tests is to ensure that caching
/// and application are working as intended
static C1_A: &str = "
p cnf 5 3
1 2 0
-1 2 0
";

static C1_B: &str = "
p cnf 2 1
2 0
";

static C2_A: &str = "
p cnf 5 3
1 2 3 0
1 2 0
-1 2 0
";

static C2_B: &str = "
p cnf 2 1
2 0
";

static C3_A: &str = "
p cnf 5 3
1 2 3 4 5 0
1 2 0
-1 2 0
";

static C3_B: &str = "
p cnf 2 1
2 0
";

static C4_A: &str = "
p cnf 5 3
-1 2 0
1
";

static C4_B: &str = "
p cnf 2 1
1 0
2 0
";

static C5_A: &str = "
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

static C5_B: &str = "
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

static C6_A: &str = "
p cnf 8 3
1 2 3 4 5 6 7 8 0
1 8 0
";

static C6_B: &str = "
p cnf 2 1
1 8 0
";

static C7_A: &str = "
p cnf 8 3
1 2 3 4 5 6 7 8 0
3 5 0
";

static C7_B: &str = "
p cnf 2 1
3 5 0
";

static C8_A: &str = "
p cnf 8 3
1 2 3 4 5 6 7 8 0
1 5 0
";

static C8_B: &str = "
p cnf 2 1
1 5 0
";

static C9_A: &str = "
p cnf 8 3
1 2 3 4 5 6 7 8 0
2 6 0
";

static C9_B: &str = "
p cnf 2 1
2 6 0
";

static C10_A: &str = "
p cnf 8 3
1 2 3 4 5 6 7 8 0
2 7 0
";

static C10_B: &str = "
p cnf 2 1
2 7 0
";

static C11_A: &str = "
p cnf 8 3
1 2 3 4 5 6 7 8 0
1 3 5 0
";

static C11_B: &str = "
p cnf 2 1
1 3 5 0
";

static C12_A: &str = "
p cnf 8 3
1 2 3 4 5 6 7 8 0
2 4 8 0
";

static C12_B: &str = "
p cnf 2 1
2 4 8 0
";

static C13_A: &str = "
p cnf 8 3
1 2 3 4 5 6 7 8 0
1 2 3 4 5 6 7 0
";

static C13_B: &str = "
p cnf 2 1
1 2 3 4 5 6 7 0
";

static C14_A: &str = "
p cnf 8 3
1 2 3 4 5 6 7 8 0
2 3 4 5 6 7 8 0
";

static C14_B: &str = "
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

#[test]
fn test_bdd_canonicity() {
    for (cnf1, cnf2) in get_canonical_forms().into_iter() {
        let mut man = BddManager::new_default_order(cnf1.num_vars());
        let r1 = man.from_cnf(&cnf1);
        let r2 = man.from_cnf(&cnf2);
        assert!(
            man.eq_bdd(r1, r2),
            "Not eq\nCNF 1: {:?}\nCNF 2: {:?}\nBDD 1:{}\n BDD 2: {}",
            cnf1,
            cnf2,
            man.to_string_debug(r1),
            man.to_string_debug(r2)
        );
    }
}

#[test]
fn test_sdd_canonicity() {
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
    use crate::builder::decision_nnf_builder::DecisionNNFBuilder;
    use crate::repr::cnf::Cnf;
    use crate::repr::var_label::VarLabel;
    use quickcheck::TestResult;
    use rsdd::builder::bdd_builder::BddWmc;
    use rsdd::builder::var_order::VarOrder;
    use rsdd::repr::model::PartialModel;
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
        fn test_ite_and(c1: Cnf, c2: Cnf) -> bool {
            let mut mgr = super::BddManager::new_default_order(16);
            let cnf1 = mgr.from_cnf(&c1);
            let cnf2 = mgr.from_cnf(&c2);

            let itebdd = mgr.ite(cnf1, cnf2, mgr.false_ptr());
            let andbdd = mgr.and(cnf1, cnf2);

            andbdd == itebdd
        }
    }

    quickcheck! {
        fn bdd_ite_iff(c1: Cnf, c2: Cnf) -> TestResult {
            if c1.num_vars() == 0 || c1.num_vars() > 8 { return TestResult::discard() }
            if c1.clauses().len() > 12 { return TestResult::discard() }
            let mut mgr = super::BddManager::new_default_order(16);
            let cnf1 = mgr.from_cnf(&c1);
            let cnf2 = mgr.from_cnf(&c2);
            let iff1 = mgr.iff(cnf1, cnf2);

            let clause1 = mgr.and(cnf1, cnf2);
            let clause2 = mgr.and(cnf1.neg(), cnf2.neg());
            let and = mgr.or(clause1, clause2);

            TestResult::from_bool(and == iff1)
        }
    }

    quickcheck! {
        fn bdd_topdown(c1: Cnf) -> TestResult {
            if c1.num_vars() == 0 || c1.num_vars() > 8 { return TestResult::discard() }
            if c1.clauses().len() > 12 { return TestResult::discard() }
            let mut mgr = super::BddManager::new_default_order(c1.num_vars());
            let cnf1 = mgr.from_cnf(&c1);
            let cnf2 = mgr.from_cnf_topdown(&c1);
            // println!("bdd 1: {}, bdd 2: {}", mgr.to_string_debug(cnf1), mgr.to_string_debug(cnf2));
            assert_eq!(cnf1, cnf2);
            TestResult::from_bool(cnf1 == cnf2)
        }
    }

    quickcheck! {
        fn compile_with_assignments(c1: Cnf) -> TestResult {
            if c1.num_vars() < 3 || c1.num_vars() > 8 { return TestResult::discard() }
            if c1.clauses().len() > 12 { return TestResult::discard() }
            let mut mgr = super::BddManager::new_default_order(c1.num_vars());
            let mut pm = PartialModel::from_litvec(&Vec::new(), c1.num_vars());
            pm.set(VarLabel::new(0), true);
            pm.set(VarLabel::new(1), true);
            let cnf1 = mgr.from_cnf_with_assignments(&c1, &pm);
            let mut cnf2 = mgr.from_cnf(&c1);
            cnf2 = mgr.condition(cnf2, VarLabel::new(0), true);
            cnf2 = mgr.condition(cnf2, VarLabel::new(1), true);
            assert_eq!(cnf1, cnf2);
            TestResult::from_bool(cnf1 == cnf2)
        }
    }

    quickcheck! {
        fn compile_topdown_with_assignments(c1: Cnf) -> TestResult {
            if c1.num_vars() < 3 || c1.num_vars() > 8 { return TestResult::discard() }
            if c1.clauses().len() > 12 { return TestResult::discard() }
            let mut mgr = super::BddManager::new_default_order(c1.num_vars());
            let mut pm = PartialModel::from_litvec(&Vec::new(), c1.num_vars());
            pm.set(VarLabel::new(0), true);
            pm.set(VarLabel::new(1), true);
            let cnf1 = mgr.from_cnf_topdown_partial(&c1, &pm);
            let mut cnf2 = mgr.from_cnf(&c1);
            cnf2 = mgr.condition(cnf2, VarLabel::new(0), true);
            cnf2 = mgr.condition(cnf2, VarLabel::new(1), true);
            assert_eq!(cnf1, cnf2);
            TestResult::from_bool(cnf1 == cnf2)
        }
    }

    quickcheck! {
        fn bdd_partial_topdown(c1: Cnf) -> TestResult {
            if c1.num_vars() < 3 || c1.num_vars() > 8 { return TestResult::discard() }
            if c1.clauses().len() > 12 { return TestResult::discard() }
            let mut mgr = super::BddManager::new_default_order(c1.num_vars());
            let mut pm = PartialModel::from_litvec(&Vec::new(), c1.num_vars());
            pm.set(VarLabel::new(0), true);
            pm.set(VarLabel::new(1), true);
            let cnf1 = mgr.from_cnf_with_assignments(&c1, &pm);
            let cnf2 = mgr.from_cnf_topdown_partial(&c1, &pm);
            // println!("bdd 1: {}, bdd 2: {}", mgr.to_string_debug(cnf1), mgr.to_string_debug(cnf2));
            assert_eq!(cnf1, cnf2);
            TestResult::from_bool(cnf1 == cnf2)
        }
    }

    quickcheck! {
        fn wmc_eq(c1: Cnf) -> TestResult {
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

    quickcheck! {
        fn wmc_bdd_dnnf_eq(c1: Cnf) -> TestResult {
            // constrain the size
            if c1.num_vars() == 0 || c1.num_vars() > 8 { return TestResult::discard() }
            if c1.clauses().len() > 16 { return TestResult::discard() }

            let mut mgr = super::BddManager::new_default_order(c1.num_vars());
            let weight_map : HashMap<VarLabel, (f64, f64)> = HashMap::from_iter(
                (0..16).map(|x| (VarLabel::new(x as u64), (0.3, 0.7))));
            let cnf1 = mgr.from_cnf(&c1);

            let mut mgr2 = DecisionNNFBuilder::new(c1.num_vars());
            let dnnf = mgr2.from_cnf_topdown(&VarOrder::linear_order(c1.num_vars()), &c1);

            let bddwmc = super::BddWmc::new_with_default(0.0, 1.0, weight_map);
            let bddres = mgr.wmc(cnf1, &bddwmc);
            let dnnfres = mgr2.unsmsoothed_wmc(dnnf, &bddwmc);
            let eps = f64::abs(bddres - dnnfres) < 0.0001;
            if !eps {
              println!("error on input {}: bddres {}, cnfres {}\n topdown bdd: {}\nbottom-up bdd: {}", c1.to_string(), bddres, dnnfres, mgr2.to_string_debug(dnnf), mgr.to_string_debug(cnf1));
            }
            TestResult::from_bool(eps)
        }
    }

    quickcheck! {
        fn marginal_map(c1: Cnf) -> TestResult {
            use rsdd::repr::model::PartialModel;
            // constrain the size
            if c1.num_vars() < 5 || c1.num_vars() > 8 { return TestResult::discard() }
            if c1.clauses().len() > 14 { return TestResult::discard() }

            let mut mgr = super::BddManager::new_default_order(c1.num_vars());
            let weight_map : HashMap<VarLabel, (f64, f64)> = HashMap::from_iter(
                (0..16).map(|x| (VarLabel::new(x as u64), (0.3, 0.7))));
            let cnf = mgr.from_cnf(&c1);
            let vars = vec![VarLabel::new(0), VarLabel::new(2), VarLabel::new(4)];
            let wmc = BddWmc::new_with_default(0.0, 1.0, weight_map);
            let (marg_prob, _marg_assgn) = mgr.marginal_map(cnf, mgr.true_ptr(), &vars, &wmc);
            let assignments = vec![(true, true, true), (true, true, false), (true, false, true), (true, false, false),
                                   (false, true, true), (false, true, false), (false, false, true), (false, false, false)];

            let mut max : f64 = -10.0;
            let mut max_assgn : PartialModel = PartialModel::from_litvec(&[], c1.num_vars());
            for (v1, v2, v3) in assignments.iter() {
                let x = mgr.var(VarLabel::new(0), *v1);
                let y = mgr.var(VarLabel::new(2), *v2);
                let z = mgr.var(VarLabel::new(4), *v3);
                let mut conj = mgr.and(x, y);
                conj = mgr.and(conj, z);
                conj = mgr.and(conj, cnf);
                let poss_max = mgr.wmc(conj, &wmc);
                if poss_max > max {
                    max = poss_max;
                    max_assgn.set(VarLabel::new(0), *v1);
                    max_assgn.set(VarLabel::new(2), *v2);
                    max_assgn.set(VarLabel::new(4), *v3);
                }
            }
            TestResult::from_bool(f64::abs(max - marg_prob) < 0.00001)
        }
    }
}

#[cfg(test)]
mod test_sdd_manager {
    use crate::builder::bdd_builder::{BddManager, BddWmc};
    use crate::repr::cnf::Cnf;
    use crate::repr::var_label::{Literal, VarLabel};
    use quickcheck::TestResult;
    use std::collections::HashMap;
    use std::iter::FromIterator;

    quickcheck! {
        fn test_cond_and(c: Cnf) -> bool {
            let order : Vec<VarLabel> = (0..16).map(VarLabel::new).collect();
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
            let order : Vec<VarLabel> = (0..16).map(VarLabel::new).collect();
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
