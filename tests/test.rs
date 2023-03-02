//! Contains all randomized and integration tests for the RSDD library
//! Many of these are property based tests that are created using QuickCheck

extern crate rsdd;
#[macro_use]
extern crate quickcheck;
use crate::repr::cnf::Cnf;
use crate::repr::var_label::VarLabel;
use rsdd::builder::bdd_builder::BddManager;
use rsdd::builder::cache::all_app::AllTable;
use rsdd::builder::sdd_builder::SddManager;
use rsdd::repr::bdd::BddPtr;
use rsdd::repr::vtree::VTree;
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
        let mut man = BddManager::<AllTable<BddPtr>>::new_default_order(cnf1.num_vars());
        let r1 = man.from_cnf(&cnf1);
        let r2 = man.from_cnf(&cnf2);
        assert!(
            man.eq_bdd(r1, r2),
            "Not eq\nCNF 1: {:?}\nCNF 2: {:?}\nBDD 1:{}\n BDD 2: {}",
            cnf1,
            cnf2,
            r1.to_string_debug(),
            r2.to_string_debug()
        );
    }
}

#[test]
fn test_sdd_canonicity() {
    for (cnf1, cnf2) in get_canonical_forms().into_iter() {
        let v: Vec<VarLabel> = (0..cnf1.num_vars())
            .map(|x| VarLabel::new(x as u64))
            .collect();
        let vtree = VTree::even_split(&v, 1);
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

#[test]
fn test_sdd_is_canonical() {
    for (cnf1, cnf2) in get_canonical_forms().into_iter() {
        let v: Vec<VarLabel> = (0..cnf1.num_vars())
            .map(|x| VarLabel::new(x as u64))
            .collect();
        let vtree = VTree::even_split(&v, 1);
        let mut man = SddManager::new(vtree);
        let r1 = man.from_cnf(&cnf1);
        let r2 = man.from_cnf(&cnf2);
        assert!(
            r1.is_canonical(),
            "Not canonical\nCNF 1: {:?}\nSDD 1:{}",
            cnf1,
            man.print_sdd(r1),
        );
        assert!(
            r2.is_canonical(),
            "Not canonical\nCNF 2: {:?}\nSDD 2:{}",
            cnf2,
            man.print_sdd(r2),
        );
    }
}

#[cfg(test)]
mod test_bdd_manager {
    use crate::builder::decision_nnf_builder::DecisionNNFBuilder;
    use crate::repr::cnf::Cnf;
    use crate::repr::var_label::VarLabel;
    use num::abs;
    use quickcheck::TestResult;
    use rsdd::builder::cache::all_app::AllTable;
    use rsdd::builder::cache::lru_app::BddApplyTable;

    use rsdd::repr::bdd::BddPtr;
    use rsdd::repr::ddnnf::DDNNFPtr;
    use rsdd::repr::model::PartialModel;
    use rsdd::repr::var_order::VarOrder;
    use rsdd::repr::wmc::WmcParams;
    use std::collections::HashMap;
    use std::iter::FromIterator;

    quickcheck! {
        fn test_cond_and(c: Cnf) -> bool {
            let mut mgr = super::BddManager::<AllTable<BddPtr>>::new_default_order(16);
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
            let mut mgr = super::BddManager::<AllTable<BddPtr>>::new_default_order(16);
            let cnf1 = mgr.from_cnf(&c1);
            let cnf2 = mgr.from_cnf(&c2);

            let itebdd = mgr.ite(cnf1, cnf2, BddPtr::false_ptr());
            let andbdd = mgr.and(cnf1, cnf2);

            andbdd == itebdd
        }
    }

    quickcheck! {
        fn bdd_ite_iff(c1: Cnf, c2: Cnf) -> TestResult {
            if c1.num_vars() == 0 || c1.num_vars() > 8 { return TestResult::discard() }
            if c1.clauses().len() > 12 { return TestResult::discard() }
            let mut mgr = super::BddManager::<AllTable<BddPtr>>::new_default_order(16);
            let cnf1 = mgr.from_cnf(&c1);
            let cnf2 = mgr.from_cnf(&c2);
            let iff1 = mgr.iff(cnf1, cnf2);

            let clause1 = mgr.and(cnf1, cnf2);
            let clause2 = mgr.and(cnf1.neg(), cnf2.neg());
            let and = mgr.or(clause1, clause2);

            if and != iff1 {
                println!("cnf1: {}", c1);
                println!("cnf2: {}", c2);
                println!("not equal: Bdd1: {}, Bdd2: {}", and.to_string_debug(), iff1.to_string_debug());
            }
            TestResult::from_bool(and == iff1)
        }
    }

    quickcheck! {
        fn bdd_topdown(c1: Cnf) -> TestResult {
            if c1.num_vars() == 0 || c1.num_vars() > 8 { return TestResult::discard() }
            if c1.clauses().len() > 12 { return TestResult::discard() }
            let mut mgr = super::BddManager::<AllTable<BddPtr>>::new_default_order(c1.num_vars());
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
            let mut mgr = super::BddManager::<AllTable<BddPtr>>::new_default_order(c1.num_vars());
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
            let mut mgr = super::BddManager::<AllTable<BddPtr>>::new_default_order(c1.num_vars());
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
            let mut mgr = super::BddManager::<AllTable<BddPtr>>::new_default_order(c1.num_vars());
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

            let mut mgr = super::BddManager::<AllTable<BddPtr>>::new_default_order(c1.num_vars());
            let weight_map : HashMap<VarLabel, (f64, f64)> = HashMap::from_iter(
                (0..16).map(|x| (VarLabel::new(x as u64), (0.2, 0.8))));
            let cnf1 = mgr.from_cnf(&c1);
            let bddwmc = super::repr::wmc::WmcParams::new_with_default(0.0, 1.0, weight_map.clone());
            let bddres = cnf1.wmc(mgr.get_order(), &bddwmc);
            let cnfres = c1.wmc(&weight_map);
            TestResult::from_bool(abs(bddres - cnfres) < 0.00001)
        }
    }

    quickcheck! {
        fn wmc_bdd_dnnf_eq(c1: Cnf) -> TestResult {
            // constrain the size
            if c1.num_vars() == 0 || c1.num_vars() > 8 { return TestResult::discard() }
            if c1.clauses().len() > 16 { return TestResult::discard() }

            let mut mgr = super::BddManager::<AllTable<BddPtr>>::new_default_order(c1.num_vars());
            let weight_map : HashMap<VarLabel, (f64, f64)> = HashMap::from_iter(
                (0..16).map(|x| (VarLabel::new(x as u64), (0.3, 0.7))));
            let cnf1 = mgr.from_cnf(&c1);

            let mut mgr2 = DecisionNNFBuilder::new(c1.num_vars());
            let dnnf = mgr2.from_cnf_topdown(&VarOrder::linear_order(c1.num_vars()), &c1);

            let bddwmc = super::repr::wmc::WmcParams::new_with_default(0.0, 1.0, weight_map);
            let bddres = cnf1.wmc(mgr.get_order(),  &bddwmc);
            let dnnfres = dnnf.wmc(mgr.get_order(), &bddwmc);
            let eps = f64::abs(bddres - dnnfres) < 0.0001;
            if !eps {
              println!("error on input {}: bddres {}, cnfres {}\n topdown bdd: {}\nbottom-up bdd: {}",
                c1, bddres, dnnfres, dnnf.to_string_debug(), cnf1.to_string_debug());
            }
            TestResult::from_bool(eps)
        }
    }

    quickcheck! {
        /// test if the lru cache and the all cache give the same results
        fn bdd_lru(c1: Cnf) -> TestResult {
            let mut mgr1 = super::BddManager::<AllTable<BddPtr>>::new_default_order(16);
            let mut mgr2 = super::BddManager::<BddApplyTable<BddPtr>>::new_default_order_lru(16);

            let weight_map : HashMap<VarLabel, (f64, f64)> = HashMap::from_iter(
                (0..16).map(|x| (VarLabel::new(x as u64), (0.3, 0.7))));

            let bddwmc = super::repr::wmc::WmcParams::new_with_default(0.0, 1.0, weight_map);
            let cnf1 = mgr1.from_cnf(&c1);
            let cnf2 = mgr2.from_cnf(&c1);
            let wmc1 = cnf1.wmc(mgr1.get_order(), &bddwmc);
            let wmc2 = cnf2.wmc(mgr2.get_order(), &bddwmc);
            TestResult::from_bool(f64::abs(wmc1 - wmc2) < 0.00001)
        }
    }

    quickcheck! {
        fn marginal_map(c1: Cnf) -> TestResult {
            use rsdd::repr::model::PartialModel;
            // constrain the size
            if c1.num_vars() < 5 || c1.num_vars() > 8 { return TestResult::discard() }
            if c1.clauses().len() > 14 { return TestResult::discard() }

            let mut mgr = super::BddManager::<AllTable<BddPtr>>::new_default_order(c1.num_vars());
            let weight_map : HashMap<VarLabel, (f64, f64)> = HashMap::from_iter(
                (0..16).map(|x| (VarLabel::new(x as u64), (0.3, 0.7))));
            let cnf = mgr.from_cnf(&c1);
            let vars = vec![VarLabel::new(0), VarLabel::new(2), VarLabel::new(4)];
            let wmc = WmcParams::new_with_default(0.0, 1.0, weight_map);
            let (marg_prob, _marg_assgn) = cnf.marginal_map(&vars, mgr.num_vars(), &wmc);
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
                let poss_max = conj.wmc(mgr.get_order(), &wmc);
                if poss_max > max {
                    max = poss_max;
                    max_assgn.set(VarLabel::new(0), *v1);
                    max_assgn.set(VarLabel::new(2), *v2);
                    max_assgn.set(VarLabel::new(4), *v3);
                }
            }
            if f64::abs(max - marg_prob) > 0.00001 {
                println!("cnf: {}", c1);
                println!("true map probability: {max}\nGot map probability: {marg_prob} with assignment {:?}", _marg_assgn);
            }
            TestResult::from_bool(f64::abs(max - marg_prob) < 0.00001)
        }
    }
}

#[cfg(test)]
mod test_sdd_manager {
    use crate::builder::bdd_builder::BddManager;
    use crate::repr::cnf::Cnf;
    use crate::repr::var_label::{Literal, VarLabel};
    use quickcheck::TestResult;
    use rsdd::builder::cache::all_app::AllTable;
    use rsdd::repr::bdd::BddPtr;
    use rsdd::repr::ddnnf::DDNNFPtr;
    use rsdd::repr::vtree::VTree;
    use rsdd::repr::wmc::WmcParams;
    use std::collections::{HashMap, HashSet};
    use std::iter::FromIterator;

    quickcheck! {
        fn test_cond_and(c: Cnf) -> bool {
            let order : Vec<VarLabel> = (0..16).map(VarLabel::new).collect();
            let mut mgr = super::SddManager::new(VTree::even_split(&order, 4));
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
        fn ite_iff_rightlinear(c1: Cnf, c2: Cnf) -> bool {
            // println!("testing with cnf {:?}, {:?}", c1, c2);
            let order : Vec<VarLabel> = (0..16).map(VarLabel::new).collect();
            // let vtree = VTree::even_split(&order, 4);
            let vtree = VTree::right_linear(&order);
            let mut mgr = super::SddManager::new(vtree);
            let cnf1 = mgr.from_cnf(&c1);
            let cnf2 = mgr.from_cnf(&c2);
            let iff1 = mgr.iff(cnf1, cnf2);

            let clause1 = mgr.and(cnf1, cnf2);
            let clause2 = mgr.and(cnf1.neg(), cnf2.neg());
            let and = mgr.or(clause1, clause2);

            if and != iff1 {
                println!("Not equal:\n{}\n{}", mgr.print_sdd(and), mgr.print_sdd(iff1));
            }

            and == iff1
        }
    }

    quickcheck! {
        fn ite_iff_split(c1: Cnf, c2: Cnf) -> bool {
            // println!("testing with cnf {:?}, {:?}", c1, c2);
            let order : Vec<VarLabel> = (0..16).map(VarLabel::new).collect();
            let vtree = VTree::even_split(&order, 4);
            // let vtree = VTree::right_linear(&order);
            let mut mgr = super::SddManager::new(vtree);
            let cnf1 = mgr.from_cnf(&c1);
            let cnf2 = mgr.from_cnf(&c2);
            let iff1 = mgr.iff(cnf1, cnf2);

            let clause1 = mgr.and(cnf1, cnf2);
            let clause2 = mgr.and(cnf1.neg(), cnf2.neg());
            let and = mgr.or(clause1, clause2);

            if and != iff1 {
                println!("Not equal:\n{}\n{}", mgr.print_sdd(and), mgr.print_sdd(iff1));
            }

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
            let mut mgr = super::SddManager::new(VTree::even_split(&order, 3));
            let cnf_sdd = mgr.from_cnf(&cnf);
            let sdd_wmc = WmcParams::new_with_default(0.0, 1.0, weight_map);
            let sdd_res = cnf_sdd.wmc(mgr.get_vtree_manager(), &sdd_wmc);


            let mut bddmgr = BddManager::<AllTable<BddPtr>>::new_default_order(cnf.num_vars());
            let cnf_bdd = bddmgr.from_cnf(&cnf);
            let bdd_res = cnf_bdd.wmc(bddmgr.get_order(), &sdd_wmc);

            if f64::abs(sdd_res - bdd_res) > 0.00001 {
                println!("not equal for cnf {}: sdd_res:{sdd_res}, bdd_res: {bdd_res}", cnf);
                println!("sdd: {}", mgr.print_sdd(cnf_sdd));
                TestResult::from_bool(false)
            } else {
                TestResult::from_bool(true)
            }
        }
    }

    quickcheck! {
        fn sdd_compressed_right_linear(c: Cnf) -> bool {
            let order : Vec<VarLabel> = (0..16).map(VarLabel::new).collect();
            let vtree = VTree::right_linear(&order);
            let mut mgr = super::SddManager::new(vtree);
            let cnf = mgr.from_cnf(&c);
            cnf.is_compressed()
        }
    }

    quickcheck! {
        fn sdd_trimmed_right_linear(c: Cnf) -> bool {
            let order : Vec<VarLabel> = (0..16).map(VarLabel::new).collect();
            let vtree = VTree::right_linear(&order);
            let mut mgr = super::SddManager::new(vtree);
            let cnf = mgr.from_cnf(&c);

            cnf.is_trimmed()
        }
    }

    quickcheck! {
        fn sdd_compressed_arbitrary_vtree(c: Cnf, vtree: VTree) -> bool {
            let mut mgr = super::SddManager::new(vtree);
            let cnf = mgr.from_cnf(&c);
            cnf.is_compressed()
        }
    }

    quickcheck! {
        fn sdd_trimmed_arbitrary_vtree(c: Cnf, vtree: VTree) -> bool {
            let mut mgr = super::SddManager::new(vtree);
            let cnf = mgr.from_cnf(&c);
            cnf.is_trimmed()
        }
    }

    quickcheck! {
        fn prob_equiv_trivial(c: Cnf, vtree:VTree) -> bool {
            let mut mgr1 = super::SddManager::new(vtree.clone());
            let c1 = mgr1.from_cnf(&c);

            let mut mgr2 = super::SddManager::new(vtree);
            let c2 = mgr2.from_cnf(&c);

            let prime = 1123; // large enough for our purposes
            let map = mgr1.create_prob_map(prime);

            let h1 = c1.get_semantic_hash(&map, prime);
            let h2 = c2.get_semantic_hash(&map, prime);

            h1 == h2
        }
    }

    quickcheck! {
        fn prob_equiv_sdd_identity_uncompressed(c: Cnf, vtree:VTree) -> TestResult {
            let mut compr_mgr = super::SddManager::new(vtree.clone());
            let compr_cnf = compr_mgr.from_cnf(&c);

            let mut uncompr_mgr = super::SddManager::new(vtree);
            uncompr_mgr.set_compression(false);
            let uncompr_cnf = uncompr_mgr.from_cnf(&c);

            let prime = 4391; // large enough for our purposes

            let map = uncompr_mgr.create_prob_map(prime);

            let compr_h = compr_cnf.get_semantic_hash(&map, prime);
            let uncompr_h = uncompr_cnf.get_semantic_hash(&map, prime);

            if compr_h != uncompr_h {
                println!("not equal! hashes: compr: {compr_h}, uncompr: {uncompr_h}");
                println!("map: {:?}", map);
                println!("compr sdd: {}", compr_mgr.print_sdd(compr_cnf));
                println!("uncompr sdd: {}", uncompr_mgr.print_sdd(uncompr_cnf));
                TestResult::from_bool(false)
            } else {
                TestResult::from_bool(true)
            }
        }
    }

    quickcheck! {
        fn prob_equiv_sdd_inequality(c1: Cnf, c2: Cnf, vtree:VTree) -> TestResult {
            let mut mgr = super::SddManager::new(vtree);
            let cnf_1 = mgr.from_cnf(&c1);
            let cnf_2 = mgr.from_cnf(&c2);

            if cnf_1 == cnf_2 {
                return TestResult::discard();
            }

            let prime = 4391; // large enough for our purposes

            // running iteratively, taking majority

            let mut num_collisions = 0;

            for _ in 1 .. 5 {

                let map = mgr.create_prob_map(prime);

                let h1 = cnf_1.get_semantic_hash(&map, prime);
                let h2 = cnf_2.get_semantic_hash(&map, prime);

                if h1 == h2 {
                    println!("collision! h1: {h1}, h2: {h2}");
                    println!("map: {:?}", map);
                    println!("sdd1: {}", mgr.print_sdd(cnf_1));
                    println!("sdd2: {}", mgr.print_sdd(cnf_2));
                    num_collisions += 1;
                }
            }
            TestResult::from_bool(num_collisions < 2) // less than half
        }
    }

    quickcheck! {
        /// verify that every node in the SDD has a unique semantic hash
        fn qc_sdd_canonicity(c1: Cnf, vtree:VTree) -> TestResult {
            let mut mgr = super::SddManager::new(vtree);
            let _ = mgr.from_cnf(&c1);
            // take a large prime to make collisions impossible 
            // useful site: http://compoasso.free.fr/primelistweb/page/prime/liste_online_en.php
            let prime = 10037; 

            // running iteratively, taking majority
            let map = mgr.create_prob_map(prime);
            let seen_hashes : HashSet<u128> = HashSet::new();
            for sdd in mgr.node_iter() {
                let hash = sdd.get_semantic_hash(&map, prime);
                if seen_hashes.contains(&hash) {
                    return TestResult::from_bool(false);
                }
            }
            return TestResult::from_bool(true);
        }
    }
}
