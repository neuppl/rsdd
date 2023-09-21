//! Contains all randomized and integration tests for the RSDD library
//! Many of these are property based tests that are created using QuickCheck

extern crate rsdd;
#[macro_use]
extern crate quickcheck;
use rsdd::builder::bdd::RobddBuilder;
use rsdd::builder::cache::AllIteTable;
use rsdd::builder::sdd::{CompressionSddBuilder, SddBuilder};
use rsdd::builder::BottomUpBuilder;
use rsdd::repr::BddPtr;
use rsdd::repr::Cnf;
use rsdd::repr::VTree;
use rsdd::repr::VarLabel;
use rsdd::*;

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

fn canonical_forms() -> Vec<(Cnf, Cnf)> {
    vec![
        (Cnf::from_dimacs(C1_A), Cnf::from_dimacs(C1_B)),
        (Cnf::from_dimacs(C2_A), Cnf::from_dimacs(C2_B)),
        (Cnf::from_dimacs(C3_A), Cnf::from_dimacs(C3_B)),
        (Cnf::from_dimacs(C4_A), Cnf::from_dimacs(C4_B)),
        (Cnf::from_dimacs(C5_A), Cnf::from_dimacs(C5_B)),
        (Cnf::from_dimacs(C6_A), Cnf::from_dimacs(C6_B)),
        (Cnf::from_dimacs(C7_A), Cnf::from_dimacs(C7_B)),
        (Cnf::from_dimacs(C8_A), Cnf::from_dimacs(C8_B)),
        (Cnf::from_dimacs(C9_A), Cnf::from_dimacs(C9_B)),
        (Cnf::from_dimacs(C10_A), Cnf::from_dimacs(C10_B)),
        (Cnf::from_dimacs(C11_A), Cnf::from_dimacs(C11_B)),
        (Cnf::from_dimacs(C12_A), Cnf::from_dimacs(C12_B)),
        (Cnf::from_dimacs(C13_A), Cnf::from_dimacs(C13_B)),
        (Cnf::from_dimacs(C14_A), Cnf::from_dimacs(C14_B)),
    ]
}

#[test]
fn test_bdd_canonicity() {
    for (cnf1, cnf2) in canonical_forms().into_iter() {
        let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(cnf1.num_vars());
        let r1 = builder.compile_cnf(&cnf1);
        let r2 = builder.compile_cnf(&cnf2);
        assert!(
            builder::BottomUpBuilder::eq(&builder, r1, r2),
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
    for (cnf1, cnf2) in canonical_forms().into_iter() {
        let v: Vec<VarLabel> = (0..cnf1.num_vars())
            .map(|x| VarLabel::new(x as u64))
            .collect();
        let vtree = VTree::even_split(&v, 1);
        let builder = CompressionSddBuilder::new(vtree);
        let r1 = builder.compile_cnf(&cnf1);
        let r2 = builder.compile_cnf(&cnf2);
        assert!(
            builder::BottomUpBuilder::eq(&builder, r1, r2),
            "Not eq\nCNF 1: {:?}\nCNF 2: {:?}\nSDD 1:{}\n SDD 2: {}",
            cnf1,
            cnf2,
            builder.print_sdd(r1),
            builder.print_sdd(r2)
        );
    }
}

#[test]
fn test_sdd_is_canonical() {
    for (cnf1, cnf2) in canonical_forms().into_iter() {
        let v: Vec<VarLabel> = (0..cnf1.num_vars())
            .map(|x| VarLabel::new(x as u64))
            .collect();
        let vtree = VTree::even_split(&v, 1);
        let builder = CompressionSddBuilder::new(vtree);
        let r1 = builder.compile_cnf(&cnf1);
        let r2 = builder.compile_cnf(&cnf2);
        assert!(
            r1.is_canonical(),
            "Not canonical\nCNF 1: {:?}\nSDD 1:{}",
            cnf1,
            builder.print_sdd(r1),
        );
        assert!(
            r2.is_canonical(),
            "Not canonical\nCNF 2: {:?}\nSDD 2:{}",
            cnf2,
            builder.print_sdd(r2),
        );
    }
}

#[cfg(test)]
mod test_bdd_builder {
    use quickcheck::TestResult;
    use rand::Rng;
    use rsdd::builder::bdd::BddBuilder;
    use rsdd::builder::bdd::RobddBuilder;
    use rsdd::builder::cache::AllIteTable;
    use rsdd::builder::cache::LruIteTable;
    use rsdd::builder::decision_nnf::DecisionNNFBuilder;
    use rsdd::builder::decision_nnf::StandardDecisionNNFBuilder;
    use rsdd::builder::BottomUpBuilder;
    use rsdd::constants::primes;
    use rsdd::repr::BddPtr;
    use rsdd::repr::Cnf;
    use rsdd::repr::DTree;
    use rsdd::repr::PartialModel;
    use rsdd::repr::VTree;
    use rsdd::repr::VarLabel;
    use rsdd::repr::VarOrder;
    use rsdd::repr::WmcParams;
    use rsdd::repr::{create_semantic_hash_map, DDNNFPtr};
    use rsdd::util::semirings::ExpectedUtility;
    use rsdd::util::semirings::RealSemiring;
    use rsdd::util::semirings::Semiring;
    use std::collections::HashMap;
    use std::iter::FromIterator;

    quickcheck! {
        fn test_cond_and(c: Cnf) -> bool {
            let builder = super::RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(16);
            let cnf = builder.compile_cnf(&c);
            let v1 = VarLabel::new(0);
            let bdd1 = builder.exists(cnf, v1);

            let bdd2 = builder.condition(cnf, v1, true);
            let bdd3 = builder.condition(cnf, v1, false);
            let bdd4 = builder.or(bdd2, bdd3);
            bdd4 == bdd1
        }
    }

    quickcheck! {
        fn test_ite_and(c1: Cnf, c2: Cnf) -> bool {
            let builder = super::RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(16);
            let cnf1 = builder.compile_cnf(&c1);
            let cnf2 = builder.compile_cnf(&c2);

            let itebdd = builder.ite(cnf1, cnf2, BddPtr::false_ptr());
            let andbdd = builder.and(cnf1, cnf2);

            andbdd == itebdd
        }
    }

    quickcheck! {
        fn bdd_ite_iff(c1: Cnf, c2: Cnf) -> TestResult {
            if c1.num_vars() == 0 || c1.num_vars() > 8 { return TestResult::discard() }
            if c1.clauses().len() > 12 { return TestResult::discard() }
            let builder = super::RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(16);
            let cnf1 = builder.compile_cnf(&c1);
            let cnf2 = builder.compile_cnf(&c2);
            let iff1 = builder.iff(cnf1, cnf2);

            let clause1 = builder.and(cnf1, cnf2);
            let clause2 = builder.and(cnf1.neg(), cnf2.neg());
            let and = builder.or(clause1, clause2);

            if and != iff1 {
                println!("cnf1: {}", c1);
                println!("cnf2: {}", c2);
                println!("not equal: Bdd1: {}, Bdd2: {}", and.to_string_debug(), iff1.to_string_debug());
            }
            TestResult::from_bool(and == iff1)
        }
    }

    quickcheck! {
        fn compile_with_assignments(c1: Cnf) -> TestResult {
            if c1.num_vars() < 3 || c1.num_vars() > 8 { return TestResult::discard() }
            if c1.clauses().len() > 12 { return TestResult::discard() }
            let builder = super::RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(c1.num_vars());
            let mut pm = PartialModel::from_litvec(&Vec::new(), c1.num_vars());
            pm.set(VarLabel::new(0), true);
            pm.set(VarLabel::new(1), true);
            let cnf1 = builder.compile_cnf_with_assignments(&c1, &pm);
            let mut cnf2 = builder.compile_cnf(&c1);
            cnf2 = builder.condition(cnf2, VarLabel::new(0), true);
            cnf2 = builder.condition(cnf2, VarLabel::new(1), true);
            assert_eq!(cnf1, cnf2);
            TestResult::from_bool(cnf1 == cnf2)
        }
    }

    quickcheck! {
        /// test that an BDD and CNF compute identical WMC
        fn wmc_eq(c1: Cnf) -> TestResult {
            // constrain the size
            if c1.num_vars() == 0 || c1.num_vars() > 8 { return TestResult::discard() }
            if c1.clauses().len() > 16 { return TestResult::discard() }

            let builder = super::RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(c1.num_vars());
            let weight = create_semantic_hash_map::<{primes::U32_SMALL}>(c1.num_vars());
            let cnf1 = builder.compile_cnf(&c1);
            let bddres = cnf1.unsmoothed_wmc(&weight);
            let cnfres = c1.wmc(&weight);
            TestResult::from_bool(bddres == cnfres)
        }
    }

    quickcheck! {
        /// test that an SDD and BDD both have the same semantic hash
        fn sdd_semantic_eq_bdd(c1: Cnf, vtree: VTree) -> bool {
            let bdd_builder = super::RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(c1.num_vars());
            let sdd_builder = super::CompressionSddBuilder::new(vtree);
            let map : WmcParams<rsdd::util::semirings::FiniteField<{primes::U32_SMALL}>>= create_semantic_hash_map(c1.num_vars());
            let bdd = bdd_builder.compile_cnf(&c1);
            let sdd = sdd_builder.compile_cnf(&c1);
            bdd.semantic_hash( &map) == sdd.semantic_hash(&map)
        }
    }

    quickcheck! {
        /// test that an SDD and BDD both have the same semantic hash with min-fill order
        fn sdd_semantic_eq_bdd_dtree(c1: Cnf) -> bool {
            let bdd_builder = super::RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(c1.num_vars());
            let min_fill_order = c1.min_fill_order();
            let dtree = DTree::from_cnf(&c1, &min_fill_order);
            let vtree = VTree::from_dtree(&dtree).unwrap();

            let sdd_builder = super::CompressionSddBuilder::new(vtree);
            let map : WmcParams<rsdd::util::semirings::FiniteField<{primes::U32_SMALL}>>= create_semantic_hash_map(c1.num_vars());
            let bdd = bdd_builder.compile_cnf(&c1);
            let sdd = sdd_builder.compile_cnf(&c1);
            bdd.semantic_hash( &map) == sdd.semantic_hash(&map)
        }
    }

    quickcheck! {
        fn wmc_bdd_dnnf_eq(c1: Cnf) -> TestResult {
            // constrain the size
            if c1.num_vars() == 0 || c1.num_vars() > 8 { return TestResult::discard() }
            if c1.clauses().len() > 16 { return TestResult::discard() }

            let builder = super::RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(c1.num_vars());
            let weight_map : HashMap<VarLabel, (RealSemiring, RealSemiring)> = HashMap::from_iter(
                (0..16).map(|x| (VarLabel::new(x as u64), (RealSemiring(0.3), RealSemiring(0.7)))));
            let order = VarOrder::linear_order(c1.num_vars());
            let cnf1 = builder.compile_cnf(&c1);

            let builder2 = StandardDecisionNNFBuilder::new(order);
            let dnnf = builder2.compile_cnf_topdown(&c1);

            let bddwmc = super::repr::WmcParams::new(weight_map);
            let bddres = cnf1.unsmoothed_wmc( &bddwmc);
            let dnnfres = dnnf.unsmoothed_wmc(&bddwmc);
            let eps = f64::abs(bddres.0 - dnnfres.0) < 0.0001;
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
            let builder1 = super::RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(16);
            let builder2 = super::RobddBuilder::<LruIteTable<BddPtr>>::new_with_linear_order(16);

            let weight_map : HashMap<VarLabel, (RealSemiring, RealSemiring)> = HashMap::from_iter(
                (0..16).map(|x| (VarLabel::new(x as u64), (RealSemiring(0.3), RealSemiring(0.7)))));

            let bddwmc = super::repr::WmcParams::new(weight_map);
            let cnf1 = builder1.compile_cnf(&c1);
            let cnf2 = builder2.compile_cnf(&c1);
            let wmc1 = cnf1.unsmoothed_wmc(&bddwmc);
            let wmc2 = cnf2.unsmoothed_wmc( &bddwmc);
            TestResult::from_bool(f64::abs(wmc1.0 - wmc2.0) < 0.00001)
        }
    }

    quickcheck! {
        fn marginal_map(c1: Cnf) -> TestResult {
            use rsdd::repr::PartialModel;
            // constrain the size
            if c1.num_vars() < 5 || c1.num_vars() > 8 { return TestResult::discard() }
            if c1.clauses().len() > 14 { return TestResult::discard() }

            let builder = super::RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(c1.num_vars());
            let weight_map : HashMap<VarLabel, (RealSemiring, RealSemiring)> = HashMap::from_iter(
                (0..16).map(|x| (VarLabel::new(x as u64), (RealSemiring(0.3), RealSemiring(0.7)))));
            let cnf = builder.compile_cnf(&c1);
            let vars = vec![VarLabel::new(0), VarLabel::new(2), VarLabel::new(4)];
            if !c1.var_in_cnf(VarLabel::new(0))
               || !c1.var_in_cnf(VarLabel::new(2))
               || !c1.var_in_cnf(VarLabel::new(4)) {
                return TestResult::discard()
            }
            let wmc = WmcParams::new(weight_map);

            let (marg_prob, marg_assgn) = cnf.marginal_map(&vars, builder.num_vars(), &wmc);
            let (marg_prob_bb, marg_assgn_bb) = cnf.bb(&vars, builder.num_vars(), &wmc);
            let assignments = vec![(true, true, true), (true, true, false), (true, false, true), (true, false, false),
                                   (false, true, true), (false, true, false), (false, false, true), (false, false, false)];

            let mut max : f64 = -10.0;
            let mut max_assgn : PartialModel = PartialModel::from_litvec(&[], c1.num_vars());
            for (v1, v2, v3) in assignments.iter() {
                let x = builder.var(VarLabel::new(0), *v1);
                let y = builder.var(VarLabel::new(2), *v2);
                let z = builder.var(VarLabel::new(4), *v3);
                let mut conj = builder.and(x, y);
                conj = builder.and(conj, z);
                conj = builder.and(conj, cnf);
                let poss_max = conj.unsmoothed_wmc(&wmc);
                if poss_max.0 > max {
                    max = poss_max.0;
                    max_assgn.set(VarLabel::new(0), *v1);
                    max_assgn.set(VarLabel::new(2), *v2);
                    max_assgn.set(VarLabel::new(4), *v3);
                }
            }

            // the below tests (specifically, the bool pm_check)
            // check that the partial models evaluate to the correct margmap.
            // these pms can be different b/c of symmetries/dead literals in the CNF.
            let mut pm_check = true;
            let extract = |ob : Option<bool>| -> bool {
                match ob {
                    Some(b) => b,
                    None => panic!("none encountered")
                }
            };
            let v : Vec<bool> = (0..3).map(|x| extract(marg_assgn.get(vars[x]))).collect();
            let w : Vec<bool> = (0..3).map(|x| extract(marg_assgn_bb.get(vars[x]))).collect();
            // if v != w {
            //     println!("{:?},{:?}",v,w);
            // }
            let v0 = builder.var(vars[0], v[0]);
            let v1 = builder.var(vars[1], v[1]);
            let v2 = builder.var(vars[2], v[2]);
            let mut conj = builder.and(v0, v1);
            conj = builder.and(conj, v2);
            conj = builder.and(conj, cnf);
            let poss_max = conj.unsmoothed_wmc(&wmc);
            if f64::abs(poss_max.0 - max) > 0.0001 {
                pm_check = false;
            }
            let w0 = builder.var(vars[0], w[0]);
            let w1 = builder.var(vars[1], w[1]);
            let w2 = builder.var(vars[2], w[2]);
            let mut conj2 = builder.and(w0, w1);
            conj2 = builder.and(conj2, w2);
            builder.and(conj2, cnf);
            let poss_max2 = conj.unsmoothed_wmc(&wmc);
            if f64::abs(poss_max2.0 - max) > 0.0001 {
                pm_check = false;
            }

            TestResult::from_bool(f64::abs(max - marg_prob) < 0.00001
                                  && f64::abs(marg_prob_bb.0 - marg_prob) < 0.00001
                                  && pm_check)
        }
    }

    quickcheck! {
        fn meu(c1: Cnf) -> TestResult {
            use rsdd::repr::PartialModel;
            let n = c1.num_vars();
            // constrain the size, make BDD
            if !(5..=8).contains(&n) { return TestResult::discard() }
            if c1.clauses().len() > 14 { return TestResult::discard() }
            let builder = super::RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(n);
            let cnf = builder.compile_cnf(&c1);

            // randomizing the decisions
            let mut rng = rand::thread_rng();
            let decisions : Vec<VarLabel> = (0..3).map(|_| VarLabel::new(rng.gen_range(0..(n-2)) as u64)).collect();
            if decisions[0] == decisions[1] || decisions[1] == decisions[2] || decisions[0] == decisions[2] {
                return TestResult::discard()
            }
            if !c1.var_in_cnf(decisions[0])
               || !c1.var_in_cnf(decisions[1])
               || !c1.var_in_cnf(decisions[2]) {
                return TestResult::discard()
            }

            // weight function and weight map
            let probs : Vec<f64> = (0..n).map(|_| rng.gen_range(0.0..1.0)).collect();
            let weight_fn = |x : usize| -> (VarLabel, (ExpectedUtility, ExpectedUtility)) {
                let vx = VarLabel::new(x as u64);
                if vx == decisions[0] || vx == decisions[1] || vx == decisions[2] {
                    return (VarLabel::new(x as u64),
                    (ExpectedUtility::one(), ExpectedUtility::one()))
                }
                if x == n-1 || x == n-2 {
                    return (VarLabel::new(x as u64),
                    (ExpectedUtility::one(), ExpectedUtility(1.0, 10.0)))
                }
                let pr = probs[x];
                (vx, (ExpectedUtility(pr, 0.0), ExpectedUtility(1.0-pr, 0.0)))
            };
            let weight_map : HashMap<VarLabel, (ExpectedUtility, ExpectedUtility)> = HashMap::from_iter(
                (0..n).map(&weight_fn));

            // set up wmc, run meu
            let vars = decisions.clone();
            let wmc = WmcParams::new(weight_map);
            let (meu , _meu_assgn) = cnf.meu(builder.true_ptr(),  &vars, builder.num_vars(), &wmc);
            let (meu_bb, _meu_assgn_bb) = cnf.bb(&vars, builder.num_vars(), &wmc);

            println!("meu = {}, bb = {}\n", meu, meu_bb);

            // brute-force meu
            let assignments = vec![(true, true, true), (true, true, false), (true, false, true), (true, false, false),
                                   (false, true, true), (false, true, false), (false, false, true), (false, false, false)];
            let mut max : f64 = -10000.0;
            let mut max_assgn : PartialModel = PartialModel::from_litvec(&[], c1.num_vars());
            for (v1, v2, v3) in assignments.iter() {
                let x = builder.var(decisions[0], *v1);
                let y = builder.var(decisions[1], *v2);
                let z = builder.var(decisions[2], *v3);
                let mut conj = builder.and(x, y);
                conj = builder.and(conj, z);
                conj = builder.and(conj, cnf);
                let poss_max = conj.unsmoothed_wmc(&wmc);
                if poss_max.1 > max {
                    max = poss_max.1;
                    max_assgn.set(decisions[0], *v1);
                    max_assgn.set(decisions[1], *v2);
                    max_assgn.set(decisions[2], *v3);
                }
            }

            // and the actual checks.
            // these checks test that the meus coincide.
            let pr_check1 = f64::abs(meu.1 - meu_bb.1) < 0.00001;
            let pr_check2 = f64::abs(max - meu.1)< 0.00001;

            // the below tests (specifically, the bool pm_check)
            // check that the partial models evaluate to the correct meu.
            // these pms can be different b/c of symmetries/dead literals in the CNF.
            // let mut pm_check = true;
            // let extract = |ob : Option<bool>| -> bool {
            //     match ob {
            //         Some(b) => b,
            //         None => panic!("none encountered")
            //     }
            // };
            // let v : Vec<bool> = (0..3).map(|x| extract(meu_assgn.get(decisions[x]))).collect();
            // let w : Vec<bool> = (0..3).map(|x| extract(meu_assgn_bb.get(decisions[x]))).collect();
            // // if v != w {
            // //     println!("{:?},{:?}",v,w);
            // // }
            // let v0 = builder.var(decisions[0], v[0]);
            // let v1 = builder.var(decisions[1], v[1]);
            // let v2 = builder.var(decisions[2], v[2]);
            // let mut conj = builder.and(v0, v1);
            // conj = builder.and(conj, v2);
            // conj = builder.and(conj, cnf);
            // let poss_max = conj.unsmoothed_wmc(&wmc);
            // if f64::abs(poss_max.1 - max) > 0.0001 {
            //     pm_check = false;
            // }
            // let w0 = builder.var(decisions[0], w[0]);
            // let w1 = builder.var(decisions[1], w[1]);
            // let w2 = builder.var(decisions[2], w[2]);
            // let mut conj2 = builder.and(w0, w1);
            // conj2 = builder.and(conj2, w2);
            // builder.and(conj2, cnf);
            // let poss_max2 = conj.unsmoothed_wmc(&wmc);
            // if f64::abs(poss_max2.1 - max) > 0.0001 {
            //     pm_check = false;
            // }

            TestResult::from_bool(pr_check1 && pr_check2)
        }
    }

    quickcheck! {
        fn smooth_and_unsmooth_bdd_agree(cnf: Cnf) -> bool {
            let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(cnf.num_vars());

            let bdd = builder.compile_cnf(&cnf);

            let smoothed = builder.smooth(bdd, cnf.num_vars());

            // while it's not true that smooth and unsmooth wmc are the same generally,
            // they are with the property that pos_weight + neg_weight = 1
            let map = create_semantic_hash_map::<{primes::U32_SMALL}>(cnf.num_vars());

            bdd.semantic_hash( &map) == smoothed.semantic_hash( &map)
        }
    }
}

#[cfg(test)]
mod test_sdd_builder {
    use quickcheck::{Arbitrary, TestResult};
    use rand::rngs::SmallRng;
    use rand::seq::SliceRandom;
    use rand::SeedableRng;
    use rsdd::builder::bdd::RobddBuilder;
    use rsdd::builder::cache::AllIteTable;
    use rsdd::builder::sdd::{CompressionSddBuilder, SddBuilder, SemanticSddBuilder};
    use rsdd::builder::BottomUpBuilder;
    use rsdd::constants::primes;
    use rsdd::repr::BddPtr;
    use rsdd::repr::Cnf;
    use rsdd::repr::DTree;
    use rsdd::repr::SddPtr;
    use rsdd::repr::VTree;
    use rsdd::repr::VarOrder;
    use rsdd::repr::WmcParams;
    use rsdd::repr::{create_semantic_hash_map, DDNNFPtr};
    use rsdd::repr::{Literal, VarLabel};
    use rsdd::util::semirings::FiniteField;
    use std::collections::HashMap;

    quickcheck! {
        fn test_cond_and(c: Cnf) -> bool {
            let order : Vec<VarLabel> = (0..16).map(VarLabel::new).collect();
            let builder = super::CompressionSddBuilder::new(VTree::even_split(&order, 4));
            let cnf = builder.compile_cnf(&c);
            let v1 = VarLabel::new(0);
            let bdd1 = builder.exists(cnf, v1);

            let bdd2 = builder.condition(cnf, v1, true);
            let bdd3 = builder.condition(cnf, v1, false);
            let bdd4 = builder.or(bdd2, bdd3);
            bdd4 == bdd1
        }
    }

    quickcheck! {
        fn ite_iff_rightlinear(c1: Cnf, c2: Cnf) -> bool {
            // println!("testing with cnf {:?}, {:?}", c1, c2);
            let order : Vec<VarLabel> = (0..16).map(VarLabel::new).collect();
            // let vtree = VTree::even_split(&order, 4);
            let vtree = VTree::right_linear(&order);
            let builder = super::CompressionSddBuilder::new(vtree);
            let cnf1 = builder.compile_cnf(&c1);
            let cnf2 = builder.compile_cnf(&c2);
            let iff1 = builder.iff(cnf1, cnf2);

            let clause1 = builder.and(cnf1, cnf2);
            let clause2 = builder.and(cnf1.neg(), cnf2.neg());
            let and = builder.or(clause1, clause2);

            if and != iff1 {
                println!("Not equal:\n{}\n{}", builder.print_sdd(and), builder.print_sdd(iff1));
            }

            and == iff1
        }
    }

    quickcheck! {
        fn ite_iff_split(c1: Cnf, c2: Cnf) -> bool {
            let order : Vec<VarLabel> = (0..16).map(VarLabel::new).collect();
            let vtree = VTree::even_split(&order, 4);
            let builder = super::CompressionSddBuilder::new(vtree);
            let cnf1 = builder.compile_cnf(&c1);
            let cnf2 = builder.compile_cnf(&c2);
            let iff1 = builder.iff(cnf1, cnf2);

            let clause1 = builder.and(cnf1, cnf2);
            let clause2 = builder.and(cnf1.neg(), cnf2.neg());
            let and = builder.or(clause1, clause2);

            if and != iff1 {
                println!("Not equal:\n{}\n{}", builder.print_sdd(and), builder.print_sdd(iff1));
            }

            and == iff1
        }
    }

    quickcheck! {
        /// test that the same CNF compiled by both an SDD and BDD have the same weighted model count
        /// with an even_split ordering
        fn sdd_wmc_eq_even_split(clauses: Vec<Vec<Literal>>) -> TestResult {
            let cnf = Cnf::new(&clauses);
            if cnf.num_vars() < 8 || cnf.num_vars() > 16 { return TestResult::discard() }
            if cnf.clauses().len() > 16 { return TestResult::discard() }

           let weight_map = create_semantic_hash_map::<{primes::U32_SMALL}>(cnf.num_vars());
           let order : Vec<VarLabel> = (0..cnf.num_vars()).map(|x| VarLabel::new(x as u64)).collect();
           let builder = super::CompressionSddBuilder::new(VTree::even_split(&order, 3));
           let cnf_sdd = builder.compile_cnf(&cnf);
           let sdd_res = cnf_sdd.semantic_hash(&weight_map);


            let bdd_builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(cnf.num_vars());
            let cnf_bdd = bdd_builder.compile_cnf(&cnf);
            let bdd_res = cnf_bdd.semantic_hash( &weight_map);
            assert_eq!(bdd_res, sdd_res);
            TestResult::passed()
        }
    }

    quickcheck! {
        /// test that the same CNF compiled by both an SDD and BDD have the same weighted model count
        /// with a dtree ordering
        fn sdd_wmc_eq(clauses: Vec<Vec<Literal>>) -> TestResult {
            let cnf = Cnf::new(&clauses);
            if cnf.num_vars() < 8 || cnf.num_vars() > 16 { return TestResult::discard() }
            if cnf.clauses().len() > 16 { return TestResult::discard() }

            let dtree = DTree::from_cnf(&cnf, &VarOrder::linear_order(cnf.num_vars()));
            let vtree = VTree::from_dtree(&dtree).unwrap();

            let weight_map = create_semantic_hash_map::<{primes::U32_SMALL}>(cnf.num_vars());
            let builder = super::CompressionSddBuilder::new(vtree);
            let cnf_sdd = builder.compile_cnf(&cnf);
            let sdd_res = cnf_sdd.semantic_hash(&weight_map);


            let bdd_builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(cnf.num_vars());
            let cnf_bdd = bdd_builder.compile_cnf(&cnf);
            let bdd_res = cnf_bdd.semantic_hash( &weight_map);
            assert_eq!(bdd_res, sdd_res);
            TestResult::passed()
        }
    }

    // why does this exist?
    // well, I wasn't able to figure out how to generate a random permutation of vectors from 0..16 with quickcheck
    #[derive(Clone, Debug)]
    struct SixteenVarLabels {
        order: Vec<VarLabel>,
    }

    impl Arbitrary for SixteenVarLabels {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            let mut rng = SmallRng::seed_from_u64(u64::arbitrary(g));
            let mut order: Vec<VarLabel> = (0..16).map(VarLabel::new).collect();
            order.shuffle(&mut rng);
            SixteenVarLabels { order }
        }
    }

    quickcheck! {
        fn sdd_left_linear_predicate(s: SixteenVarLabels) -> bool {
            let vtree = VTree::left_linear(&s.order);
            vtree.is_left_linear()
        }
    }

    quickcheck! {
        fn sdd_right_linear_predicate(s: SixteenVarLabels) -> bool {
            let vtree = VTree::right_linear(&s.order);
            vtree.is_right_linear()
        }
    }

    quickcheck! {
        fn sdd_compressed_right_linear(c: Cnf) -> bool {
            let order : Vec<VarLabel> = (0..16).map(VarLabel::new).collect();
            let vtree = VTree::right_linear(&order);
            let builder = super::CompressionSddBuilder::new(vtree);
            let cnf = builder.compile_cnf(&c);
            cnf.is_compressed()
        }
    }

    quickcheck! {
        fn sdd_trimmed_right_linear(c: Cnf) -> bool {
            let order : Vec<VarLabel> = (0..16).map(VarLabel::new).collect();
            let vtree = VTree::right_linear(&order);
            let builder = super::CompressionSddBuilder::new(vtree);
            let cnf = builder.compile_cnf(&c);

            cnf.is_trimmed()
        }
    }

    quickcheck! {
        fn sdd_compressed_arbitrary_vtree(c: Cnf, vtree: VTree) -> bool {
            let builder = super::CompressionSddBuilder::new(vtree);
            let cnf = builder.compile_cnf(&c);
            cnf.is_compressed()
        }
    }

    quickcheck! {
        fn sdd_trimmed_arbitrary_vtree(c: Cnf, vtree: VTree) -> bool {
            let builder = super::CompressionSddBuilder::new(vtree);
            let cnf = builder.compile_cnf(&c);
            cnf.is_trimmed()
        }
    }

    quickcheck! {
        fn prob_equiv_identical(c: Cnf, vtree:VTree) -> bool {
            let builder1 = CompressionSddBuilder::new(vtree.clone());
            let c1 = builder1.compile_cnf(&c);

            let builder2 = SemanticSddBuilder::<{primes::U32_SMALL}>::new(vtree);
            let c2 = builder2.compile_cnf(&c);

            let map : WmcParams<FiniteField<{primes::U32_SMALL}>> = create_semantic_hash_map(builder1.num_vars());

            let h1 = c1.semantic_hash(&map);
            let h2 = c2.semantic_hash(&map);

            h1 == h2
        }
    }

    quickcheck! {
        fn prob_equiv_reflexive(c: Cnf, vtree: VTree) -> bool {
            let builder = SemanticSddBuilder::<{primes::U32_SMALL}>::new(vtree);
            let c = builder.compile_cnf(&c);

            builder.eq(c, c)
        }
    }

    quickcheck! {
        fn prob_equiv_sdd_identity_uncompressed_depr(c: Cnf, vtree:VTree) -> TestResult {
            let compr_builder = super::CompressionSddBuilder::new(vtree.clone());
            let compr_cnf = compr_builder.compile_cnf(&c);

            let mut uncompr_builder = super::CompressionSddBuilder::new(vtree);
            uncompr_builder.set_compression(false);
            let uncompr_cnf = uncompr_builder.compile_cnf(&c);

            let map : WmcParams<FiniteField<{primes::U32_SMALL}>> = create_semantic_hash_map(compr_builder.num_vars());

            let compr_h = compr_cnf.semantic_hash(&map);
            let uncompr_h = uncompr_cnf.semantic_hash(&map);

            if compr_h != uncompr_h {
                println!("not equal! hashes: compr: {compr_h}, uncompr: {uncompr_h}");
                println!("map: {:?}", map);
                println!("compr sdd: {}", compr_builder.print_sdd(compr_cnf));
                println!("uncompr sdd: {}", uncompr_builder.print_sdd(uncompr_cnf));
                TestResult::from_bool(false)
            } else {
                TestResult::from_bool(true)
            }
        }
    }

    quickcheck! {
        fn prob_equiv_sdd_identity_uncompressed(c: Cnf, vtree:VTree) -> TestResult {
            let compr_builder = CompressionSddBuilder::new(vtree.clone());
            let compr_cnf = compr_builder.compile_cnf(&c);

            let uncompr_builder = SemanticSddBuilder::<{primes::U32_SMALL}>::new(vtree);
            let uncompr_cnf = uncompr_builder.compile_cnf(&c);

            if !uncompr_builder.eq(compr_cnf, uncompr_cnf) {
                println!("not equal!");
                println!("compr sdd: {}", compr_builder.print_sdd(compr_cnf));
                println!("uncompr sdd: {}", uncompr_builder.print_sdd(uncompr_cnf));
                TestResult::from_bool(false)
            } else {
                TestResult::from_bool(true)
            }
        }
    }

    quickcheck! {
        fn prob_equiv_sdd_inequality(c1: Cnf, c2: Cnf, vtree:VTree) -> TestResult {
            let mut builder = SemanticSddBuilder::<{primes::U64_LARGEST}>::new(vtree);
            builder.set_compression(true); // necessary to make sure we don't generate two uncompressed SDDs that canonicalize to the same SDD
            let cnf_1 = builder.compile_cnf(&c1);
            let cnf_2 = builder.compile_cnf(&c2);

            if cnf_1 == cnf_2 {
                return TestResult::discard();
            }

            if builder.eq(cnf_1, cnf_2) {
                println!("collision!");
                println!("sdd 1: {}", builder.print_sdd(cnf_1));
                println!("sdd 2: {}", builder.print_sdd(cnf_2));
                TestResult::from_bool(false)
            } else {
                TestResult::from_bool(true)
            }
        }
    }

    quickcheck! {
        fn prob_equiv_sdd_eq_vs_prob_eq(c1: Cnf, c2: Cnf, vtree:VTree) -> TestResult {
            let mut builder = SemanticSddBuilder::<{primes::U64_LARGEST}>::new(vtree);
            builder.set_compression(true); // necessary to make sure we don't generate two uncompressed SDDs that canonicalize to the same SDD
            let cnf_1 = builder.compile_cnf(&c1);
            let cnf_2 = builder.compile_cnf(&c2);

            let h_eq = builder.eq(cnf_1, cnf_2);

            if h_eq != (cnf_1 == cnf_2) {
                println!("disagreement!");
                println!("ptr eq: {}, builder_eq: {}", cnf_1 == cnf_2, h_eq);
                println!("sdd 1: {}", builder.print_sdd(cnf_1));
                println!("sdd 2: {}", builder.print_sdd(cnf_2));
                TestResult::from_bool(false)
            } else {
                TestResult::from_bool(true)
            }
        }
    }

    quickcheck! {
        /// verify that every node in the SDD compression canonicalizer has a unique semantic hash, using CompressionCanonicalizer
        fn qc_sdd_canonicity(c1: Cnf, vtree:VTree) -> TestResult {
            let builder = super::CompressionSddBuilder::new(vtree);
            builder.compile_cnf(&c1);

            let map : WmcParams<FiniteField<{primes::U32_SMALL}>>= create_semantic_hash_map(builder.num_vars());
            let mut seen_hashes : HashMap<u128, SddPtr> = HashMap::new();
            for sdd in builder.node_iter() {
                let hash = sdd.semantic_hash(&map);
                if seen_hashes.contains_key(&hash.value()) {
                    let c = seen_hashes.get(&hash.value()).unwrap();
                    println!("cnf: {}", c1);
                    println!("probmap: {:?}", map);
                    println!("collision found for hash value {}", hash);
                    println!("sdd a: {}\n", builder.print_sdd(sdd));
                    println!("sdd b: {}\n", builder.print_sdd(*c));
                    return TestResult::from_bool(false);
                }
                seen_hashes.insert(hash.value(), sdd);
            }
            TestResult::from_bool(true)
        }
    }

    quickcheck! {
        /// verify that every node in the SDD with the semantic canonicalizer a unique semantic hash w.r.t negations
        /// using SemanticCanonicalizer
        fn qc_semantic_sdd_canonicity(c1: Cnf, vtree:VTree) -> TestResult {
            let builder = SemanticSddBuilder::<{primes::U32_SMALL}>::new(vtree);
            builder.compile_cnf(&c1);

            let map : WmcParams<FiniteField<{primes::U32_SMALL}>>= create_semantic_hash_map(builder.num_vars());
            let mut seen_hashes : HashMap<u128, SddPtr> = HashMap::new();
            for sdd in builder.node_iter() {
                let hash = sdd.semantic_hash(&map);

                // see the hash itself
                if seen_hashes.contains_key(&hash.value()) {
                    let c = seen_hashes.get(&hash.value()).unwrap();
                    println!("cnf: {}", c1);
                    println!("probmap: {:?}", map);
                    println!("collision found for hash value {}", hash);
                    println!("sdd a: {}\n", builder.print_sdd(sdd));
                    println!("sdd b: {}\n", builder.print_sdd(*c));
                    return TestResult::from_bool(false);
                }

                // see the hash's negation
                if seen_hashes.contains_key(&hash.negate().value()) {
                    let c = seen_hashes.get(&hash.negate().value()).unwrap();
                    println!("cnf: {}", c1);
                    println!("probmap: {:?}", map);
                    println!("collision found for negated hash value {}", hash.negate());
                    println!("sdd a: {}\n", builder.print_sdd(sdd));
                    println!("sdd b: {}\n", builder.print_sdd(*c));
                    return TestResult::from_bool(false);
                }
                seen_hashes.insert(hash.value(), sdd);
            }
            TestResult::from_bool(true)
        }
    }

    quickcheck! {
        fn vtree_validity_arbitrary(vtree: VTree) -> bool {
            VTree::is_valid_vtree(&vtree)
        }
    }

    quickcheck! {
        fn vtree_validity_from_dtree(cnf: Cnf) -> bool {
            let dtree = rsdd::repr::DTree::from_cnf(&cnf, &cnf.min_fill_order());
            let vtree = VTree::from_dtree(&dtree).unwrap();
            VTree::is_valid_vtree(&vtree)
        }
    }

    quickcheck! {
        /// verify that the semantic hash of an SDDPtr + its compl is always equal to 1
        fn semantic_reg_plus_compl_eq_one(c1: Cnf, vtree:VTree) -> bool {
            let builder = SemanticSddBuilder::<{primes::U32_SMALL}>::new(vtree);
            let map : WmcParams<FiniteField<{primes::U32_SMALL}>>= create_semantic_hash_map(builder.num_vars());

            let sdd = builder.compile_cnf(&c1);
            let compl = sdd.neg();

            let sdd_hash = sdd.semantic_hash(&map);
            let compl_hash = compl.semantic_hash(&map);

            let sum = (sdd_hash + compl_hash).value();

            if sum != 1 {
                println!("hashes do not sum to one; Reg: {}, Compl: {}", sdd_hash, compl_hash);
            }

            sum == 1
        }
    }
}

#[cfg(test)]
mod test_dnnf_builder {
    use std::collections::HashMap;

    use quickcheck::{Arbitrary, Gen, TestResult};
    use rand::{rngs::SmallRng, seq::SliceRandom, SeedableRng};
    use rsdd::{
        builder::decision_nnf::{
            DecisionNNFBuilder, SemanticDecisionNNFBuilder, StandardDecisionNNFBuilder,
        },
        constants::primes,
        repr::{Cnf, DDNNFPtr, VarLabel, VarOrder, WmcParams},
        util::semirings::RealSemiring,
    };

    #[derive(Clone, Debug)]
    struct CnfAndOrdering {
        pub cnf: Cnf,
        pub order: VarOrder,
    }

    impl Arbitrary for CnfAndOrdering {
        fn arbitrary(g: &mut Gen) -> Self {
            let cnf = Cnf::arbitrary(g);

            let mut order: Vec<VarLabel> = (0..cnf.num_vars() as u64).map(VarLabel::new).collect();

            let mut rng = SmallRng::seed_from_u64(u64::arbitrary(g));
            order.shuffle(&mut rng);

            CnfAndOrdering {
                cnf,
                order: VarOrder::new(&order),
            }
        }
    }

    quickcheck! {
        fn semantic_no_redundant_nodes(cnf: Cnf) -> TestResult {
            // constrain the size
            if cnf.num_vars() == 0 || cnf.num_vars() > 8 { return TestResult::discard() }
            if cnf.clauses().len() > 16 { return TestResult::discard() }

            let linear_order = VarOrder::linear_order(cnf.num_vars());

            let sem_builder = SemanticDecisionNNFBuilder::<{primes::U32_SMALL}>::new(linear_order);
            sem_builder.compile_cnf_topdown(&cnf);

            let num_redundant = sem_builder.num_logically_redundant();

            if num_redundant > 0 {
                println!("Error on input {}, found {} redundant nodes", cnf, num_redundant);
            }

            TestResult::from_bool(num_redundant == 0)
        }
    }

    quickcheck! {
        fn semantic_and_standard_agree_on_hash_linear_order(cnf: Cnf) -> TestResult {
            // constrain the size
            if cnf.num_vars() == 0 || cnf.num_vars() > 8 { return TestResult::discard() }
            if cnf.clauses().len() > 16 { return TestResult::discard() }

            let linear_order = VarOrder::linear_order(cnf.num_vars());

            let std_builder = StandardDecisionNNFBuilder::new(linear_order.clone());
            let std_dnnf = std_builder.compile_cnf_topdown(&cnf);

            let sem_builder = SemanticDecisionNNFBuilder::<{primes::U64_LARGEST}>::new(linear_order);
            let sem_dnnf = sem_builder.compile_cnf_topdown(&cnf);


            let weight_map : HashMap<VarLabel, (RealSemiring, RealSemiring)> = HashMap::from_iter(
                (0..16).map(|x| (VarLabel::new(x as u64), (RealSemiring(0.3), RealSemiring(0.7)))));
            let params = WmcParams::new(weight_map);

            let std_wmc = std_dnnf.unsmoothed_wmc(&params);
            let sem_wmc = sem_dnnf.unsmoothed_wmc(&params);

            let eps = f64::abs(std_wmc.0 - sem_wmc.0) < 0.0001;
            if !eps {
              println!("error on input {}: std wmc: {}, sem wmc: {}\n std bdd: {}\nsem bdd: {}",
                cnf, std_wmc, sem_wmc, std_dnnf.to_string_debug(), sem_dnnf.to_string_debug());
            }
            TestResult::from_bool(eps)
        }
    }

    quickcheck! {
        fn semantic_and_standard_agree_on_hash_arbitrary_order(cnf_and_ordering: CnfAndOrdering) -> TestResult {
            let cnf = cnf_and_ordering.cnf;
            let order = cnf_and_ordering.order;

            let std_builder = StandardDecisionNNFBuilder::new(order.clone());
            let std_dnnf = std_builder.compile_cnf_topdown(&cnf);

            let sem_builder = SemanticDecisionNNFBuilder::<{primes::U64_LARGEST}>::new(order);
            let sem_dnnf = sem_builder.compile_cnf_topdown(&cnf);


            let weight_map : HashMap<VarLabel, (RealSemiring, RealSemiring)> = HashMap::from_iter(
                (0..cnf.num_vars()).map(|x| (VarLabel::new(x as u64), (RealSemiring(0.3), RealSemiring(0.7)))));
            let params = WmcParams::new(weight_map);

            let std_wmc = std_dnnf.unsmoothed_wmc(&params);
            let sem_wmc = sem_dnnf.unsmoothed_wmc(&params);

            let eps = f64::abs(std_wmc.0 - sem_wmc.0) < 0.0001;
            if !eps {
              println!("error on input {}: std wmc: {}, sem wmc: {}\n std bdd: {}\nsem bdd: {}",
                cnf, std_wmc, sem_wmc, std_dnnf.to_string_debug(), sem_dnnf.to_string_debug());
            }
            TestResult::from_bool(eps)
        }
    }
}
