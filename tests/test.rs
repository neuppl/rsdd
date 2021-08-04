extern crate rsdd;
use rsdd::*;
use manager::rsbdd_manager::BddManager;
use repr::boolexpr::BoolExpr;
use manager::sdd_manager::{SddManager, even_split};
use manager::var_order::VarOrder;
use repr::var_label::VarLabel;
use std::collections::HashMap;
use repr::cnf::Cnf;
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
    vec!(
        (Cnf::from_file(String::from(C1_A)), Cnf::from_file(String::from(C1_B))),
        (Cnf::from_file(String::from(C2_A)), Cnf::from_file(String::from(C2_B))),
        (Cnf::from_file(String::from(C3_A)), Cnf::from_file(String::from(C3_B))),
        (Cnf::from_file(String::from(C4_A)), Cnf::from_file(String::from(C4_B))),
        (Cnf::from_file(String::from(C5_A)), Cnf::from_file(String::from(C5_B))),
        (Cnf::from_file(String::from(C6_A)), Cnf::from_file(String::from(C6_B))),
        (Cnf::from_file(String::from(C7_A)), Cnf::from_file(String::from(C7_B))),
        (Cnf::from_file(String::from(C8_A)), Cnf::from_file(String::from(C8_B))),
        (Cnf::from_file(String::from(C9_A)), Cnf::from_file(String::from(C9_B))),
        (Cnf::from_file(String::from(C10_A)), Cnf::from_file(String::from(C10_B))),
        (Cnf::from_file(String::from(C11_A)), Cnf::from_file(String::from(C11_B))),
        (Cnf::from_file(String::from(C12_A)), Cnf::from_file(String::from(C12_B))),
        (Cnf::from_file(String::from(C13_A)), Cnf::from_file(String::from(C13_B))),
        (Cnf::from_file(String::from(C14_A)), Cnf::from_file(String::from(C14_B))),
    )
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
        assert!(man.eq_bdd(r1, r2), "Not eq\nCNF 1: {:?}\nCNF 2: {:?}\nBDD 1:{}\n BDD 2: {}",
                cnf1, cnf2,
                man.print_bdd(r1), man.print_bdd(r2)
        );
    }
}

#[test]
fn test_sdd_canonicity() -> () {
    for (cnf1, cnf2) in get_canonical_forms().into_iter() {
        let v : Vec<VarLabel> =
            (0..cnf1.num_vars()).map(|x| VarLabel::new(x as u64)).collect();
        let vtree = even_split(&v, 3);
        let mut man = SddManager::new(vtree);
        let r1 = man.from_cnf(&cnf1);
        let r2 = man.from_cnf(&cnf2);
        assert!(man.sdd_eq(r1, r2), "Not eq\nCNF 1: {:?}\nCNF 2: {:?}\nSDD 1:{}\n SDD 2: {}",
                cnf1, cnf2,
                man.print_sdd(r1), man.print_sdd(r2)
        );
    }
}

/// generate 10 random CNFs and test to see that they compile to the same value
#[test]
pub fn rand_bdds() -> () {
    let mut rng = rand::StdRng::new().unwrap();
    rng.reseed(&[0]);
    for _ in 1..20 {
        let num_vars = 20;
        let cnf = BoolExpr::rand_cnf(&mut rng, num_vars, 30);
        let mut man = BddManager::new_default_order(num_vars);
        let r = man.from_boolexpr(&cnf);
        // check that they evaluate to the same value for a variety of
        // assignments
        // println!("bdd: {},\n cnf: {:?}", man.print_bdd(r), cnf);
        for _ in 1..100 {
            let assgn = random_assignment(num_vars);
            assert_eq!(man.eval_bdd(r, &assgn), cnf.eval(&assgn),
                       "Not eq:\n CNF: {:?}\nBDD:{}", cnf, man.print_bdd(r));
        }
        // check for canonicity: conjoin it with itself and make sure that it is
        // still the same
        let r2 = man.and(r, r);
        assert!(man.eq_bdd(r2, r), "Not canonical: \nbdd1: {}\nbdd2: {}",
                man.print_bdd(r), man.print_bdd(r2));
        let r3 = man.or(r, r);
        assert!(man.eq_bdd(r3, r), "Not canonical: \nbdd1: {}\nbdd2: {}",
                man.print_bdd(r), man.print_bdd(r3));
    }
}

#[test]
pub fn big_sdd() -> () {
    // let file_contents = File::open("/Users/sholtzen/Downloads/sdd-1.1.1/cnf/c8-easier.cnf");
    let file_contents = include_str!("../cnf/c8-easier.cnf");
    let cnf = Cnf::from_file(String::from(file_contents));
    let v : Vec<usize> = cnf.force_order().get_vec();
    let var_vec : Vec<VarLabel> =
        v.into_iter().map(|v| VarLabel::new(v as u64)).collect();
    let vtree = even_split(&var_vec, 3);
    let mut man = SddManager::new(vtree);
    man.from_cnf(&cnf);
}

// #[test]
pub fn big_bdd() -> () {
    // let file_contents = File::open("/Users/sholtzen/Downloads/sdd-1.1.1/cnf/c8-easier.cnf");
    let file_contents = include_str!("../cnf/c8-easier.cnf");
    let cnf = Cnf::from_file(String::from(file_contents));
    let v : Vec<VarLabel> = cnf.force_order()
        .get_vec()
        .into_iter()
        .map(|x| VarLabel::new(x as u64))
        .collect();
    let mut man =
        BddManager::new(VarOrder::new(v));
    man.from_cnf(&cnf);
    // assert!(man.is_false(r), "Expected unsat");
}

/// Randomized tests
#[test]
pub fn random_sdd() {
    let mut rng = rand::StdRng::new().unwrap();
    rng.reseed(&[0]);
    for _ in 1..20 {
        let num_vars = 10;
        let cnf = BoolExpr::rand_cnf(&mut rng, num_vars, 20);
        let v : Vec<VarLabel> =
            (0..num_vars).map(|x| VarLabel::new(x as u64)).collect();
        let vtree = even_split(&v, 1);
        let mut man = SddManager::new(vtree);
        let r = man.from_boolexpr(&cnf);
        // check that they evaluate to the same value for a variety of
        // assignments
        // println!("expr: {:?}\nsdd: {}", cnf, man.print_sdd(r));
        for _ in 1..30 {
            let assgn = random_assignment(num_vars);
            assert_eq!(man.eval_sdd(r, &assgn), cnf.eval(&assgn),
                       "Not equal: {:?}\n{}", cnf, man.print_sdd(r)
            );
        }
        // check canonicity
        let new_r = man.and(r, r);
        assert!(man.sdd_eq(r, new_r));
        let new_r = man.or(r, r);
        assert!(man.sdd_eq(r, new_r));
    }
}
