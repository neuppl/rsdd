use bdd;
use manager;
use boolexpr;
use var_order;
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;


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

use rand;
fn random_assignment(num_vars: usize) -> HashMap<bdd::VarLabel, bool> {
    let mut init = HashMap::new();
    for i in 0..num_vars {
        init.insert(bdd::VarLabel::new(i as u64), rand::random());
    }
    init
}

#[test]
fn test_canonicity() -> () {
    use boolexpr::BoolExpr::*;
    let c1 = Or(Box::new(Or(Box::new(Var(1, true)), Box::new(Var(2, false)))),
                Box::new(Var(2, false)));
    let c2 = Or(Box::new(Or(Box::new(Var(2, false)), Box::new(Var(2, false)))),
                Box::new(Var(2, false)));
    let c3 = Or(Box::new(Or(Box::new(Var(2, false)), Box::new(Var(1, true)))),
                Box::new(Var(2, false)));
    let bexpr = And(Box::new(And(Box::new(c1), Box::new(c2))), Box::new(c3));
    let mut man = manager::BddManager::new_default_order(3);
    let r1 = bexpr.into_bdd(&mut man);
    println!("second apply");
    let r2 = man.and(r1, r1);
    println!("bdd1: {},\nbdd2:{}\ncnf: {:?}", man.print_bdd(r1), man.print_bdd(r2), bexpr);
    assert!(man.eq_bdd(r1, r2));
}

/// generate 10 random CNFs and test to see that they compile to the same value
use rand::SeedableRng;
#[test]
pub fn rand_bdds() -> () {
    let mut rng = rand::StdRng::new().unwrap();
    rng.reseed(&[0]);
    for _ in 1..20 {
        let num_vars = 20;
        let cnf = boolexpr::rand_cnf(&mut rng, num_vars, 30);
        let mut man = manager::BddManager::new_default_order(num_vars);
        let r = cnf.into_bdd(&mut man);
        println!("eval");
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

// #[test]
pub fn big_bdd() -> () {
    let mut rng = rand::StdRng::new().unwrap();
    rng.reseed(&[0]);
    let num_vars = 50;
    let cnf = boolexpr::rand_cnf(&mut rng, num_vars, 70);
    let mut man = manager::BddManager::new_default_order(num_vars);
    let r = cnf.into_bdd(&mut man);
    // check that they evaluate to the same value for a variety of
    // assignments
    for _ in 1..100 {
        let assgn = random_assignment(num_vars);
        assert_eq!(man.eval_bdd(r, &assgn), cnf.eval(&assgn));
    }
    println!("stats: {:?}", man.get_apply_cache_stats())
}


// #[test]
pub fn bdd_from_file() -> () {
    use cnf::Cnf;
    let num_vars = 228;
    // let file_contents = File::open("/Users/sholtzen/Downloads/sdd-1.1.1/cnf/c8-easier.cnf");
    let file_contents = File::open("/Users/sholtzen/Downloads/sdd-1.1.1/cnf/c8.cnf");
    let mut string = String::new();
    file_contents.unwrap().read_to_string(&mut string).unwrap();
    let cnf = Cnf::from_file(string);
    println!("cnf: {:?}", cnf);
    let mut man = manager::BddManager::new_default_order(num_vars);
    let r = cnf.into_bdd(&mut man);
    let assgn = random_assignment(num_vars);
    // println!("BDD: {}\nExpr: {:?}\nAssignment: {:?}", man.print_bdd(), cnf, assgn);
    // assert_eq!(man.eval_bdd(r, &assgn), cnf.eval(&assgn));
    println!("apply cache stats: {:?}", man.get_apply_cache_stats());
    println!("num apply nodes: {}", man.num_nodes());
    println!("node count: {}", man.count_nodes(r));
    println!("backing store stats: {:?}", man.get_backing_store_stats());
    // println!("bdd: {}", man.print_bdd(r));
}

#[test]
pub fn sdd_from_file() -> () {
    use sdd_manager::*;
    use cnf::Cnf;
    let num_vars = 228;
    // let file_contents = File::open("/Users/sholtzen/Downloads/sdd-1.1.1/cnf/c8-easier.cnf");
    let file_contents = File::open("/Users/sholtzen/Downloads/sdd-1.1.1/cnf/c8.cnf");
    let mut string = String::new();
    file_contents.unwrap().read_to_string(&mut string).unwrap();
    let cnf = Cnf::from_file(string);
    // println!("cnf: {:?}", cnf);
    let v : Vec<bdd::VarLabel> =
        (0..num_vars).map(|x| bdd::VarLabel::new(x as u64)).collect();
    let vtree = even_split(&v, 2);
    let mut man = SddManager::new(vtree);
    let r = cnf.into_sdd(&mut man);
    let assgn = random_assignment(num_vars);
    // println!("BDD: {}\nExpr: {:?}\nAssignment: {:?}", man.print_bdd(), cnf, assgn);
    // assert_eq!(man.eval_bdd(r, &assgn), cnf.eval(&assgn));
    // println!("apply cache stats: {:?}", man.get_apply_cache_stats());
    // println!("num apply nodes: {}", man.num_nodes());
    // println!("node count: {}", man.count_nodes(r));
    // println!("backing store stats: {:?}", man.get_backing_store_stats());
    // println!("bdd: {}", man.print_bdd(r));
}


#[test]
pub fn random_sdd() {
    use sdd::*;
    use sdd_manager::*;
    let mut rng = rand::StdRng::new().unwrap();
    rng.reseed(&[0]);
    for _ in 1..20 {
        println!("compiling\n\n");
        let num_vars = 10;
        let cnf = boolexpr::rand_cnf(&mut rng, num_vars, 20);
        let v : Vec<bdd::VarLabel> =
            (0..num_vars).map(|x| bdd::VarLabel::new(x as u64)).collect();
        let vtree = even_split(&v, 1);
        let mut man = SddManager::new(vtree);
        let r = cnf.into_sdd(&mut man);
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
        let new_r = man.apply(bdd::Op::BddAnd, r, r);
        assert!(man.sdd_eq(r, new_r));
        let new_r = man.apply(bdd::Op::BddOr, r, r);
        assert!(man.sdd_eq(r, new_r));
    }
}

// #[test]
pub fn big_sdd() {
    use sdd::*;
    use sdd_manager::*;
    let mut rng = rand::StdRng::new().unwrap();
    rng.reseed(&[0]);
    let num_vars = 50;
    let cnf = boolexpr::rand_cnf(&mut rng, num_vars, 50);
    let v : Vec<bdd::VarLabel> =
        (0..num_vars).map(|x| bdd::VarLabel::new(x as u64)).collect();
    let vtree = even_split(&v, 1);
    let mut man = SddManager::new(vtree);
    let r = cnf.into_sdd(&mut man);
    // println!("sdd: {}", man.print_sdd(r));
    // check that they evaluate to the same value for a variety of
    // assignments
    // println!("evaluating");
    // for _ in 1..100 {
    //     let assgn = random_assignment(num_vars);
    //     assert_eq!(man.eval_sdd(r, &assgn), cnf.eval(&assgn));
    // }
}
