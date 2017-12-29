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
        // check that they evaluate to the same value for a variety of
        // assignments
        for _ in 1..100 {
            let assgn = random_assignment(num_vars);
            assert_eq!(man.eval_bdd(r, &assgn), cnf.eval(&assgn));
        }
        // println!("avg offset: {}", man.apply_table.a)
    }
}

#[test]
pub fn big_bdd() -> () {
    let mut rng = rand::StdRng::new().unwrap();
    rng.reseed(&[0]);
    let num_vars = 30;
    let cnf = boolexpr::rand_cnf(&mut rng, num_vars, 60);
    let mut man = manager::BddManager::new_default_order(num_vars);
    let r = cnf.into_bdd(&mut man);
    // check that they evaluate to the same value for a variety of
    // assignments
    for _ in 1..100 {
        let assgn = random_assignment(num_vars);
        assert_eq!(man.eval_bdd(r, &assgn), cnf.eval(&assgn));
    }
}


// #[test]
pub fn from_file() -> () {
    let num_vars = 228;
    // let file_contents = File::open("/Users/sholtzen/Downloads/sdd-1.1.1/cnf/c8-easier.cnf");
    let file_contents = File::open("/Users/sholtzen/Downloads/sdd-1.1.1/cnf/c8-easier.cnf");
    let mut string = String::new();
    file_contents.unwrap().read_to_string(&mut string).unwrap();
    let cnf = boolexpr::parse_cnf(string);
    let mut man = manager::BddManager::new_default_order(num_vars);
    let r = cnf.into_bdd(&mut man);
    let assgn = random_assignment(num_vars);
    // println!("BDD: {}\nExpr: {:?}\nAssignment: {:?}", man.print_bdd(), cnf, assgn);
    assert_eq!(man.eval_bdd(r, &assgn), cnf.eval(&assgn));
}


#[test]
pub fn random_sdd() {
    use sdd::*;
    use sdd_manager::*;
    let mut rng = rand::StdRng::new().unwrap();
    rng.reseed(&[0]);
    for _ in 1..20 {
        println!("compiling");
        let num_vars = 10;
        let cnf = boolexpr::rand_cnf(&mut rng, num_vars, 5);
        let v : Vec<bdd::VarLabel> =
            (0..num_vars).map(|x| bdd::VarLabel::new(x as u64)).collect();
        let vtree = even_split(&v, 2);
        let mut man = SddManager::new(vtree);
        let r = cnf.into_sdd(&mut man);
        // check that they evaluate to the same value for a variety of
        // assignments
        println!("expr: {:?}\nsdd: {}", cnf, man.print_sdd(r));
        println!("evaluating");
        for _ in 1..100 {
            let assgn = random_assignment(num_vars);
            assert_eq!(man.eval_sdd(r, &assgn), cnf.eval(&assgn));
        }
    }
}

#[test]
pub fn big_sdd() {
    use sdd::*;
    use sdd_manager::*;
    let mut rng = rand::StdRng::new().unwrap();
    rng.reseed(&[0]);
    let num_vars = 50;
    let cnf = boolexpr::rand_cnf(&mut rng, num_vars, 50);
    let v : Vec<bdd::VarLabel> =
        (0..num_vars).map(|x| bdd::VarLabel::new(x as u64)).collect();
    let vtree = even_split(&v, 3);
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
