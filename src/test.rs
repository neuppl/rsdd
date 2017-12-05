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
    let num_vars = 60;
    let cnf = boolexpr::rand_cnf(&mut rng, num_vars, 40);
    let mut man = manager::BddManager::new_default_order(num_vars);
    cnf.into_bdd(&mut man);
    let assgn = random_assignment(num_vars);
    // println!("BDD: {}\nExpr: {:?}\nAssignment: {:?}", man.print_bdd(), cnf, assgn);
    assert_eq!(man.eval_bdd(&assgn), cnf.eval(&assgn));
}

#[test]
pub fn from_file() -> () {
    let num_vars = 228;
    let file_contents = File::open("/Users/sholtzen/Downloads/sdd-1.1.1/cnf/c8.cnf");
    let mut string = String::new();
    file_contents.unwrap().read_to_string(&mut string).unwrap();
    let cnf = boolexpr::parse_cnf(string);
    let mut man = manager::BddManager::new_default_order(num_vars);
    cnf.into_bdd(&mut man);
    let assgn = random_assignment(num_vars);
    // println!("BDD: {}\nExpr: {:?}\nAssignment: {:?}", man.print_bdd(), cnf, assgn);
    assert_eq!(man.eval_bdd(&assgn), cnf.eval(&assgn));
}
