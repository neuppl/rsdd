use rsdd::manager::bdd_manager::BddManager;
use rsdd::manager::sdd_manager::SddManager;
use rsdd::repr::cnf::Cnf;
use rand::SeedableRng;

// fn rand_small_bdds_no_heuristic() -> () {
//     let mut rng = rand::StdRng::new().unwrap();
//     rng.reseed(&[0]);
//     bench.iter(|| {
//         let num_vars = 20;
//         let cnf = Cnf::rand_cnf(&mut rng, num_vars, 30);
//         let mut man = BddManager::new_default_order(num_vars);
//         let r = man.from_cnf(&cnf);
//     })
// }

// fn rand_med_bdds_no_heuristic() -> () {
//     let mut rng = rand::StdRng::new().unwrap();
//     rng.reseed(&[0]);
//     bench.iter(|| {
//         let num_vars = 20;
//         let cnf = Cnf::rand_cnf(&mut rng, num_vars, 50);
//         let mut man = BddManager::new_default_order(num_vars);
//         let r = man.from_cnf(&cnf);
//     })
// }

