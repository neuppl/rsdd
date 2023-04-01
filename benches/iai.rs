use iai::black_box;
use rsdd::{
    builder::{canonicalize::CompressionCanonicalizer, sdd_builder::SddManager},
    repr::{cnf::Cnf, var_label::VarLabel, vtree::VTree},
};

// subset of DIMACS from CNFs from test.rs
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

fn sdd_benchmark_helper(cnf1: Cnf, cnf2: Cnf) {
    let v: Vec<VarLabel> = (0..cnf1.num_vars())
        .map(|x| VarLabel::new(x as u64))
        .collect();
    let vtree = VTree::even_split(&v, 1);
    let mut man = SddManager::<CompressionCanonicalizer>::new(vtree);
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

fn iai_benchmark_sdd_canonicity_c1() {
    sdd_benchmark_helper(
        Cnf::from_file(String::from(black_box(C1_A))),
        Cnf::from_file(String::from(black_box(C1_B))),
    )
}

fn iai_benchmark_sdd_canonicity_c2() {
    sdd_benchmark_helper(
        Cnf::from_file(String::from(black_box(C2_A))),
        Cnf::from_file(String::from(black_box(C2_B))),
    )
}

fn iai_benchmark_sdd_canonicity_c3() {
    sdd_benchmark_helper(
        Cnf::from_file(String::from(black_box(C3_A))),
        Cnf::from_file(String::from(black_box(C3_B))),
    )
}

iai::main!(
    iai_benchmark_sdd_canonicity_c1,
    iai_benchmark_sdd_canonicity_c2,
    iai_benchmark_sdd_canonicity_c3
);
