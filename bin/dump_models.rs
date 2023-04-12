extern crate rsdd;

use clap::Parser;
use rsdd::builder::bdd_builder::BddManager;
use rsdd::builder::cache::lru_app::BddApplyTable;
use rsdd::repr::bdd::BddPtr;
use rsdd::serialize::{ser_bdd, ser_sdd, ser_vtree};
use rsdd::{
    builder::{canonicalize::CompressionCanonicalizer, sdd_builder::SddManager},
    repr::{cnf::Cnf, var_label::VarLabel, vtree::VTree},
};
use std::fs::{self, File};
use std::io::Write;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// An input CNF
    #[clap(short, long, value_parser)]
    file: String,
}

fn dump_bdd(c: &Cnf) -> String {
    let mut man = BddManager::<BddApplyTable<BddPtr>>::new_default_order_lru(c.num_vars());
    let bdd = man.from_cnf(c);

    let json = ser_bdd::BDDSerializer::from_bdd(bdd);

    serde_json::to_string(&json).unwrap()
}

fn dump_sdd(c: &Cnf, vtree: VTree) -> String {
    let mut compr_mgr = SddManager::<CompressionCanonicalizer>::new(vtree);
    let sdd = compr_mgr.from_cnf(c);

    let json = ser_sdd::SDDSerializer::from_sdd(sdd);

    serde_json::to_string(&json).unwrap()
}

fn dump_vtree(vtree: &VTree) -> String {
    let json = ser_vtree::VTreeSerializer::from_vtree(vtree);
    serde_json::to_string(&json).unwrap()
}

fn write(str: String, path: String) {
    let mut file = File::create(path).unwrap();
    let r = file.write_all(str.as_bytes());
    assert!(r.is_ok(), "Error writing file");
}

fn main() {
    let args = Args::parse();

    let cnf_input = fs::read_to_string(args.file).expect("Should have been able to read the file");

    let cnf = Cnf::from_file(cnf_input);

    let range: Vec<usize> = (0..cnf.num_vars()).collect();
    let binding = range
        .iter()
        .map(|i| VarLabel::new(*i as u64))
        .collect::<Vec<VarLabel>>();
    let vars = binding.as_slice();

    let vtree = VTree::right_linear(vars);

    write(dump_vtree(&vtree), "vtree.json".to_string());
    write(dump_bdd(&cnf), "bdd.json".to_string());
    write(dump_sdd(&cnf, vtree), "sdd.json".to_string());
}
