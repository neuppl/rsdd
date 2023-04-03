use crate::builder::bdd_builder::{BddManager, BddPtr};
use crate::builder::cache::lru_app::BddApplyTable;
use crate::repr::{cnf::Cnf, var_label::VarLabel, vtree::VTree};
use crate::serialize::{ser_bdd, ser_vtree};
use wasm_bindgen::prelude::*;

// used in: https://github.com/mattxwang/indecision
#[wasm_bindgen]
pub fn get_vtree(cnf_input: String) -> String {
    let cnf = Cnf::from_file(cnf_input);

    let range: Vec<usize> = (0..cnf.num_vars() + 1).collect();
    let binding = range
        .iter()
        .map(|i| VarLabel::new(*i as u64))
        .collect::<Vec<VarLabel>>();
    let vars = binding.as_slice();

    let vtree = VTree::right_linear(vars);

    let json = ser_vtree::VTreeSerializer::from_vtree(&vtree);
    serde_json::to_string(&json).unwrap()
}

// used in: https://github.com/mattxwang/indecision
#[wasm_bindgen]
pub fn get_bdd(cnf_input: String) -> String {
    let cnf = Cnf::from_file(cnf_input);

    let mut man = BddManager::<BddApplyTable<BddPtr>>::new_default_order_lru(cnf.num_vars());
    let bdd = man.from_cnf(&cnf);

    let json = ser_bdd::BDDSerializer::from_bdd(bdd);

    serde_json::to_string(&json).unwrap()
}
