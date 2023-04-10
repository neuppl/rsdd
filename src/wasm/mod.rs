use crate::builder::bdd_builder::{BddManager, BddPtr};
use crate::builder::cache::lru_app::BddApplyTable;
use crate::builder::canonicalize::CompressionCanonicalizer;
use crate::builder::sdd_builder::SddManager;
use crate::repr::bdd::VarOrder;
use crate::repr::dtree::DTree;
use crate::repr::{cnf::Cnf, var_label::VarLabel, vtree::VTree};
use crate::serialize::{ser_bdd, ser_sdd, ser_vtree};
use wasm_bindgen::prelude::*;

#[derive(Serialize, Deserialize)]
pub enum VTreeType {
    LeftLinear,
    RightLinear,
    EvenSplit(usize),
    FromDTree,
}

// used in: https://github.com/mattxwang/indecision
#[wasm_bindgen]
pub fn vtree(cnf_input: String, vtree_type_input: JsValue) -> Result<JsValue, JsValue> {
    let cnf = Cnf::from_file(cnf_input);

    let vtree_type: VTreeType = serde_wasm_bindgen::from_value(vtree_type_input)?;

    let vtree = build_vtree(&cnf, vtree_type);

    let serialized = ser_vtree::VTreeSerializer::from_vtree(&vtree);

    Ok(serde_wasm_bindgen::to_value(&serialized)?)
}

// used in: https://github.com/mattxwang/indecision
#[wasm_bindgen]
pub fn bdd(cnf_input: String) -> String {
    let cnf = Cnf::from_file(cnf_input);

    let mut man = BddManager::<BddApplyTable<BddPtr>>::new_default_order_lru(cnf.num_vars());
    let bdd = man.from_cnf(&cnf);

    let json = ser_bdd::BDDSerializer::from_bdd(bdd);

    serde_json::to_string(&json).unwrap()
}

// used in: https://github.com/mattxwang/indecision
#[wasm_bindgen]
pub fn sdd(cnf_input: String, vtree_type_input: JsValue) -> Result<JsValue, JsValue> {
    let cnf = Cnf::from_file(cnf_input);

    let vtree_type: VTreeType = serde_wasm_bindgen::from_value(vtree_type_input)?;

    let vtree = build_vtree(&cnf, vtree_type);

    let mut compr_mgr = SddManager::<CompressionCanonicalizer>::new(vtree);
    let sdd = compr_mgr.from_cnf(&cnf);

    let serialized = ser_sdd::SDDSerializer::from_sdd(sdd);

    Ok(serde_wasm_bindgen::to_value(&serialized)?)
}

// internal function -- no intermediate types needed
fn build_vtree(cnf: &Cnf, vtree_type: VTreeType) -> VTree {
    let range: Vec<usize> = (0..cnf.num_vars()).collect();
    let binding = range
        .iter()
        .map(|i| VarLabel::new(*i as u64))
        .collect::<Vec<VarLabel>>();
    let vars = binding.as_slice();

    match vtree_type {
        VTreeType::LeftLinear => VTree::left_linear(vars),
        VTreeType::RightLinear => VTree::right_linear(vars),
        VTreeType::EvenSplit(num) => VTree::even_split(vars, num),
        VTreeType::FromDTree => {
            let dtree = DTree::from_cnf(&cnf, &VarOrder::linear_order(cnf.num_vars()));
            VTree::from_dtree(&dtree).unwrap()
        }
    }
}
