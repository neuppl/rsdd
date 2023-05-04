use crate::builder::bdd_builder::{BddManager, BddPtr};
use crate::builder::cache::lru_app::BddApplyTable;
use crate::builder::canonicalize::{CompressionCanonicalizer, SemanticCanonicalizer};
use crate::builder::sdd_builder::SddManager;
use crate::repr::bdd::{create_semantic_hash_map_with_linear_order_and_weights, VarOrder};
use crate::repr::dtree::DTree;
use crate::repr::vtree::VTreeManager;
use crate::repr::{cnf::Cnf, var_label::VarLabel, vtree::VTree};
use crate::serialize::{ser_bdd, ser_sdd, ser_vtree};
use wasm_bindgen::prelude::*;

#[derive(Serialize, Deserialize)]
pub enum VTreeType {
    LeftLinear,
    RightLinear,
    EvenSplit(usize),
    FromDTreeLinear,
    FromDTreeMinFill,
}

const MEDIUM_PRIME: u128 = 100000049;

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
pub fn bdd_with_var_order(cnf_input: String, order: &[u64]) -> String {
    let cnf = Cnf::from_file(cnf_input);

    let var_order = VarOrder::new(order.iter().map(|v| VarLabel::new(*v)).collect());

    let mut man = BddManager::new(var_order, BddApplyTable::new(21));
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

// used in: https://github.com/mattxwang/indecision
#[wasm_bindgen]
pub fn sdd_semantic_with_map_linear(
    cnf_input: String,
    vtree_type_input: JsValue,
) -> Result<JsValue, JsValue> {
    let cnf = Cnf::from_file(cnf_input);

    let vtree_type: VTreeType = serde_wasm_bindgen::from_value(vtree_type_input)?;

    let vtree = build_vtree(&cnf, vtree_type);

    let map = create_semantic_hash_map_with_linear_order_and_weights(cnf.num_vars());

    let vtree_man = VTreeManager::new(vtree.clone());

    let canonicalizer = SemanticCanonicalizer::new_with_map(&vtree_man, map.clone());

    let mut mgr = SddManager::<SemanticCanonicalizer<MEDIUM_PRIME>>::new_with_canonicalizer(
        vtree,
        canonicalizer,
    );
    let sdd = mgr.from_cnf(&cnf);

    // forces all semantic hashes to be calculated
    let _ = sdd.cached_semantic_hash(&vtree_man, &map);

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
        VTreeType::FromDTreeLinear => {
            let dtree = DTree::from_cnf(&cnf, &VarOrder::linear_order(cnf.num_vars()));
            VTree::from_dtree(&dtree).unwrap()
        }
        VTreeType::FromDTreeMinFill => {
            let dtree = DTree::from_cnf(&cnf, &cnf.min_fill_order());
            VTree::from_dtree(&dtree).unwrap()
        }
    }
}
