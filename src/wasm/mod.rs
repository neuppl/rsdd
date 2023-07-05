use crate::builder::bdd_builder::{BddManager, BddPtr, DDNNFPtr};
use crate::builder::cache::lru_app::BddApplyTable;
use crate::builder::sdd::builder::SddBuilder;
use crate::builder::sdd::compression::CompressionSddBuilder;
use crate::repr::dtree::DTree;
use crate::repr::robdd::VarOrder;
use crate::repr::wmc::WmcParams;
use crate::repr::{cnf::Cnf, var_label::VarLabel, vtree::VTree};
use crate::serialize::ser_sdd::SDDSerializer;
use crate::serialize::ser_vtree::VTreeSerializer;
use crate::serialize::{ser_bdd, ser_sdd, ser_vtree};
use crate::util::semirings::realsemiring::RealSemiring;
use crate::util::semirings::semiring_traits::Semiring;
use wasm_bindgen::prelude::*;

#[derive(Serialize, Deserialize)]
pub enum VTreeType {
    LeftLinear,
    RightLinear,
    EvenSplit(usize),
    FromDTreeLinear,
    FromDTreeMinFill,
}

#[derive(Serialize, Deserialize)]
pub struct SddModelCountResult {
    model_count: f64,
    sdd: SDDSerializer,
    vtree: VTreeSerializer,
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

    let man = BddManager::<BddApplyTable<BddPtr>>::new_default_order_lru(cnf.num_vars());
    let bdd = man.from_cnf(&cnf);

    let json = ser_bdd::BDDSerializer::from_bdd(bdd);

    serde_json::to_string(&json).unwrap()
}

// used in: https://github.com/mattxwang/indecision
#[wasm_bindgen]
pub fn bdd_with_var_order(cnf_input: String, order: &[u64]) -> String {
    let cnf = Cnf::from_file(cnf_input);

    let var_order = VarOrder::new(order.iter().map(|v| VarLabel::new(*v)).collect());

    let man = BddManager::new(var_order, BddApplyTable::new(21));
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

    let man = CompressionSddBuilder::new(vtree);
    let sdd = man.from_cnf(&cnf);

    let serialized = ser_sdd::SDDSerializer::from_sdd(sdd);

    Ok(serde_wasm_bindgen::to_value(&serialized)?)
}

// used in rsdd-docs
#[wasm_bindgen]
pub fn demo_model_count_sdd(cnf_input: String) -> Result<JsValue, JsValue> {
    let cnf = Cnf::from_file(cnf_input);

    let vtree = build_vtree(&cnf, VTreeType::FromDTreeLinear);

    let man = CompressionSddBuilder::new(vtree.clone());
    let sdd = man.from_cnf(&cnf);

    let mut params = WmcParams::new(RealSemiring::zero(), RealSemiring::one());

    for v in 0..man.get_vtree_manager().num_vars() + 1 {
        params.set_weight(
            VarLabel::new_usize(v),
            RealSemiring::zero(),
            RealSemiring::one(),
        )
    }

    let model_count = sdd.wmc(man.get_vtree_manager(), &params);

    let res = SddModelCountResult {
        model_count: model_count.0,
        sdd: ser_sdd::SDDSerializer::from_sdd(sdd),
        vtree: ser_vtree::VTreeSerializer::from_vtree(&vtree),
    };

    Ok(serde_wasm_bindgen::to_value(&res)?)
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
            let dtree = DTree::from_cnf(cnf, &VarOrder::linear_order(cnf.num_vars()));
            VTree::from_dtree(&dtree).unwrap()
        }
        VTreeType::FromDTreeMinFill => {
            let dtree = DTree::from_cnf(cnf, &cnf.min_fill_order());
            VTree::from_dtree(&dtree).unwrap()
        }
    }
}
