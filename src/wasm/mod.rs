use crate::{
    builder::{
        bdd::{BddBuilder, RobddBuilder},
        cache::{all_app::AllTable, lru_app::BddApplyTable},
        sdd::{CompressionSddBuilder, SddBuilder},
    },
    constants::primes,
    repr::{
        bdd::BddPtr, cnf::Cnf, ddnnf::DDNNFPtr, dtree::DTree, var_label::VarLabel,
        var_order::VarOrder, vtree::VTree, wmc::WmcParams,
    },
    serialize::{BDDSerializer, SDDSerializer, VTreeSerializer},
    util::semirings::FiniteField,
};
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
    model_count: u128,
    sdd: SDDSerializer,
    vtree: VTreeSerializer,
}

// used in: https://github.com/mattxwang/indecision
#[wasm_bindgen]
pub fn vtree(cnf_input: String, vtree_type_input: JsValue) -> Result<JsValue, JsValue> {
    let cnf = Cnf::from_dimacs(&cnf_input);

    let vtree_type: VTreeType = serde_wasm_bindgen::from_value(vtree_type_input)?;

    let vtree = build_vtree(&cnf, vtree_type);

    let serialized = VTreeSerializer::from_vtree(&vtree);

    Ok(serde_wasm_bindgen::to_value(&serialized)?)
}

// used in: https://github.com/mattxwang/indecision
#[wasm_bindgen]
pub fn bdd(cnf_input: String) -> String {
    let cnf = Cnf::from_dimacs(&cnf_input);

    let builder: RobddBuilder<'_, BddApplyTable<BddPtr<'_>>> =
        RobddBuilder::<BddApplyTable<BddPtr>>::new_default_order_lru(cnf.num_vars());
    let bdd = builder.compile_cnf(&cnf);

    let json = BDDSerializer::from_bdd(bdd);

    serde_json::to_string(&json).unwrap()
}

// used in: https://github.com/mattxwang/indecision
#[wasm_bindgen]
pub fn bdd_with_var_order(cnf_input: String, order: &[u64]) -> String {
    let cnf = Cnf::from_dimacs(&cnf_input);

    let var_order = VarOrder::new(order.iter().map(|v| VarLabel::new(*v)).collect());

    let builder = RobddBuilder::<AllTable<BddPtr>>::new(var_order);
    let bdd = builder.compile_cnf(&cnf);

    let json = BDDSerializer::from_bdd(bdd);

    serde_json::to_string(&json).unwrap()
}

// used in: https://github.com/mattxwang/indecision
#[wasm_bindgen]
pub fn sdd(cnf_input: String, vtree_type_input: JsValue) -> Result<JsValue, JsValue> {
    let cnf = Cnf::from_dimacs(&cnf_input);

    let vtree_type: VTreeType = serde_wasm_bindgen::from_value(vtree_type_input)?;

    let vtree = build_vtree(&cnf, vtree_type);

    let builder = CompressionSddBuilder::new(vtree);
    let sdd = builder.compile_cnf(&cnf);

    let serialized = SDDSerializer::from_sdd(sdd);

    Ok(serde_wasm_bindgen::to_value(&serialized)?)
}

// used in rsdd-docs
#[wasm_bindgen]
pub fn demo_model_count_sdd(cnf_input: String) -> Result<JsValue, JsValue> {
    let cnf = Cnf::from_dimacs(&cnf_input);

    let vtree = build_vtree(&cnf, VTreeType::FromDTreeLinear);

    let builder = CompressionSddBuilder::new(vtree.clone());
    let sdd = builder.compile_cnf(&cnf);

    let mut params: WmcParams<FiniteField<{ primes::U32_TINY }>> = WmcParams::default();

    for v in 0..builder.vtree_manager().num_vars() + 1 {
        params.set_weight(
            VarLabel::new_usize(v),
            FiniteField::new(1),
            FiniteField::new(1),
        )
    }

    let model_count = sdd.wmc(builder.vtree_manager(), &params);

    let res = SddModelCountResult {
        model_count: model_count.value(),
        sdd: SDDSerializer::from_sdd(sdd),
        vtree: VTreeSerializer::from_vtree(&vtree),
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
