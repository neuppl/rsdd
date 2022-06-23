//! Top-down knowledge compiler

// use std::collections::HashMap;

// use crate::repr::bdd::*;
// use crate::repr::cnf::*;
// use crate::builder::var_order::*;

// struct CnfToBDD<'a> {
//     cnf: &'a Cnf,
//     up: UnitPropagate<'a>, 
//     alloc: Vec<Bdd>,
//     unique_table: HashMap<Bdd, BddPtr>, 
//     order: VarOrder
// }

// impl<'a> CnfToBDD<'a> {
//     /// returns a pointer to a canonical and unique version of this BDD
//     pub fn unique_and_canonical(bdd: Bdd) -> BddPtr {
//         todo!()
//     }
// }

// fn cnf_to_bdd_h(state: &mut CnfToBDD, level: usize) -> BddPtr {
//     let cur_v = state.order.var_at_pos(level);
//     // check if this variable is currently implied; if it is, use its 
//     // implied value and continue
//     match state.up.get_assgn().get(cur_v) {
//         Some(v) => {

//         },
//         None => {
//             // this variable is not implied and not set; branch on both possible
//             // values
//         }
//     }
// }

// pub fn cnf_to_bdd(cnf: &Cnf) -> FinalizedBDD {

// }