// Contains generator for generalised network example in
// Minsung's submission to DRAGSTERS 2023.

extern crate rsdd;
extern crate quickcheck;
use crate::repr::var_label::VarLabel;
use rsdd::builder::bdd_builder::BddManager;
use rsdd::builder::cache::all_app::AllTable;
use rsdd::builder::canonicalize::*;
use rsdd::repr::bdd::BddPtr;
use rsdd::repr::ddnnf::DDNNFPtr;
use rsdd::repr::vtree::VTree;
use rsdd::util::semiring::{ExpectedUtility, Semiring};
use rsdd::repr::wmc::WmcParams;
use rsdd::*;
use serde::de::Expected;
use std::collections::HashMap;
extern crate rand;

// Generates a network with top/bottom path lengths n+1
// For example, network_gen(4) will generate a network with top path edges
// st1, t1t2, t2t3, t3t4, t4e.
// Output is: BDDPtr to network, associated BDDManager, Vec of edge VarLabels
fn network_gen(n : usize) 
    -> (BddPtr, BddManager<AllTable<BddPtr>>, Vec<VarLabel>) {
    // Initialize BDD
    let bdd_size = (2 * n) + 2;
    let mut man = BddManager::<AllTable<BddPtr>>::new_default_order(bdd_size);

    // Initialize variables in the BDD, sort into top or bottom edge
    let v: Vec<VarLabel> = (0..bdd_size)
            .map(|x| VarLabel::new(x as u64))
            .collect();
    let w = v.clone();
    let len = n+1;
    let mut top_edges = Vec::<BddPtr>::with_capacity(len);
    let mut bot_edges = Vec::<BddPtr>::with_capacity(len);
    for i in v { 
        let i_val = i.value_usize();
        if i_val < n+1 { 
            top_edges.insert(i_val % (n+1), man.var(i, true)); 
        }
        else {
            bot_edges.insert(i_val % (n+1), man.var(i, true));
        }
    }

    // Generate network
    let top_path = man.and_lst(&top_edges);
    let bot_path = man.and_lst(&bot_edges);
    let network = man.xor(top_path, bot_path);

    (network, man, w)
}

// Given a BDDmanager and associated n, generates decision problem
// wrt the network semantics given in network_gen.
// Output: decision tree BDDPtr, associated BDDManager,
//         Vec of VarLabels of Decisions, VarLabel of Reward.
fn decisions(
    n : usize,
    mut man : BddManager<AllTable<BddPtr>>
) -> (BddPtr, BddManager<AllTable<BddPtr>>, Vec<VarLabel>, VarLabel) {
    // Initialize top and bottom decisions, reward
    let top_dec: Vec<(VarLabel, BddPtr)> = (0..n)
            .map(|_x| man.new_var(true))
            .collect();
    let bot_dec: Vec<(VarLabel, BddPtr)> = (0..n)
            .map(|_x| man.new_var(true))
            .collect();
    let (mut top_labels, top_ptrs): (Vec<_>, Vec<_>) = top_dec.into_iter().unzip();
    let (mut bot_labels, bot_ptrs): (Vec<_>, Vec<_>) = bot_dec.into_iter().unzip();
    let (rew_label, rew_ptr) = man.new_var(true);
    
    // A HashMap storing a decision label the related ITE BDDPtr that
    // encodes if adjacent edges were failed, we get reward
    let mut adj_ite : HashMap<BddPtr, BddPtr> = HashMap::new();
    let mut i = 0;
    let top_labels_cl = top_ptrs.clone();
    for t in top_labels_cl {
        let left = man.var(VarLabel::new(i), true);
        let right = man.var(VarLabel::new(i+1), true);
        let ifguard = man.or(left.neg(), right.neg());
        let then_branch = man.and(ifguard, rew_ptr);
        let else_branch = man.and(ifguard.neg(), rew_ptr.neg());
        let ite = man.or(then_branch, else_branch);
        adj_ite.insert(t, ite);
        i = i+1;
        
    }
    // We need this to not accidentally connect the last edge of 
    // top path and first edge of bot path.
    i = i+1;
    let bot_labels_cl = bot_ptrs.clone();
    for b in bot_labels_cl {
        let left = man.var(VarLabel::new(i), true);
        let right = man.var(VarLabel::new(i+1), true);
        let ifguard = man.or(left.neg(), right.neg());
        let then_branch = man.and(ifguard, rew_ptr);
        let else_branch = man.and(ifguard.neg(), rew_ptr.neg());
        let ite = man.or(then_branch, else_branch);
        adj_ite.insert(b, ite);
        i = i+1;
    }
    
    // All decisions aggregator
    let mut all_decs_cl = top_ptrs.clone();
    let mut bot_ptrs_cl = bot_ptrs.clone();
    all_decs_cl.append(&mut bot_ptrs_cl);
    let res_decs = all_decs_cl.clone();

    // We get our list of decision branch clauses.
    let mut list_of_branches : Vec<BddPtr> = Vec::new();
    let e = all_decs_cl.clone();
    for ptr in e {
        // Helper function that, given a BDDPtr to a decision, gives a
        // BDDPtr to the conjunction of all the decisions negated except 
        // for the specified input.
        let mut neg_all_but_one = |u : BddPtr| {
            let dec_ptrs_cl = all_decs_cl.clone();
            let mut ret = Vec::new();
            for ptr in dec_ptrs_cl {
                if ptr == u {
                    ret.push(ptr);
                } else {
                    let neg = ptr.neg();
                    ret.push(neg);
                }
            }
            man.and_lst(&ret)
        };
        let neg_all_but_ptr = neg_all_but_one(ptr);
        let assoc_ite = adj_ite.get(&ptr);
        match assoc_ite {
            None => panic!(),
            Some(v) => {let join = man.and(neg_all_but_ptr, *v);
                list_of_branches.push(join)},
        }
    }
    let x = list_of_branches.len();
    print!("list of branches is {}\n", x);
    top_labels.append(&mut bot_labels);
    (man.or_lst(&list_of_branches), man, top_labels, rew_label)
}

#[test]
fn gen() {
    let (network, man, edge_lbls) = network_gen(1);
    let (decs, mut man2, dec_lbls, rw_lbls) = decisions(1, man);
    let mut eu_map : HashMap<VarLabel, (ExpectedUtility, ExpectedUtility)> 
        = HashMap::new();
    let vars = dec_lbls.clone();

    let probs = [0.55, 0.65, 0.92, 0.73];
    let mut i = 0;
    for e in edge_lbls {
        let x = probs[i];
        eu_map.insert(e, (ExpectedUtility(1.0-x, 0.0), ExpectedUtility(x, 0.0)));
        i = i+1;
    }
    for d in dec_lbls {
        eu_map.insert(d, (ExpectedUtility::one(), ExpectedUtility::one()));
    }
    eu_map.insert(rw_lbls, 
                  (ExpectedUtility::one(), ExpectedUtility(1.0, 10.0)));
    
    let network_fail = network.neg();
    let end = man2.and(decs, network_fail);
    let wmc = WmcParams::new_with_default(ExpectedUtility::zero(), ExpectedUtility::one(), eu_map);

    let (meu_num, pm) = end.meu(&vars, man2.num_vars(), &wmc);   
    let (meu_dec, _) = network_fail.meu(&vars, man2.num_vars(), &wmc); 
    println!("MEU: {} \n PM : {:?}", meu_num.1 / meu_dec.0, pm);
}