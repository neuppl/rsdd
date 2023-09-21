// Contains generator for generalised network example in
// Minsung's submission to DRAGSTERS 2023.

extern crate rsdd;
use crate::repr::VarLabel;
use rsdd::builder::bdd::{BddBuilder, RobddBuilder};
use rsdd::builder::cache::AllIteTable;
use rsdd::builder::BottomUpBuilder;
use rsdd::repr::BddPtr;
use rsdd::repr::DDNNFPtr;
use rsdd::repr::WmcParams;
use rsdd::util::semirings::{ExpectedUtility, Semiring};
use rsdd::*;
use std::collections::HashMap;
extern crate rand;

// Generates a network with top/bottom path lengths n+1, given a BDD Builder
// For example, network_gen(4) will generate a network with top path edges
// st1, t1t2, t2t3, t3t4, t4e.
// Output is: BDDPtr to network, Vec of edge VarLabels
fn network_gen<'a>(
    n: usize,
    builder: &'a RobddBuilder<'a, AllIteTable<BddPtr<'a>>>,
) -> (BddPtr<'a>, Vec<VarLabel>) {
    // Initialize BDD
    let bdd_size = (2 * n) + 2;

    // Initialize variables in the BDD, sort into top or bottom edge
    let v: Vec<VarLabel> = (0..bdd_size).map(|x| VarLabel::new(x as u64)).collect();
    let w = v.clone();
    let len = n + 1;
    let mut top_edges = Vec::<BddPtr>::with_capacity(len);
    let mut bot_edges = Vec::<BddPtr>::with_capacity(len);
    for i in v {
        let i_val = i.value_usize();
        if i_val < n + 1 {
            top_edges.insert(i_val % (n + 1), builder.var(i, true));
        } else {
            bot_edges.insert(i_val % (n + 1), builder.var(i, true));
        }
    }

    // Generate network
    let top_path = builder.and_lst(&top_edges);
    let bot_path = builder.and_lst(&bot_edges);
    let network = builder.xor(top_path, bot_path);

    (network, w)
}

// Given a BDDbuilder and associated n, generates decision problem
// wrt the network sebuildertics given in network_gen.
// Output: decision tree BDDPtr,
//         Vec of VarLabels of Decisions, VarLabel of Reward.
fn decisions<'a>(
    n: usize,
    builder: &'a RobddBuilder<'a, AllIteTable<BddPtr<'a>>>,
) -> (BddPtr<'a>, Vec<VarLabel>, VarLabel) {
    // Initialize top and bottom decisions, reward
    let top_dec: Vec<(VarLabel, BddPtr)> = (0..n).map(|_x| builder.new_var(true)).collect();
    let bot_dec: Vec<(VarLabel, BddPtr)> = (0..n).map(|_x| builder.new_var(true)).collect();
    let (mut top_labels, top_ptrs): (Vec<_>, Vec<_>) = top_dec.into_iter().unzip();
    let (mut bot_labels, bot_ptrs): (Vec<_>, Vec<_>) = bot_dec.into_iter().unzip();
    let (rew_label, rew_ptr) = builder.new_var(true);

    // A HashMap storing a decision label the related ITE BDDPtr that
    // encodes if adjacent edges were failed, we get reward
    let mut adj_ite: HashMap<BddPtr, BddPtr> = HashMap::new();
    let mut i = 0;
    let top_ptrs_cl = top_ptrs.clone();
    for t in top_ptrs_cl {
        match t {
            BddPtr::Reg(node) | BddPtr::Compl(node) => {
                let x = node.var.value();
                println!(
                    "Top Decision variable {} is joined to edges {} and {}",
                    x,
                    i,
                    i + 1
                );
                let left = builder.var(VarLabel::new(i), false);
                let right = builder.var(VarLabel::new(i + 1), false);
                let ifguard = builder.or(left, right);
                let ite = builder.ite(ifguard, rew_ptr, rew_ptr.neg());
                adj_ite.insert(t, ite);
                i += 1;
            }
            BddPtr::PtrTrue | BddPtr::PtrFalse => panic!("hit constant!"),
        }
    }
    // We need this to not accidentally connect the last edge of
    // top path and first edge of bot path.
    i += 1;
    let bot_ptrs_cl = bot_ptrs.clone();
    for b in bot_ptrs_cl {
        match b {
            BddPtr::Reg(node) | BddPtr::Compl(node) => {
                let y = node.var.value();
                println!(
                    "Bot Decision variable {} is joined to edges {} and {}",
                    y,
                    i,
                    i + 1
                );
                let left = builder.var(VarLabel::new(i), false);
                let right = builder.var(VarLabel::new(i + 1), false);
                let ifguard = builder.or(left, right);
                let ite = builder.ite(ifguard, rew_ptr, rew_ptr.neg());
                adj_ite.insert(b, ite);
                i += 1;
            }
            BddPtr::PtrTrue | BddPtr::PtrFalse => panic!("hit constant!"),
        }
    }

    // All decisions aggregator
    let mut all_decs_cl = top_ptrs;
    let mut bot_ptrs_cl = bot_ptrs;
    all_decs_cl.append(&mut bot_ptrs_cl);

    // We get our list of decision branch clauses.
    let mut list_of_branches: Vec<BddPtr> = Vec::new();
    let e = all_decs_cl.clone();
    for ptr in e {
        // Helper function that, given a BDDPtr to a decision, gives a
        // BDDPtr to the conjunction of all the decisions negated except
        // for the specified input.
        let neg_all_but_one = |u: BddPtr| {
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
            builder.and_lst(&ret)
        };
        let neg_all_but_ptr = neg_all_but_one(ptr);
        let assoc_ite: Option<&BddPtr> = adj_ite.get(&ptr);
        match assoc_ite {
            None => panic!(),
            Some(v) => {
                let join = builder.and(neg_all_but_ptr, *v);
                list_of_branches.push(join)
            }
        }
    }

    top_labels.append(&mut bot_labels);
    (builder.or_lst(&list_of_branches), top_labels, rew_label)
}

#[test]
fn gen() {
    use std::time::Instant;

    let n = 4;

    let bdd_size = (2 * n) + 2;
    let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(bdd_size);

    let (network, edge_lbls) = network_gen(n, &builder);
    let (decs, dec_lbls, rw_lbls) = decisions(n, &builder);
    let mut eu_map: HashMap<VarLabel, (ExpectedUtility, ExpectedUtility)> = HashMap::new();
    let vars = dec_lbls.clone();

    let probs = [0.35, 0.13, 0.71, 0.79, 0.08, 0.86, 0.31, 0.12, 0.75, 0.26];
    for (i, e) in edge_lbls.into_iter().enumerate() {
        let x = probs[i];
        // println!("Assigning probability {} to variable {}", x, e.value());
        eu_map.insert(e, (ExpectedUtility(1.0 - x, 0.0), ExpectedUtility(x, 0.0)));
    }
    for d in dec_lbls {
        eu_map.insert(d, (ExpectedUtility::one(), ExpectedUtility::one()));
    }
    eu_map.insert(
        rw_lbls,
        (ExpectedUtility::one(), ExpectedUtility(1.0, 10.0)),
    );

    let network_fail = network.neg();
    let end = builder.and(decs, network_fail);
    let wmc = WmcParams::new(eu_map);

    let now = Instant::now();
    let (meu_num, pm) = end.meu(network_fail, &vars, builder.num_vars(), &wmc);
    println!(
        "Regular MEU: {} \nPM : {:?}",
        meu_num.1, pm.true_assignments
    );
    let elapsed = now.elapsed();
    println!("Elapsed: {:.2?}", elapsed);
    let now2 = Instant::now();
    let (meu_num_bb, pm_bb) = end.bb(&vars, builder.num_vars(), &wmc);
    let (meu_dec_bb, _) = network_fail.bb(&vars, builder.num_vars(), &wmc);

    println!(
        "BB MEU: {} \nPM : {:?}",
        meu_num_bb.1 / meu_dec_bb.0,
        pm_bb.true_assignments
    );
    let elapsed2 = now2.elapsed();
    println!("BB Elapsed: {:.2?}", elapsed2);
}

// Uncomment and run test if you need a sanity check above code works.
#[test]
fn sanity_check() {
    let n = 3;

    let bdd_size = (2 * n) + 2;
    let builder = RobddBuilder::<AllIteTable<BddPtr>>::new_with_linear_order(bdd_size);

    let (_, _) = network_gen(n, &builder);
    let (decs, _, _) = decisions(n, &builder);

    let st1 = builder.var(VarLabel::new(0), true);
    let t1t2 = builder.var(VarLabel::new(1), true);
    let t2t3 = builder.var(VarLabel::new(2), true);
    let t3e = builder.var(VarLabel::new(3), true);

    let sb1 = builder.var(VarLabel::new(4), true);
    let b1b2 = builder.var(VarLabel::new(5), true);
    let b2b3 = builder.var(VarLabel::new(6), true);
    let b3e = builder.var(VarLabel::new(7), true);

    let t1 = builder.var(VarLabel::new(8), true);
    let t2 = builder.var(VarLabel::new(9), true);
    let t3 = builder.var(VarLabel::new(10), true);

    let b1 = builder.var(VarLabel::new(11), true);
    let b2 = builder.var(VarLabel::new(12), true);
    let b3 = builder.var(VarLabel::new(13), true);

    let r = builder.var(VarLabel::new(14), true);

    let ig_t1 = builder.or(st1.neg(), t1t2.neg());
    let ig_t2 = builder.or(t1t2.neg(), t2t3.neg());
    let ig_t3 = builder.or(t2t3.neg(), t3e.neg());

    let ig_b1 = builder.or(sb1.neg(), b1b2.neg());
    let ig_b2 = builder.or(b1b2.neg(), b2b3.neg());
    let ig_b3 = builder.or(b2b3.neg(), b3e.neg());

    let t1_ite = builder.ite(ig_t1, r, r.neg());
    let t2_ite = builder.ite(ig_t2, r, r.neg());
    let t3_ite = builder.ite(ig_t3, r, r.neg());

    let b1_ite = builder.ite(ig_b1, r, r.neg());
    let b2_ite = builder.ite(ig_b2, r, r.neg());
    let b3_ite = builder.ite(ig_b3, r, r.neg());

    let t1_d = builder.and_lst(&[t1_ite, t1, t2.neg(), t3.neg(), b1.neg(), b2.neg(), b3.neg()]);
    let t2_d = builder.and_lst(&[t2_ite, t1.neg(), t2, t3.neg(), b1.neg(), b2.neg(), b3.neg()]);
    let t3_d = builder.and_lst(&[t3_ite, t1.neg(), t2.neg(), t3, b1.neg(), b2.neg(), b3.neg()]);

    let b1_d = builder.and_lst(&[b1_ite, t1.neg(), t2.neg(), t3.neg(), b1, b2.neg(), b3.neg()]);
    let b2_d = builder.and_lst(&[b2_ite, t1.neg(), t2.neg(), t3.neg(), b1.neg(), b2, b3.neg()]);
    let b3_d = builder.and_lst(&[b3_ite, t1.neg(), t2.neg(), t3.neg(), b1.neg(), b2.neg(), b3]);

    let dec = builder.or_lst(&[t1_d, t2_d, t3_d, b1_d, b2_d, b3_d]);

    assert!(builder.eq(dec, decs));
}
