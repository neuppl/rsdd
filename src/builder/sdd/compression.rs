use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

use crate::backing_store::bump_table::BackedRobinhoodTable;
use crate::backing_store::UniqueTable;
use crate::builder::cache::all_app::AllTable;
use crate::builder::cache::ite::Ite;
use crate::builder::cache::LruTable;
use crate::builder::BottomUpBuilder;
use crate::repr::ddnnf::DDNNFPtr;
use crate::repr::robdd::create_semantic_hash_map;
use crate::repr::sdd::binary_sdd::BinarySDD;
use crate::repr::sdd::sdd_or::{SddAnd, SddOr};
use crate::repr::sdd::SddPtr;
use crate::repr::vtree::{VTree, VTreeIndex, VTreeManager};

use super::builder::SddBuilder;

pub struct CompressionSddManager<'a> {
    vtree: VTreeManager,
    should_compress: bool,
    // tables
    bdd_tbl: RefCell<BackedRobinhoodTable<'a, BinarySDD<'a>>>,
    sdd_tbl: RefCell<BackedRobinhoodTable<'a, SddOr<'a>>>,
    // caches
    ite_cache: RefCell<AllTable<SddPtr<'a>>>,
    app_cache: RefCell<HashMap<SddAnd<'a>, SddPtr<'a>>>,
}

impl<'a> SddBuilder<'a> for CompressionSddManager<'a> {
    #[inline]
    fn get_vtree_manager(&self) -> &VTreeManager {
        &self.vtree
    }

    #[inline]
    fn app_cache_get(&self, and: &SddAnd<'a>) -> Option<SddPtr<'a>> {
        // TODO: check if this is right?
        self.app_cache.borrow().get(and).cloned()
    }

    #[inline]
    fn app_cache_insert(&self, and: SddAnd<'a>, ptr: SddPtr<'a>) {
        self.app_cache.borrow_mut().insert(and, ptr);
    }

    #[inline]
    fn ite_cache_hash(&self, ite: &Ite<SddPtr>) -> u64 {
        self.ite_cache.borrow().hash(ite)
    }

    #[inline]
    fn ite_cache_get(&self, ite: Ite<SddPtr<'a>>, hash: u64) -> Option<SddPtr> {
        self.ite_cache.borrow().get(ite, hash)
    }

    #[inline]
    fn ite_cache_insert(&self, ite: Ite<SddPtr<'a>>, res: SddPtr<'a>, hash: u64) {
        self.ite_cache.borrow_mut().insert(ite, res, hash)
    }

    #[inline]
    fn get_or_insert_bdd(&'a self, bdd: BinarySDD<'a>) -> SddPtr<'a> {
        unsafe {
            let tbl = &mut *self.bdd_tbl.as_ptr();
            SddPtr::BDD(tbl.get_or_insert(bdd))
        }
    }

    #[inline]
    fn get_or_insert_sdd(&'a self, or: SddOr<'a>) -> SddPtr<'a> {
        unsafe {
            let tbl = &mut *self.sdd_tbl.as_ptr();
            SddPtr::Reg(tbl.get_or_insert(or))
        }
    }

    #[inline]
    fn sdd_eq(&'a self, a: SddPtr<'a>, b: SddPtr<'a>) -> bool {
        a == b
    }

    #[inline]
    fn set_compression(&mut self, b: bool) {
        self.should_compress = b
    }

    /// Canonicalizes the list of (prime, sub) terms in-place
    /// `node`: a list of (prime, sub) pairs
    fn compress(&'a self, node: &mut Vec<SddAnd<'a>>) {
        for i in 0..node.len() {
            // see if we can compress i
            let mut j = i + 1;
            while j < node.len() {
                if self.sdd_eq(node[i].sub(), node[j].sub()) {
                    // compress j into i and remove j from the node list
                    node[i] = SddAnd::new(self.or(node[i].prime(), node[j].prime()), node[i].sub());
                    node.swap_remove(j);
                } else {
                    j += 1;
                }
            }
        }
    }

    /// Returns a canonicalized SDD pointer from a list of (prime, sub) pairs
    fn canonicalize(&'a self, mut node: Vec<SddAnd<'a>>, table: VTreeIndex) -> SddPtr<'a> {
        // check for base cases before compression
        if let Some(sdd) = self.canonicalize_base_case(&node) {
            return sdd;
        }

        if self.should_compress {
            self.compress(&mut node);

            // check for a base case after compression (compression can sometimes
            // reduce node counts to a base case)
            if let Some(sdd) = self.canonicalize_base_case(&node) {
                return sdd;
            }
        }

        self.unique_or(node, table)
    }

    fn node_iter(&self) -> Vec<SddPtr> {
        let binding = self.bdd_tbl.borrow_mut();
        let bdds = binding.iter().map(SddPtr::BDD);
        let binding = self.sdd_tbl.borrow_mut();
        let sdds = binding.iter().map(SddPtr::Reg);
        bdds.chain(sdds).collect()
    }

    // eventually, remove this
    fn num_app_cache_hits(&self) -> usize {
        self.bdd_tbl.borrow().hits() + self.sdd_tbl.borrow().hits()
    }

    /// computes the number of logically redundant nodes allocated by the
    /// manager (nodes that have the same semantic hash)
    fn num_logically_redundant(&self) -> usize {
        let mut s: HashSet<u128> = HashSet::new();
        let hasher = create_semantic_hash_map::<100000000063>(self.num_vars() + 1000); // TODO FIX THIS BADNESS
        let mut num_collisions = 0;
        for n in self.node_iter() {
            let h = n.cached_semantic_hash(self.get_vtree_manager(), &hasher);
            if s.contains(&h.value()) {
                num_collisions += 1;
            }
            s.insert(h.value());
        }
        num_collisions
    }
}

impl<'a> CompressionSddManager<'a> {
    pub fn new(vtree: VTree) -> CompressionSddManager<'a> {
        let vtree_man = VTreeManager::new(vtree);
        CompressionSddManager {
            ite_cache: RefCell::new(AllTable::new()),
            app_cache: RefCell::new(HashMap::new()),
            bdd_tbl: RefCell::new(BackedRobinhoodTable::new()),
            sdd_tbl: RefCell::new(BackedRobinhoodTable::new()),
            vtree: vtree_man,
            should_compress: true,
        }
    }

    #[inline]
    fn canonicalize_base_case(&'a self, node: &Vec<SddAnd<'a>>) -> Option<SddPtr<'a>> {
        if node.is_empty() {
            return Some(SddPtr::true_ptr());
        }
        if node.len() == 1 {
            if self.is_true(node[0].prime()) {
                return Some(node[0].sub());
            }

            if self.is_false(node[0].sub()) {
                return Some(SddPtr::false_ptr());
            }
        }
        if node.len() == 2 {
            if self.is_true(node[0].sub()) && self.is_false(node[1].sub()) {
                return Some(node[0].prime());
            } else if self.is_false(node[0].sub()) && self.is_true(node[1].sub()) {
                return Some(node[1].prime());
            }
        }
        None
    }
}

// check that (a \/ b) /\ a === a
#[test]
fn simple_equality() {
    use crate::builder::bdd_builder::VarLabel;
    let man = CompressionSddManager::new(VTree::even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        2,
    ));
    let a = SddPtr::Var(VarLabel::new(0), true);
    let d = SddPtr::Var(VarLabel::new(3), true);
    let inner = man.or(a, d);
    println!("0 || 3:\n{}", man.print_sdd(inner));
    let term = man.and(inner, a);
    assert_eq!(a, term);
}

// check that (a \/ b) | !b === a
#[test]
fn sdd_simple_cond() {
    use crate::builder::bdd_builder::VarLabel;
    let man = CompressionSddManager::new(VTree::even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        2,
    ));
    let a = SddPtr::Var(VarLabel::new(0), true);
    let d = SddPtr::Var(VarLabel::new(3), true);
    let inner = man.or(a, d);
    println!("0 || 3: {}", man.print_sdd(inner));
    let term = man.condition(inner, VarLabel::new(3), false);
    assert_eq!(
        a,
        term,
        "Got:\n{}\nexpected:\n{}\n",
        man.print_sdd(term),
        man.print_sdd(a)
    );
}

#[test]
fn sdd_test_exist() {
    use crate::builder::bdd_builder::VarLabel;
    let man = CompressionSddManager::new(VTree::even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        2,
    ));
    // 1 /\ 2 /\ 3
    let v1 = SddPtr::Var(VarLabel::new(0), true);
    let v2 = SddPtr::Var(VarLabel::new(1), true);
    let v3 = SddPtr::Var(VarLabel::new(2), true);
    let a1 = man.and(v1, v2);
    let r1 = man.and(a1, v3);
    let r_expected = man.and(v1, v3);
    let res = man.exists(r1, VarLabel::new(1));
    assert!(
        man.sdd_eq(r_expected, res),
        "Got:\n{}\nExpected:\n{}",
        man.print_sdd(res),
        man.print_sdd(r_expected)
    );
}

#[test]
fn sdd_bigand() {
    use crate::builder::bdd_builder::VarLabel;
    let man = CompressionSddManager::new(VTree::right_linear(&[
        VarLabel::new(0),
        VarLabel::new(1),
        VarLabel::new(2),
        VarLabel::new(3),
        VarLabel::new(4),
    ]));

    // let man = CompressionSddManager::new(VTree::even_split(
    //     &[
    //         VarLabel::new(0),
    //         VarLabel::new(1),
    //         VarLabel::new(2),
    //         VarLabel::new(3),
    //         VarLabel::new(4),
    //     ],
    //     2,
    // ));
    // 1 /\ 2 /\ 3
    let v1 = SddPtr::Var(VarLabel::new(0), true);
    let v2 = SddPtr::Var(VarLabel::new(1), true);
    let v3 = SddPtr::Var(VarLabel::new(2), true);
    let a1 = man.and(v1, v2);
    let r1 = man.and(a1, v3);
    let f = man.and(r1, SddPtr::Var(VarLabel::new(0), false));
    println!("{}", man.print_sdd(r1));
    assert_eq!(
        f,
        SddPtr::false_ptr(),
        "Expected False, got {}",
        man.print_sdd(f)
    );
}

#[test]
fn sdd_ite1() {
    use crate::builder::bdd_builder::VarLabel;
    let man = CompressionSddManager::new(VTree::even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        2,
    ));
    let v1 = SddPtr::Var(VarLabel::new(0), true);
    let v2 = SddPtr::Var(VarLabel::new(1), true);
    println!("v1: {}", man.print_sdd(v1));
    println!("v2: {}", man.print_sdd(v2));
    let r1 = man.or(v1, v2);
    // r1: 0 \/ 1
    println!("0 || 1: {}", man.print_sdd(r1));
    let r2 = man.and(r1, v1);
    // r2: (0 \/ 1) && 1
    assert!(
        man.sdd_eq(v1, r2),
        "Not eq:\n {}\n{}",
        man.print_sdd(v1),
        man.print_sdd(r2)
    );
}

#[test]
fn sdd_demorgan() {
    use crate::builder::bdd_builder::VarLabel;
    let man = CompressionSddManager::new(VTree::even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        1,
    ));
    let x = SddPtr::Var(VarLabel::new(0), true);
    let y = SddPtr::Var(VarLabel::new(3), true);
    let res = man.or(x, y).neg();
    let expected = man.and(x.neg(), y.neg());
    assert!(
        man.sdd_eq(res, expected),
        "Not eq:\nGot: {}\nExpected: {}",
        man.print_sdd(res),
        man.print_sdd(expected)
    );
}

#[test]
fn sdd_circuit1() {
    use crate::builder::bdd_builder::VarLabel;
    let man = CompressionSddManager::new(VTree::even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        2,
    ));
    let x = SddPtr::Var(VarLabel::new(0), false);
    let y = SddPtr::Var(VarLabel::new(1), true);
    let delta = man.and(x, y);
    let yp = SddPtr::Var(VarLabel::new(2), true);
    let inner = man.iff(yp, y);
    println!("(2 <=> 1): \n{}", man.print_sdd(inner));
    println!("(0 && 1): \n{}", man.print_sdd(delta));
    let conj = man.and(inner, delta);
    println!("((!0 && 1) && (2 <=> 1)): \n{}", man.print_sdd(conj));
    let res = man.exists(conj, VarLabel::new(1));

    let expected = man.and(x, yp);
    assert!(
        man.sdd_eq(res, expected),
        "Not eq:\nGot: {}\nExpected: {}",
        man.print_sdd(res),
        man.print_sdd(expected)
    );
}

#[test]
fn sdd_circuit2() {
    use crate::builder::bdd_builder::VarLabel;
    // same as circuit1, but with a different variable order
    let man = CompressionSddManager::new(VTree::even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
            VarLabel::new(4),
        ],
        2,
    ));
    let x = SddPtr::Var(VarLabel::new(3), false);
    let y = SddPtr::Var(VarLabel::new(1), true);
    let delta = man.and(x, y);
    let yp = SddPtr::Var(VarLabel::new(4), true);
    let inner = man.iff(yp, y);
    let conj = man.and(inner, delta);
    let res = man.exists(conj, VarLabel::new(1));

    let expected = man.and(x, yp);
    assert!(
        man.sdd_eq(res, expected),
        "Not eq:\nGot: {}\nExpected: {}",
        man.print_sdd(res),
        man.print_sdd(expected)
    );
}

#[test]
fn sdd_wmc1() {
    use crate::builder::bdd_builder::VarLabel;
    use crate::util::semirings::realsemiring::RealSemiring;
    // modeling the formula (x<=>fx) && (y<=>fy), with f weight of 0.5

    // let vtree = VTree::right_linear(
    //     &[
    //         VarLabel::new(0),
    //         VarLabel::new(1),
    //         VarLabel::new(2),
    //         VarLabel::new(3),
    //     ],
    // );

    let vtree = VTree::even_split(
        &[
            VarLabel::new(0),
            VarLabel::new(1),
            VarLabel::new(2),
            VarLabel::new(3),
        ],
        1,
    );
    let man = CompressionSddManager::new(vtree);
    let mut wmc_map = crate::repr::wmc::WmcParams::new(RealSemiring(0.0), RealSemiring(1.0));
    let x = SddPtr::Var(VarLabel::new(0), true);
    wmc_map.set_weight(VarLabel::new(0), RealSemiring(1.0), RealSemiring(1.0));
    let y = SddPtr::Var(VarLabel::new(1), true);
    wmc_map.set_weight(VarLabel::new(1), RealSemiring(1.0), RealSemiring(1.0));
    let fx = SddPtr::Var(VarLabel::new(2), true);
    wmc_map.set_weight(VarLabel::new(2), RealSemiring(0.5), RealSemiring(0.5));
    let fy = SddPtr::Var(VarLabel::new(3), true);
    wmc_map.set_weight(VarLabel::new(3), RealSemiring(0.5), RealSemiring(0.5));
    let x_fx = man.iff(x, fx);
    let y_fy = man.iff(y, fy);
    let ptr = man.and(x_fx, y_fy);
    let wmc_res: RealSemiring = ptr.wmc(man.get_vtree_manager(), &wmc_map);
    let expected = RealSemiring(1.0);
    let diff = (wmc_res - expected).0.abs();
    println!("sdd: {}", man.print_sdd(ptr));
    assert!(
        (diff < 0.0001),
        "Not eq: \n Expected: {:?} \n WMC: {:?}",
        expected,
        wmc_res
    );
}
