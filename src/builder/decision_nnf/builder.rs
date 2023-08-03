use rustc_hash::FxHashMap;

use crate::{
    builder::TopDownBuilder,
    repr::{
        BddNode, BddPtr, Cnf, DDNNFPtr, DecisionResult, Literal, SATSolver, VarLabel, VarOrder,
    },
};

pub struct DecisionNNFBuilderStats {
    pub num_nodes_alloc: usize,
}

pub trait DecisionNNFBuilder<'a>: TopDownBuilder<'a, BddPtr<'a>> {
    fn order(&'a self) -> &'a VarOrder;

    /// Normalizes and fetches a node from the store
    fn get_or_insert(&'a self, bdd: BddNode<'a>) -> BddPtr<'a>;

    /// counts number of redundant nodes allocated in internal table
    fn num_logically_redundant(&self) -> usize;

    fn stats(&self) -> DecisionNNFBuilderStats;

    // impls

    fn conjoin_implied(
        &'a self,
        literals: impl Iterator<Item = Literal>,
        nnf: BddPtr<'a>,
    ) -> BddPtr<'a> {
        if nnf.is_false() {
            return BddPtr::false_ptr();
        }
        let mut sub = nnf;
        for l in literals {
            let node = if l.polarity() {
                BddNode::new(l.label(), BddPtr::false_ptr(), sub)
            } else {
                BddNode::new(l.label(), sub, BddPtr::false_ptr())
            };
            sub = self.get_or_insert(node);
        }
        sub
    }

    /// Returns A BDD that represents `cnf` conditioned on all
    ///     variables set in the current top model
    /// We need both of these BDDs for sound CNF caching
    /// `cache`: a map from hashed CNFs to their compiled BDDs
    fn topdown_h(
        &'a self,
        cnf: &Cnf,
        sat: &mut SATSolver,
        level: usize,
        cache: &mut FxHashMap<u128, BddPtr<'a>>,
    ) -> BddPtr<'a> {
        // check for base case
        if level >= cnf.num_vars() || sat.is_sat() {
            return BddPtr::true_ptr();
        }
        let cur_v = self.order().var_at_level(level);

        // check if this literal is currently set in unit propagation; if
        // it is, skip it
        if sat.is_set(cur_v) {
            return self.topdown_h(cnf, sat, level + 1, cache);
        }

        // check cache
        let hashed = sat.cur_hash();
        match cache.get(&hashed) {
            None => (),
            Some(v) => {
                return *v;
            }
        }

        // recurse on both values of cur_v
        let high_bdd = match sat.decide(Literal::new(cur_v, true)) {
            DecisionResult::UNSAT => BddPtr::false_ptr(),
            DecisionResult::SAT => {
                let new_assgn = sat.difference_iter().filter(|x| x.label() != cur_v);
                let r = self.conjoin_implied(new_assgn, BddPtr::true_ptr());
                sat.pop();
                r
            }
            DecisionResult::Unknown => {
                let sub = self.topdown_h(cnf, sat, level + 1, cache);
                let new_assgn = sat.difference_iter().filter(|x| x.label() != cur_v);
                let r = self.conjoin_implied(new_assgn, sub);
                sat.pop();
                r
            }
        };
        let low_bdd = match sat.decide(Literal::new(cur_v, false)) {
            DecisionResult::UNSAT => BddPtr::false_ptr(),
            DecisionResult::SAT => {
                let new_assgn = sat.difference_iter().filter(|x| x.label() != cur_v);
                let r = self.conjoin_implied(new_assgn, BddPtr::true_ptr());
                sat.pop();
                r
            }
            DecisionResult::Unknown => {
                let sub = self.topdown_h(cnf, sat, level + 1, cache);
                let new_assgn = sat.difference_iter().filter(|x| x.label() != cur_v);
                let r = self.conjoin_implied(new_assgn, sub);
                sat.pop();
                r
            }
        };

        let r = if high_bdd == low_bdd {
            high_bdd
        } else {
            let bdd = BddNode::new(cur_v, low_bdd, high_bdd);
            self.get_or_insert(bdd)
        };
        cache.insert(hashed, r);
        r
    }

    /// compile a decision DNNF top-down from a CNF
    fn compile_cnf_topdown(&'a self, cnf: &Cnf) -> BddPtr<'a> {
        let mut sat = match SATSolver::new(cnf.clone()) {
            Some(v) => v,
            None => return BddPtr::false_ptr(),
        };

        let mut r = self.topdown_h(cnf, &mut sat, 0, &mut FxHashMap::default());

        // conjoin in any initially implied literals
        for l in sat.difference_iter() {
            let node = if l.polarity() {
                BddNode::new(l.label(), BddPtr::false_ptr(), r)
            } else {
                BddNode::new(l.label(), r, BddPtr::false_ptr())
            };
            r = self.get_or_insert(node);
        }
        r
    }

    fn cond_helper(&'a self, bdd: BddPtr<'a>, lbl: VarLabel, value: bool) -> BddPtr<'a> {
        match bdd {
            BddPtr::PtrTrue | BddPtr::PtrFalse => bdd,
            BddPtr::Reg(node) | BddPtr::Compl(node) if node.var == lbl => {
                let r = if value { bdd.high() } else { bdd.low() };
                if bdd.is_neg() {
                    r.neg()
                } else {
                    r
                }
            }
            BddPtr::Reg(node) | BddPtr::Compl(node) => {
                // check cache
                if let Some(v) = bdd.scratch::<BddPtr>() {
                    return if bdd.is_neg() { v.neg() } else { v };
                }

                // recurse on the children
                let l = self.cond_helper(bdd.low(), lbl, value);
                let h = self.cond_helper(bdd.high(), lbl, value);
                if l == h {
                    if bdd.is_neg() {
                        return l.neg();
                    } else {
                        return l;
                    };
                };
                let res = if l != bdd.low() || h != bdd.high() {
                    // cache and return the new BDD
                    let new_bdd = BddNode::new(node.var, l, h);
                    let r = self.get_or_insert(new_bdd);
                    if bdd.is_neg() {
                        r.neg()
                    } else {
                        r
                    }
                } else {
                    bdd
                };
                // TODO: fix scratch lifetime issue
                // bdd.set_scratch(if bdd.is_neg() { res.neg() } else { res });
                res
            }
        }
    }
}

impl<'a, T> TopDownBuilder<'a, BddPtr<'a>> for T
where
    T: DecisionNNFBuilder<'a>,
{
    /// Get a pointer to the variable with label `lbl` and polarity `polarity`
    fn var(&'a self, lbl: VarLabel, polarity: bool) -> BddPtr<'a> {
        let bdd = BddNode::new(lbl, BddPtr::false_ptr(), BddPtr::true_ptr());
        let r = self.get_or_insert(bdd);
        if polarity {
            r
        } else {
            r.neg()
        }
    }

    /// Compute the Boolean function `f | var = value`
    fn condition(&'a self, bdd: BddPtr<'a>, lbl: VarLabel, value: bool) -> BddPtr<'a> {
        let r = self.cond_helper(bdd, lbl, value);
        bdd.clear_scratch();
        r
    }
}
