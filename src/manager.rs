use bdd_table::BddTable;
use var_order::VarOrder;
use bdd::*;
use std::collections::{HashMap, HashSet};
use std::slice;
use apply_cache::{BddApplyTable, BddCacheStats};
use ref_table::*;


/// Represent a standard ITE manipulation Ite(f, g, h)
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Ite(BddPtr, BddPtr, BddPtr);

fn mk_ite(f: &BddPtr, g: &BddPtr, h: &BddPtr) -> Ite {
    // cloning pointers is just a stack copy so this is very cheap
    Ite(f.clone(), g.clone(), h.clone())
}

impl Ite {
    /// standardize an ITE into a consistent representation. See blue book p. 115
    fn standardize(&self, man: &BddManager) -> Ite {
        // first, transform Ite's into constants when possible
        let phase1 = match *self {
            // first check for any base cases, which we do not want to process further
            Ite(ref f, ref g, ref h)
                if f.is_true() || (g.is_true() && h.is_false()) ||
                (g.is_false() && h.is_true()) || g == h
                => return self.clone(),
            Ite(ref f, ref g, ref h) if man.is_var(*f) => {
                if f.is_compl() { mk_ite(&f.neg(), h, g) } else { self.clone() }
            },
            // negate this base case
            Ite(ref f, ref g, ref h) if f.is_false() => return mk_ite(&f.neg(), h, g),
            // now transform constants
            Ite(ref f, ref g, ref h) if g == h => mk_ite(f, &BddPtr::true_node(), h),
            Ite(ref f, ref g, ref h) if f == h => mk_ite(f, g, &BddPtr::false_node()),
            Ite(ref f, ref g, ref h) if (h.neg() == *f) => mk_ite(f, g, &BddPtr::true_node()),
            Ite(ref f, ref g, ref h) if (f.neg() == *g) => mk_ite(f, &BddPtr::false_node(), h),
            _ => self.clone(),
        };
        // then, re-order bdds based on their precedence in the variable order
        let o = man.get_order();
        let phase2 = match phase1 {
            Ite(ref f, ref g, ref h) if g.is_true() => {
                if o.lt(f.label(), h.label()) {
                    mk_ite(f, g, h)
                } else {
                    mk_ite(h, g, f)
                }
            }
            Ite(ref f, ref g, ref h) if h.is_false() => {
                if o.lt(f.label(), g.label()) {
                    mk_ite(f, g, h)
                } else {
                    mk_ite(g, f, h)
                }
            }
            Ite(ref f, ref g, ref h) if h.is_true() => {
                if o.lt(f.label(), g.label()) {
                    mk_ite(f, g, h)
                } else {
                    mk_ite(&g.neg(), &f.neg(), h)
                }
            }
            Ite(ref f, ref g, ref h) if g.is_false() => {
                if o.lt(f.label(), h.label()) {
                    mk_ite(f, g, h)
                } else {
                    mk_ite(&h.neg(), g, &f.neg())
                }
            }
            Ite(ref f, ref g, ref h) if *g == h.neg() => {
                if o.lt(f.label(), g.label()) {
                    mk_ite(f, g, h)
                } else {
                    mk_ite(g, f, &f.neg())
                }
            }
            _ => phase1,
        };
        match phase2 {
            Ite(ref f, ref g, ref h) if f.is_compl() => mk_ite(&f.neg(), h, g),
            _ => phase2,
        }
    }
}


#[derive(Debug, Clone)]
enum ControlElement {
    Ite(Ite),
    /// Create a BDD with a chosen polarity and top variable
    Bind(bool, VarLabel),
    ApplyCache(Ite),
}

pub struct BddManager {
    compute_table: BddTable,
    apply_table: BddApplyTable,
    control_stack: Vec<ControlElement>,
    data_stack: Vec<BddPtr>,
}


impl BddManager {
    /// Make a BDD manager with a default variable ordering
    pub fn new_default_order(num_vars: usize) -> BddManager {
        let default_order = VarOrder::linear_order(num_vars);
        BddManager::new(default_order)
    }

    pub fn new(order: VarOrder) -> BddManager {
        let len = order.len();
        BddManager {
            compute_table: BddTable::new(order),
            apply_table: BddApplyTable::new(len),
            control_stack: Vec::new(),
            data_stack: Vec::new(),
        }
    }

    pub fn get_order(&self) -> &VarOrder {
        self.compute_table.order()
    }

    fn deref(&self, ptr: BddPtr) -> Bdd {
        self.compute_table.deref(ptr)
    }

    fn get_application(&mut self, f: BddPtr, g: BddPtr, h: BddPtr) -> Option<BddPtr> {
        match self.apply_table.get(f, g, h) {
            Some(r) => Some(r.clone()),
            None => None,
        }
    }

    fn insert_application(&mut self, f: BddPtr, g: BddPtr, h: BddPtr, result: BddPtr) -> () {
        self.apply_table.insert(f, g, h, result);
    }

    /// Fetch the BDD pointed to by the low-node of `ptr`, panics on constant
    // BDDs
    fn low(&self, ptr: BddPtr) -> BddPtr {
        let b = self.deref(ptr).into_node();
        b.low
    }

    /// Fetch the BDD pointed to by the high-node of `ptr`, panics on constant
    /// BDDs
    fn high(&self, ptr: BddPtr) -> BddPtr {
        let b = self.deref(ptr).into_node();
        b.high
    }

    /// Push a variable onto the stack
    pub fn var(&mut self, lbl: VarLabel, is_true: bool) -> BddPtr {
        let bdd = Bdd::new_node(BddPtr::false_node(), BddPtr::true_node(), lbl);
        let r = self.get_or_insert(bdd);
        if is_true { r } else { r.neg() }
    }

    pub fn true_ptr(&self) -> BddPtr {
        BddPtr::true_node()
    }

    pub fn false_ptr(&self) -> BddPtr {
        BddPtr::false_node()
    }

    fn get_or_insert(&mut self, bdd: Bdd) -> BddPtr {
        self.compute_table.get_or_insert(bdd)
    }

    pub fn print_bdd(&self, ptr: BddPtr) -> String {
        use bdd::PointerType::*;
        fn print_bdd_helper(t: &BddManager, ptr: BddPtr) -> String {
            match ptr.ptr_type() {
                PtrTrue => String::from("T"),
                PtrFalse => String::from("T"),
                PtrNode => {
                    let l_p = t.low(ptr);
                    let h_p = t.high(ptr);
                    let l_s = print_bdd_helper(t, l_p);
                    let r_s = print_bdd_helper(t, h_p);
                    format!(
                        "({}, {}{}, {}{})",
                        ptr.var(),
                        if l_p.is_compl() { "!" } else { "" },
                        l_s,
                        if h_p.is_compl() { "!" } else { "" },
                        r_s
                    )
                }
            }
        }
        let s = print_bdd_helper(self, ptr);
        format!("{}{}", if ptr.is_compl() { "!" } else { "" }, s)
    }


    pub fn negate(&mut self, ptr: BddPtr) -> BddPtr {
        ptr.neg()
    }

    pub fn print_bdd_lbl(&self, ptr: BddPtr, map: &HashMap<VarLabel, VarLabel>) -> String {
        use bdd::PointerType::*;
        fn print_bdd_helper(
            t: &BddManager,
            ptr: BddPtr,
            map: &HashMap<VarLabel, VarLabel>,
        ) -> String {
            match ptr.ptr_type() {
                PtrTrue => String::from("T"),
                PtrFalse => String::from("F"),
                PtrNode => {
                    let l_p = t.low(ptr);
                    let h_p = t.high(ptr);
                    let l_s = print_bdd_helper(t, l_p, map);
                    let r_s = print_bdd_helper(t, h_p, map);
                    let lbl = map.get(&VarLabel::new(ptr.var())).unwrap();
                    format!("({:?}, {}, {})", lbl.value(), l_s, r_s)
                }
            }
        }
        print_bdd_helper(self, ptr, map)
    }


    /// true if `a` represents a variable (both high and low are constant)
    #[inline]
    pub fn is_var(&self, ptr: BddPtr) -> bool {
        match ptr.ptr_type() {
            PointerType::PtrNode => {
                let b = self.compute_table.deref(ptr).into_node();
                b.low.is_const() && b.high.is_const()
            }
            _ => false,
        }
    }

    /// Helper for `apply`. If the top of `ptr` is
    /// 'lbl', then return high if compl, low if not compl, otherwise return
    /// self if the top variable does not match
    fn fix_bdd(&self, ptr: BddPtr, lbl: VarLabel, compl: bool) -> BddPtr {
        match ptr.ptr_type() {
            PointerType::PtrNode => {
                let bdd = self.deref(ptr).into_node();
                if bdd.var == lbl {
                    let ret = if compl { bdd.high } else { bdd.low };
                    if ptr.is_compl() { ret.neg() } else { ret }
                } else {
                    ptr
                }
            }
            _ => ptr,
        }
    }

    fn print_control(&self, elem: ControlElement) -> String {
        match elem {
            ControlElement::Ite(Ite(f, g, h)) => {
                        format!("Ite({}, {}, {})",
                                self.print_bdd(f),
                                self.print_bdd(g),
                                self.print_bdd(h))
            },
            ControlElement::Bind(b, v) => format!("Bind({:?}, {})", v, b),
            ControlElement::ApplyCache(Ite(f, g, h)) =>
                format!("CacheIte({}, {}, {})",
                        self.print_bdd(f),
                        self.print_bdd(g),
                        self.print_bdd(h))

        }
    }

    /// pushes the resulting application onto the top of the data stack
    pub fn apply(&mut self, op: Op, a: BddPtr, b: BddPtr) -> BddPtr {
        use bdd::Op::*;
        use bdd::PointerType::*;
        let ite = match op {
            BddAnd => mk_ite(&a, &b, &BddPtr::false_node()),
            BddOr => mk_ite(&a, &BddPtr::true_node(), &b),
        };

        self.control_stack.push(ControlElement::Ite(ite));
        loop {
            let top = self.control_stack.pop();
            match top {
                Some(top) => {
                    // println!("Popped {}", self.print_control(top.clone()));
                    // for bdd in self.data_stack.iter() {
                    //     println!("  {}", self.print_bdd(*bdd));
                    // }
                    match top {
                        ControlElement::Bind(pol, vlabel) => {
                            let low = self.data_stack.pop().unwrap();
                            let high = self.data_stack.pop().unwrap();
                            if low == high {
                                self.data_stack.push(low);
                                continue;
                            }
                            let r = match (high.ptr_type(), low.ptr_type()) {
                                (PtrTrue, PtrTrue) => BddPtr::true_node(),
                                (PtrFalse, PtrFalse) => BddPtr::false_node(),
                                _ => {
                                    // canonicalize: high node should never be negated
                                    if high.is_compl() {
                                        let bdd = Bdd::new_node(low.neg(), high.neg(), vlabel);
                                        self.get_or_insert(bdd).neg()
                                    } else {
                                        let bdd = Bdd::new_node(low, high, vlabel);
                                        self.get_or_insert(bdd)
                                    }
                                }
                            };
                            let f = if pol { r } else { r.neg() };
                            // println!("pushed {}", self.print_bdd(f));
                            self.data_stack.push(f);
                        }
                        ControlElement::ApplyCache(ite) => {
                            let Ite(f, g, h) = ite;
                            let itm = self.data_stack.last().unwrap().clone();
                            self.insert_application(f, g, h, itm)
                        }
                        ControlElement::Ite(i) => {
                            // first standardize it
                            let standard = i.standardize(self);
                            // println!("standardized: {}", self.print_ite(standard.clone()));
                            // check if it is a base case; is Some(BddRef) if a
                            // base-case is encountered
                            let v = match standard {
                                // first check for any constants
                                Ite(ref f, ref g, _) if f.is_true() => Some(g.clone()),
                                Ite(ref f, _, ref h) if f.is_false() => Some(h.clone()),
                                Ite(ref f, ref g, ref h) if g.is_true() && h.is_false() => Some(
                                    f.clone(),
                                ),
                                Ite(ref f, ref g, ref h) if g.is_false() && h.is_true() => Some(
                                    f.neg(),
                                ),
                                Ite(_, ref g, ref h) if g == h => Some(g.clone()),
                                // because we standardized by sorting on the left-most node, if
                                // f = x_i (i.e., a single variable) and x_i is located before
                                // the first variables of g and h in the ordering, then we can
                                // simplify by producing the BDD (x_i, g, h)
                                Ite(ref f, ref g, ref h) if self.is_var(*f) => {
                                    // check that the variable in f occurs first
                                    // println!("is var");
                                    let f_lbl = f.label();
                                    let g_lbl = g.label();
                                    let h_lbl = h.label();
                                    if self.get_order().lt(f_lbl, g_lbl) &&
                                        self.get_order().lt(f_lbl, h_lbl)
                                    {
                                        // standardization guarantees that f
                                        // will never be complemented
                                        let bdd = Bdd::new_node(h.clone(), g.clone(), f_lbl);
                                        Some(self.get_or_insert(bdd))
                                    } else {
                                        None
                                    }
                                }
                                _ => None,
                            };
                            match v {
                                Some(a) => self.data_stack.push(a),
                                None => {
                                    let Ite(f, g, h) = standard;
                                    match self.apply_table.get(f, g, h) {
                                        Some(a) => {
                                            // push an uncomplemented BDD onto the stack
                                            self.data_stack.push(a.clone())
                                        }
                                        None => {
                                            // the ite is not in the cache and it is not a base case
                                            self.control_stack.push(ControlElement::ApplyCache(
                                                standard.clone(),
                                            ));
                                            let Ite(f, g, h) = standard;
                                            let idx = self.get_order().first_essential(f, g, h);
                                            self.control_stack.push(ControlElement::Bind(
                                                !f.is_compl(),
                                                idx,
                                            ));
                                            // println!("fixing {:?}", idx);
                                            let f_xf = self.fix_bdd(f, idx, false);
                                            let g_xf = self.fix_bdd(g, idx, false);
                                            let h_xf = self.fix_bdd(h, idx, false);
                                            self.control_stack.push(ControlElement::Ite(
                                                Ite(f_xf, g_xf, h_xf),
                                            ));
                                            let f_xt = self.fix_bdd(f, idx, true);
                                            let g_xt = self.fix_bdd(g, idx, true);
                                            let h_xt = self.fix_bdd(h, idx, true);
                                            self.control_stack.push(ControlElement::Ite(
                                                Ite(f_xt, g_xt, h_xt),
                                            ));
                                        }
                                    }
                                }
                            }

                        }
                    }
                }
                None => return self.data_stack.pop().unwrap(),
            }
        }
    }



    /// evaluates the top element of the data stack on the values found in
    /// `vars`
    pub fn eval_bdd(&self, bdd: BddPtr, assgn: &HashMap<VarLabel, bool>) -> bool {
        fn eval_bdd_helper(man: &BddManager, ptr: BddPtr, assgn: &HashMap<VarLabel, bool>) -> bool {
            if ptr.is_true() {
                return true;
            } else if ptr.is_false() {
                return false;
            }
            let bdd = man.deref(ptr);
            let r = match bdd {
                Bdd::BddTrue => true,
                Bdd::BddFalse => false,
                Bdd::Node(n) => {
                    let value = assgn.get(&n.var).unwrap();
                    if *value {
                        eval_bdd_helper(man, n.high, assgn)
                    } else {
                        eval_bdd_helper(man, n.low, assgn)
                    }
                }
            };
            if ptr.is_compl() { !r } else { r }
        }
        eval_bdd_helper(self, bdd, assgn)
    }

    /// Returns true if `a` == `b`
    pub fn eq_bdd(&self, a: BddPtr, b: BddPtr) -> bool {
        // the magic of BDDs!
        a == b
    }

    pub fn get_apply_cache_stats(&self) -> BddCacheStats {
        self.apply_table.get_stats()
    }

    pub fn num_nodes(&self) -> usize {
        self.compute_table.num_nodes()
    }

    fn count_nodes_h(&self, ptr: BddPtr, set: &mut HashSet<BddPtr>) -> usize {
        if set.contains(&ptr) || ptr.is_const() {
            return 0;
        }
        set.insert(ptr);
        match ptr.ptr_type() {
            PointerType::PtrFalse => 1,
            PointerType::PtrTrue => 1,
            PointerType::PtrNode => {
                let n = self.deref(ptr).into_node();
                let c_l = self.count_nodes_h(n.low, set);
                let c_r = self.count_nodes_h(n.high, set);
                return c_l + c_r + 1
            }
        }
    }

    pub fn count_nodes(&self, ptr: BddPtr) -> usize {
        self.count_nodes_h(ptr, &mut HashSet::new())
    }
}

// check that (a \/ b) /\ a === a
#[test]
fn simple_equality() {
    let mut man = BddManager::new_default_order(3);
    let v1 = man.var(VarLabel::new(0), true);
    let v2 = man.var(VarLabel::new(1), true);
    let r1 = man.apply(Op::BddOr, v1, v2);
    let r2 = man.apply(Op::BddAnd, r1, v1);
    assert!(
        man.eq_bdd(v1, r2),
        "Not eq:\n {}\n{}",
        man.print_bdd(v1),
        man.print_bdd(r2)
    );
    println!("Not eq:\n {}\n{}", man.print_bdd(r1), man.print_bdd(r2));
}
