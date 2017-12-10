use bdd_table::BddTable;
use var_order::VarOrder;
use bdd::*;
use std::collections::{HashMap, HashSet};
use std::slice;
use apply_cache::{ApplyOp, ApplyTable};
use ref_table::*;

#[derive(Debug)]
enum ControlElement {
    App(ApplyOp),
    Bind(VarLabel),
    ApplyCache(ApplyOp),
}

pub struct BddManager {
    compute_table: BddTable,
    apply_table: ApplyTable,
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
            apply_table: ApplyTable::new(len),
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

    fn get_application(&self, app: ApplyOp) -> Option<BddPtr> {
        match self.apply_table.get(app) {
            Some(r) => Some(r.clone()),
            None => None,
        }
    }

    fn insert_application(&mut self, app: ApplyOp, result: BddPtr) -> () {
        self.apply_table.insert(app, result);
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
        let bdd = if is_true {
            Bdd::new_node(BddPtr::false_node(), BddPtr::true_node(), lbl)
        } else {
            Bdd::new_node(BddPtr::true_node(), BddPtr::false_node(), lbl)
        };
        self.compute_table.get_or_insert(bdd)
    }

    pub fn true_ptr(&mut self) -> BddPtr {
        BddPtr::true_node()
    }

    pub fn false_ptr(&mut self) -> BddPtr {
        BddPtr::false_node()
    }

    fn get_or_insert(&mut self, bdd: Bdd) -> BddPtr {
        self.compute_table.get_or_insert(bdd)
    }

    pub fn print_bdd(&self, ptr: BddPtr ) -> String {
        use bdd::PointerType::*;
        fn print_bdd_helper(t: &BddManager, ptr: BddPtr) -> String {
            match ptr.ptr_type() {
                PtrTrue => String::from("T"),
                PtrFalse => String::from("F"),
                PtrNode => {
                    let l_p = t.low(ptr);
                    let h_p = t.high(ptr);
                    let l_s = print_bdd_helper(t, l_p);
                    let r_s = print_bdd_helper(t, h_p);
                    format!("({}, {}, {})", ptr.var(), l_s, r_s)
                }
            }
        }
        print_bdd_helper(self, ptr)
    }

    fn normalize(&self, a: BddPtr, b: BddPtr, op: Op) -> (VarLabel, ApplyOp, ApplyOp) {
        let a_bdd = self.deref(a).into_node();
        let b_bdd = self.deref(b).into_node();
        if a_bdd.var == b_bdd.var {
            (
                a_bdd.var,
                ApplyOp(op, a_bdd.low, b_bdd.low),
                ApplyOp(op, a_bdd.high, b_bdd.high),
            )
        } else {
            let pa = self.get_order().get(VarLabel::new(a.var()));
            let pb = self.get_order().get(VarLabel::new(b.var()));
            let (a_s, b_s) = if pa < pb { (a, b) } else { (b, a) };
            let first_node = self.deref(a_s).into_node();
            (
                VarLabel::new(a_s.var()),
                ApplyOp(op, first_node.low, b_s),
                ApplyOp(op, first_node.high, b_s),
            )
        }
    }


    /// pushes the resulting application onto the top of the data stack
    pub fn apply(&mut self, op: Op, a: BddPtr, b: BddPtr) -> BddPtr {
        use self::ControlElement::*;
        use bdd::Op::*;
        use bdd::PointerType::*;
        self.control_stack.push(App(ApplyOp(op, a, b)));
        loop {
            let top = self.control_stack.pop();
            match top {
                Some(top) => {
                    match top {
                        Bind(vlabel) => {
                            let high = self.data_stack.pop().unwrap();
                            let low = self.data_stack.pop().unwrap();
                            let bdd = Bdd::new_node(low, high, vlabel);
                            let bdd_r = self.get_or_insert(bdd);
                            self.data_stack.push(bdd_r);
                        }

                        ApplyCache(op) => {
                            let itm = self.data_stack.last().unwrap().clone();
                            self.insert_application(op, itm)
                        }
                        App(ApplyOp(o, a, b)) => {
                            match (o, a.ptr_type(), b.ptr_type()) {
                                (BddAnd, PtrTrue, _) => self.data_stack.push(b),
                                (BddAnd, _, PtrTrue) => self.data_stack.push(a),
                                (BddAnd, PtrFalse, _) |
                                (BddAnd, _, PtrFalse) => self.data_stack.push(BddPtr::false_node()),
                                (BddOr, PtrFalse, _) => self.data_stack.push(b),
                                (BddOr, _, PtrFalse) => self.data_stack.push(a),
                                (BddOr, PtrTrue, _) |
                                (BddOr, _, PtrTrue) => self.data_stack.push(BddPtr::true_node()),
                                (op, PtrNode, PtrNode) => {
                                    let (s_a, s_b) = self.get_order().sort(a, b);
                                    let cached = self.get_application(ApplyOp(o, s_a, s_b));
                                    if cached.is_some() {
                                        self.data_stack.push(cached.unwrap());
                                        continue;
                                    };
                                    self.control_stack.push(ApplyCache(ApplyOp(o, s_a, s_b)));
                                    let (lbl, low_app, high_app) = self.normalize(a, b, op);
                                    self.control_stack.push(Bind(lbl));
                                    self.control_stack.push(App(high_app));
                                    self.control_stack.push(App(low_app));
                                }
                            }
                        }
                    }
                }
                None => {
                    return self.data_stack.pop().unwrap()
                }
            }
        }
    }



    /// evaluates the top element of the data stack on the values found in
    /// `vars`
    pub fn eval_bdd(&self, bdd: BddPtr, assgn: &HashMap<VarLabel, bool>) -> bool {
        fn eval_bdd_helper(man: &BddManager, ptr: BddPtr, assgn: &HashMap<VarLabel, bool>) -> bool {
            let bdd = man.deref(ptr);
            match bdd {
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
            }
        }
        eval_bdd_helper(self, bdd, assgn)
    }

    /// Returns true if `a` == `b`
    pub fn eq_bdd(&self, a: BddPtr, b: BddPtr) -> bool {
        // the magic of BDDs!
        a == b
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
    assert!(man.eq_bdd(v1, r2));
}
