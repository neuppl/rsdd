use bdd_table::BddTable;
use var_order::VarOrder;
use bdd::*;
use std::collections::HashMap;
use std::slice;

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
enum ApplicationElement {
    /// during expansion, these contain pointers into the frontier
    UnexpandedPair(BddPtr, BddPtr),
    /// these contain pointers to actual BDDs and can be dereferenced
    ExpandedPair(BddPtr, BddPtr),
    /// a successfully completed application
    Result(BddPtr)
}

/// Handles the state for a breadth-first BDD application
struct BfsApplication {
    state: Vec<Vec<ApplicationElement>>
}

/// Implements a breadth-first BDD application
/// Proceeds in two phases: expansion and contraction. Expansion explores the
/// BFS search tree and inserts each node into `state` for a particular
/// variable. Then, contraction uses the compute table to ensure each expanded
/// variable is unique.
/// 
impl BfsApplication {
    fn new(num_vars: usize) -> BfsApplication {
        let mut state = Vec::with_capacity(num_vars);
        for i in 0..num_vars {
            state.push(Vec::new());
        }
        BfsApplication {state: state}
    }

    // helper attempts to apply two BDDs
    fn single_expand(&mut self, op: Op, a: BddPtr, b: BddPtr) -> ApplicationElement {
        match (op, a.ptr_type(), b.ptr_type()) {
            (BddAnd, PtrTrue, _) => Result(b),
            (BddAnd, _, PtrTrue) => Result(a),
        }
    }

    /// Helper method for `expand`. Each element of the frontier is an UnexpandedPair
    fn expand_pair(&mut self, man: &BddManager, op: Op,
                   low: BddPtr, high: BddPtr) -> ApplicationElement {
        use self::ApplicationElement::*;
        use bdd::Op::*;
        use bdd::PointerType::*;

        if low.var() == high.var() {
            let low_lptr = man.low(low);
            let low_hptr = man.high(low);
            let high_lptr = man.low(high);
            let high_hptr = man.high(high);
            // apply low_lptr and high_lptr
        }
        panic!("oh no")
    }

    /// generate an expanded application
    fn expand(&mut self, man: &mut BddManager,
              op: Op, low: BddPtr, high: BddPtr) -> () {
        // insert the initial element
        use self::ApplicationElement::*;
        let order = man.get_order();
        let first = order.first(low, high);
        self.state[first.var() as usize].push(UnexpandedPair(low, high));
        // iterate through each variable in the ordering, expanding them one at
        // a time
        for i in order.order_iter() {
            // the borrow checker here falls on its face (we are manipulating
            // a vector of vectors, which is quite tricky), so we drop into
            // unsafe.
            let mut cur_row = unsafe {
                // make an un-checked alias for the current row to iterate over
                slice::from_raw_parts_mut(
                    self.state[*i].as_mut_ptr(),
                    self.state[*i].len()
                )
            };
            // for each unexpanded node in the current variable
            for elem in cur_row.iter_mut() {
                match elem {
                    &mut UnexpandedPair(low, high) => {
                        let apply_op = man.get_application(&ApplyOp(op.clone(), low, high));
                        if apply_op.is_some() {
                            *elem = Result(apply_op.unwrap());
                            continue;
                        }
                        *elem = self.expand_pair(man, op.clone(), low, high);
                        // generate the resulting sub-applications
                    },
                    &mut ExpandedPair(_, _) => {} // do nothing for expanded pairs
                    _ => panic!("invalid expand state")
                }
            }
        }
    }
}


#[derive(Debug, Hash, Eq, PartialEq, Clone)]
struct ApplyOp (Op, BddPtr, BddPtr);

struct BddManager {
    compute_table: BddTable,
    apply_table: HashMap<ApplyOp, BddPtr>,
}

impl BddManager {
    /// Make a BDD manager with a default variable ordering
    pub fn new_default_order(num_vars: usize) -> BddManager {
        let default_order = VarOrder::linear_order(num_vars);
        BddManager {
            compute_table: BddTable::new(default_order),
            apply_table: HashMap::with_capacity(10000),
        }
    }

    pub fn get_order(&self) -> &VarOrder {
        self.compute_table.order()
    }

    pub fn deref(&self, ptr: BddPtr) -> Bdd {
        self.compute_table.deref(ptr)
    }

    pub fn get_application(&self, app: &ApplyOp) -> Option<BddPtr> {
        match self.apply_table.get(app) {
            Some(r) => Some(r.clone()),
            None => None
        }
    }

    /// Fetch the BDD pointed to by the low-node of `ptr`, panics on constant
    // BDDs
    pub fn low(&self, ptr: BddPtr) -> BddPtr {
        let b = self.deref(ptr);
        match b {
            Bdd::BddNode{low, high: _, var: _} => low,
            _ => panic!("fetching low node of const bdd")
        }
    }

    /// Fetch the BDD pointed to by the high-node of `ptr`, panics on constant
    /// BDDs
    pub fn high(&self, ptr: BddPtr) -> BddPtr {
        let b = self.deref(ptr);
        match b {
            Bdd::BddNode{low: _, high, var: _} => high,
            _ => panic!("fetching high node of const bdd")
        }
    }



    /// Generate a pointer to a BDD representing a variable
    pub fn var(&mut self, lbl: VarLabel, is_true: bool) -> BddPtr {
        let bdd = if is_true {
            Bdd::new_node(BddPtr::false_node(), BddPtr::true_node(), lbl)
        } else {
            Bdd::new_node(BddPtr::false_node(), BddPtr::true_node(), lbl)
        };
        self.compute_table.get_or_insert(bdd)
    }

    // pub fn apply(&mut self, op: Op, l: BddPtr, r: BddPtr) -> BddPtr {}
    // {
    //     let mut control: Vec<Control> = Vec::with_capacity(1000);
    //     let mut data: Vec<BddPtr> = Vec::with_capacity(1000);
    //     control.push(Control::Apply(ApplyOp(op, l, r)));
    //     // loop until control stack is empty
    //     loop {
    //         let t = control.pop();
    //         match t {
    //             None => {
    //                 // by default, if there is nothing left on the control
    //                 // stack, return the last remaining element on the data
    //                 // stack
    //                 match data.pop() {
    //                     None => panic!("invalid state: data stack empty on return"),
    //                     Some(a) => return a,
    //                 }
    //             }
    //             Some(Control::Apply(a)) => {
    //                 // check to see if this application has already been
    //                 // computed; if it has, push it onto the data stack
    //                 let r = self.apply_table.get(&a).clone();
    //                 if r.is_some() {
    //                     data.push(r.unwrap().clone());
    //                     continue;
    //                 }
    //                 // process the application
    //                 let ApplyOp(o, l, r) = a;
    //                 use bdd::PointerType::*;
    //                 use bdd::Op::*;
    //                 match (o, l.ptr_type(), r.ptr_type()) {
    //                     (BddAnd, PtrTrue, PtrTrue) => data.push(BddPtr::true_node()),
    //                     (BddAnd, _, PtrFalse) |
    //                     (BddAnd, PtrFalse, _) => data.push(BddPtr::false_node()),
    //                     (BddAnd, PtrTrue, PtrNode) => data.push(r.clone()),
    //                     (BddAnd, PtrNode, PtrTrue) => data.push(l.clone()),
    //                     (BddAnd, PtrNode, PtrNode) => {
    //                         let l_var = VarLabel::new(l.var());
    //                         let r_var = VarLabel::new(r.var());
    //                         if l_var == r_var {
    //                             control.push(Control::Bind(l_var));
    //                             let l_bdd = self.compute_table.deref(l);
    //                             let r_bdd = self.compute_table.deref(r);
    //                             control.
    //                         }
    //                     }
    //                     (BddOr, _, _) => panic!("not implemented"),
    //                 }
    //             }
    //             Some(Control::Bind(var)) => {
    //                 let l_op = data.pop();
    //                 let r_op = data.pop();
    //                 let (l, r) = match (l_op, r_op) {
    //                     (Some(l), Some(r)) => (l, r),
    //                     _ => panic!("Invalid state: binding with empty frontier"),
    //                 };
    //                 let n = self.compute_table.get_or_insert(Bdd::new_node(l, r, var));
    //                 data.push(n)
    //             }
    //             Some(Control::ApplyCache(app)) => {
    //                 self.apply_table.insert(app, data.last().unwrap().clone());
    //             }
    //         }
    //     }
    // }
}
