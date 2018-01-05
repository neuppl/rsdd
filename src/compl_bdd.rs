extern crate fnv;
use std::collections::HashMap;
use std::ops::{BitAnd, BitOr};
use bdd::*;


/// Core BDD representation
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Bdd {
    /// the variable label of the current node
    /// index 0 is reserved for true
    index: BddIndex,
    /// pointer to low node
    low: BddRef,
    /// pointer to high node
    high: BddRef,
}

fn string_of_bdd(b: BddRef) -> String {
    fn string_of_bdd_h(b: BddRef) -> String {
        let bdd = unsafe { b.deref() };
        let low = bdd.low;
        let high = bdd.high;
        if low.is_null() || high.is_null() {
            return String::from("T");
        }
        assert!(!high.is_compl());
        let low_s = string_of_bdd_h(low.clone());
        let high_s = string_of_bdd_h(high.clone());
        let idx = bdd.index;
        format!(
            "{:?} ({}) {}({})",
            idx,
            high_s,
            if low.is_compl() { "!" } else { "" },
            low_s
        )
    }
    let s = string_of_bdd_h(b.clone());
    format!(
        "{}{}{}",
        if b.is_compl() { "!(" } else { "" },
        s,
        if b.is_compl() { ")" } else { "" }
    )
}

fn eval_bdd(b: BddRef, r: &HashMap<BddIndex, bool>) -> bool {
    fn eval_bdd_h(b: BddRef, r: &HashMap<BddIndex, bool>) -> bool {
        let bdd = unsafe { b.deref() };
        if bdd.low.is_null() || bdd.high.is_null() {
            // if it is the true node
            true
        } else {
            let dir = r.get(&bdd.index).unwrap();
            let new_r = if *dir { bdd.high } else { bdd.low };
            let new_v = eval_bdd_h(new_r.clone(), r);
            if new_r.is_compl() { !new_v } else { new_v }
        }
    }
    let ret = eval_bdd_h(b.clone(), r);
    if b.is_compl() { !ret } else { ret }
}

/// tracks the ordering of a BDD
pub struct Order {
    pub order: Vec<usize>,
}

/// Pass in the order that the variables are in (0 index === position of first
/// variable, etc.)
pub fn new_order(o: Vec<usize>) -> Order {
    Order { order: o }
}

impl Order {
    /// true if a is less than b in the current ordering, with ties given to b
    pub fn order_lt(&self, a: BddIndex, b: BddIndex) -> bool {
        if (a == 0 && b == 0) || (a == 0) {
            return false;
        };
        if b == 0 {
            return true;
        };
        let a_order = self.order[a - 1]; // subtract 1 since BDDs are 1-indexed
        let b_order = self.order[b - 1]; // subtract 1 since BDDs are 1-indexed
        a_order < b_order
    }


    /// index of the first essential variable among 3 bdd references
    pub fn first_essential(&self, a: &BddRef, b: &BddRef, c: &BddRef) -> BddIndex {
        let a_idx = unsafe { a.index() };
        let b_idx = unsafe { b.index() };
        let c_idx = unsafe { c.index() };
        // lowest between a and b
        let low = if self.order_lt(a_idx, b_idx) {
            a_idx
        } else {
            b_idx
        };
        if self.order_lt(low, c_idx) {
            low
        } else {
            c_idx
        }
    }

    pub fn num_vars(&self) -> usize {
        self.order.len()
    }
}

pub enum PubBddControl {
    PushVar(BddIndex, bool),
    /// Performs an ITE operation on the top 3 elements of the stack
    /// Ite(f, g, h) where f is the top element, g is second, h is third
    Ite,
    And,
    Or,
}

pub mod manager {
    use std;
    use bdd::fnv::FnvHashMap;
    use bdd::*;

    /// store a vector of stores which grows quadratically
    #[derive(Clone, Debug, Hash, PartialEq)]
    struct Store {
        store: Vec<Vec<Bdd>>,
        realloc_sz: usize,
    }

    fn new_store() -> Store {
        let mut s = Store {
            store: Vec::new(),
            realloc_sz: 100000,
        };
        s.store.push(Vec::with_capacity(100000));
        s
    }

    struct Allocator {
        ite_cache: FnvHashMap<Ite, BddRef>,
        gen_cache: Vec<FnvHashMap<(BddRef, BddRef), *const Bdd>>,
        var_cache: FnvHashMap<BddIndex, *const Bdd>,
        store: Store,
        true_node: BddRef,
    }

    fn new_allocator(num_vars: usize) -> Allocator {
        let null = BddRef::new(std::ptr::null(), true);
        let mut a = Allocator {
            store: new_store(),
            ite_cache: FnvHashMap::with_capacity_and_hasher(10000, Default::default()),
            gen_cache: Vec::new(),
            var_cache: FnvHashMap::with_capacity_and_hasher(10000, Default::default()),
            true_node: null.clone(),
        };
        for _ in 0..num_vars {
            a.gen_cache.push(FnvHashMap::with_capacity_and_hasher(
                10000,
                Default::default(),
            ));
        }
        let ptr = a.store.alloc_node(Bdd {
            index: 0,
            low: null.clone(),
            high: null,
        });
        a.true_node = BddRef::new(ptr, true);
        a
    }

    impl Allocator {
        pub fn true_node(&self) -> BddRef {
            self.true_node.clone()
        }
        pub fn false_node(&self) -> BddRef {
            self.true_node.neg()
        }

        pub fn mk_var(&mut self, index: BddIndex, compl: bool) -> BddRef {
            let t_node = self.true_node();
            let f_node = self.false_node();
            let st = &mut self.store;
            let r = self.var_cache.entry(index).or_insert_with(|| {
                st.alloc_node(Bdd {
                    index: index,
                    high: t_node,
                    low: f_node,
                })
            });
            BddRef::new(r.clone(), compl)
        }

        /// True if the BDD is a variable node (i.e., both of its children are constant nodes)
        pub fn is_var(&self, bdd: &BddRef) -> bool {
            let cur = unsafe { bdd.deref() };
            let true_ref = self.true_node.extract_ptr();
            let low_ref = cur.low.extract_ptr();
            let high_ref = cur.high.extract_ptr();
            (low_ref == true_ref) && (high_ref == true_ref)
        }

        /// true if bdd is a variable and the variable's high entry is true
        pub fn is_true_var(&self, bdd: &BddRef) -> bool {
            if self.is_var(&bdd) {
                let b = unsafe { bdd.deref() };
                // if the high edge is complemented, then it is false
                !b.high.is_compl()
            } else {
                false
            }
        }

        /// creates a BDD node and returns an un-complemented BddRef
        fn mk_node(&mut self, b: Bdd) -> BddRef {
            let st = &mut self.store;
            let saved = b.clone();
            let r = *self.gen_cache[b.index - 1]
                .entry((b.high, b.low))
                .or_insert_with(|| st.alloc_node(saved));
            BddRef::new(r, true)
        }

        /// creates a new BDD with the desired high and low edges
        /// must canonicalize by ensuring that the high edge is never complemented
        fn mk_bdd(&mut self, idx: BddIndex, high: BddRef, low: BddRef) -> BddRef {
            fn new_bdd(idx: BddIndex, high: BddRef, low: BddRef) -> Bdd {
                Bdd {
                    index: idx,
                    high: high,
                    low: low,
                }
            }
            match (high, low) {
                (ref high, ref low) if high.is_compl() => {
                    let b = new_bdd(idx, high.neg(), low.neg());
                    let r = self.mk_node(b);
                    r.neg()
                }
                (ref high, ref low) => {
                    let b = new_bdd(idx, high.clone(), low.clone());
                    self.mk_node(b)
                }
            }
        }
    }

    impl Store {
        pub fn alloc_node(&mut self, bdd: Bdd) -> *const Bdd {
            let is_eq = self.store.last().unwrap().len() == self.store.last().unwrap().capacity();
            if is_eq {
                self.store.push(Vec::with_capacity(self.realloc_sz))
            }
            let cur_store = self.store.last_mut().unwrap();
            cur_store.push(bdd);
            match cur_store.last() {
                None => unreachable!(),
                Some(a) => a as *const Bdd,
            }
        }

        pub fn alloc_true_node(&mut self, bdd: Bdd) -> BddRef {
            let r = self.alloc_node(bdd);
            BddRef::new(r, true)
        }
    }


    pub struct Manager {
        alloc: Allocator,
        order: Order,
        control_stack: Vec<BddControl>,
        data_stack: Vec<BddData>,
    }

    pub fn new(order: Order) -> Manager {
        Manager {
            alloc: new_allocator(order.num_vars()),
            order: order,
            control_stack: Vec::new(),
            data_stack: Vec::new(),
        }
    }

    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    enum IteValue {
        Bdd(BddRef),
        True,
        False,
    }

    /// Represent a standard ITE manipulation Ite(f, g, h)
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    struct Ite(BddRef, BddRef, BddRef);

    fn mk_ite(f: &BddRef, g: &BddRef, h: &BddRef) -> Ite {
        // cloning pointers is just a stack copy so this is very cheap
        Ite(f.clone(), g.clone(), h.clone())
    }

    impl Ite {
        /// standardize an ITE into a consistent representation. See blue book p. 115
        fn standardize(&self, man: &Manager) -> Ite {
            // first, transform Ite's into constants when possible
            let phase1 = match *self {
                // first check for any base cases, which we do not want to process further
                Ite(ref f, ref g, ref h)
                    if *f == man.alloc.true_node() ||
                           (*g == man.alloc.true_node() && *h == man.alloc.false_node()) ||
                           (*g == man.alloc.false_node() && *h == man.alloc.true_node()) ||
                           g == h => return self.clone(),
                // negate this base case
                Ite(ref f, ref g, ref h) if *f == man.alloc.false_node() => {
                    return mk_ite(&f.neg(), h, g)
                }
                // now transform constants
                Ite(ref f, ref g, ref h) if g == h => mk_ite(f, &man.alloc.true_node(), h),
                Ite(ref f, ref g, ref h) if f == h => mk_ite(f, g, &man.alloc.false_node()),
                Ite(ref f, ref g, ref h) if (h.neg() == *f) => mk_ite(f, g, &man.alloc.true_node()),
                Ite(ref f, ref g, ref h) if (f.neg() == *g) => {
                    mk_ite(f, &man.alloc.false_node(), h)
                }
                _ => self.clone(),
            };
            // then, re-order bdds based on their precedence in the variable order
            let phase2 = match phase1 {
                Ite(ref f, ref g, ref h) if *g == man.alloc.true_node() => {
                    if unsafe { man.order.order_lt(f.index(), h.index()) } {
                        mk_ite(f, g, h)
                    } else {
                        mk_ite(h, g, f)
                    }
                }
                Ite(ref f, ref g, ref h) if *h == man.alloc.false_node() => {
                    if unsafe { man.order.order_lt(f.index(), g.index()) } {
                        mk_ite(f, g, h)
                    } else {
                        mk_ite(g, f, h)
                    }
                }
                Ite(ref f, ref g, ref h) if *h == man.alloc.true_node() => {
                    if unsafe { man.order.order_lt(f.index(), g.index()) } {
                        mk_ite(f, g, h)
                    } else {
                        mk_ite(&g.neg(), &f.neg(), h)
                    }
                }
                Ite(ref f, ref g, ref h) if *g == man.alloc.false_node() => {
                    if unsafe { man.order.order_lt(f.index(), h.index()) } {
                        mk_ite(f, g, h)
                    } else {
                        mk_ite(&h.neg(), g, &f.neg())
                    }
                }
                Ite(ref f, ref g, ref h) if *g == h.neg() => {
                    if unsafe { man.order.order_lt(f.index(), g.index()) } {
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

        fn to_string(&self) -> String {
            let Ite(ref f, ref g, ref h) = *self;
            format!(
                "ITE({}, {}, {})",
                string_of_bdd(f.clone()),
                string_of_bdd(g.clone()),
                string_of_bdd(h.clone())
            )
        }
    }

    enum BddControl {
        /// Generates an ITE and pushes it onto the stack
        ApplyIte(Ite),
        /// Inserts an ITE into the ITE cache
        IteCache(Ite),
        /// Binds the two top elements of the data stack into a single BDD node;
        /// top = low, second = high.
        /// Pushes the resulting BDD onto the data stack with polarity as the first argument
        Bind(bool, BddIndex),
    }

    impl BddControl {
        fn to_string(&self) -> String {
            match &self {
                &&BddControl::ApplyIte(ref ite) => ite.to_string(),
                &&BddControl::IteCache(ref ite) => format!("Caching {}", ite.to_string()),
                &&BddControl::Bind(polarity, idx) => format!("Bind {} {}", polarity, idx),
            }
        }
    }

    enum BddData {
        Bdd(BddRef),
    }

    impl BddData {
        pub fn to_string(&self) -> String {
            match &self {
                &&BddData::Bdd(ref r) => string_of_bdd(r.clone()),
            }
        }
    }

    fn print_stack(st: &Vec<BddData>) -> String {
        let mut s = String::new();
        for i in st.into_iter() {
            s.push_str(&i.to_string());
            s.push('\n');
        }
        s
    }

    impl Manager {
        pub fn process(&mut self) {
            while self.control_stack.len() > 0 {
                let t = self.control_stack.pop().unwrap();
                // println!("----------\nProcessing {}\nStack:{}", t.to_string(), print_stack(&self.data_stack));
                match t {
                    BddControl::ApplyIte(i) => self.handle_ite(i),
                    BddControl::Bind(polarity, idx) => self.handle_bind(polarity, idx),
                    BddControl::IteCache(ite) => {
                        let t = match self.data_stack.pop() {
                            None => panic!("empty stack"),
                            Some(BddData::Bdd(a)) => a,
                        };
                        self.alloc.ite_cache.insert(ite, t.clone());
                        self.data_stack.push(BddData::Bdd(t));
                    }
                }
            }
        }

        fn handle_bind(&mut self, polarity: bool, idx: BddIndex) -> () {
            let low = self.data_stack.pop();
            let high = self.data_stack.pop();
            match (low, high) {
                (None, _) | (_, None) => panic!("popping on empty stack"),
                (Some(BddData::Bdd(ref low_r)), Some(BddData::Bdd(ref high_r)))
                    if low_r == high_r => self.data_stack.push(BddData::Bdd(low_r.clone())),
                (Some(BddData::Bdd(low_r)), Some(BddData::Bdd(high_r))) => {
                    let new_bdd = self.alloc.mk_bdd(idx, high_r, low_r);
                    let compl = if !polarity { new_bdd.neg() } else { new_bdd };
                    self.data_stack.push(BddData::Bdd(compl))
                }
            }
        }

        fn handle_ite(&mut self, i: Ite) -> () {
            // first standardize it
            let standard = i.standardize(self);
            // println!("Standardized: {}", standard.to_string());
            // check if it is a base case; is Some(BddRef) if a base-case is encountered
            let v = match standard {
                // first check for any constants
                Ite(ref f, ref g, _) if *f == self.alloc.true_node() => Some(g.clone()),
                Ite(ref f, _, ref h) if *f == self.alloc.false_node() => Some(h.clone()),
                Ite(ref f, ref g, ref h)
                    if *g == self.alloc.true_node() && *h == self.alloc.false_node() => Some(
                    f.clone(),
                ),
                Ite(ref f, ref g, ref h)
                    if *g == self.alloc.false_node() && *h == self.alloc.true_node() => Some(
                    f.neg(),
                ),
                Ite(_, ref g, ref h) if g == h => Some(g.clone()),
                Ite(ref f, _, ref h) if *f == self.alloc.false_node() => Some(h.clone()),
                // then check for equalities
                Ite(ref f, ref g, ref h)
                    if *g == self.alloc.true_node() && *h == self.alloc.false_node() => Some(
                    f.clone(),
                ),
                Ite(_, ref g, ref h) if g == h => Some(g.clone()),
                // because we standardized by sorting on the left-most node, if
                // f = x_i (i.e., a single variable) and x_i is located before
                // the first variables of g and h in the ordering, then we can
                // simplify by producing the BDD (x_i, g, h)
                Ite(ref f, ref g, ref h) if self.alloc.is_var(f) => {
                    // check that the variable in f occurs first
                    let f_idx = unsafe { f.index() };
                    let g_idx = unsafe { g.index() };
                    let h_idx = unsafe { h.index() };
                    if self.order.order_lt(f_idx, g_idx) && self.order.order_lt(f_idx, h_idx) {
                        // standardization guarantees that f will never be complemented
                        let bdd = self.alloc.mk_bdd(f_idx, g.clone(), h.clone());
                        Some(bdd)
                    } else {
                        None
                    }
                }
                _ => None,
            };
            match v {
                Some(a) => self.data_stack.push(BddData::Bdd(a)),
                None => {
                    match self.alloc.ite_cache.get(&standard) {
                        Some(a) => {
                            // push an uncomplemented BDD onto the stack
                            self.data_stack.push(BddData::Bdd(a.clone()))
                        }
                        None => {
                            // the ite is not in the cache and it is not a base case
                            self.control_stack.push(
                                BddControl::IteCache(standard.clone()),
                            );
                            let Ite(f, g, h) = standard;
                            let idx = self.order.first_essential(&f, &g, &h);
                            self.control_stack.push(BddControl::Bind(f.polarity(), idx));
                            let f_xf = f.fix_top(idx, false);
                            let g_xf = g.fix_top(idx, false);
                            let h_xf = h.fix_top(idx, false);
                            self.control_stack.push(BddControl::ApplyIte(
                                Ite(f_xf, g_xf, h_xf),
                            ));
                            let f_xt = f.fix_top(idx, true);
                            let g_xt = g.fix_top(idx, true);
                            let h_xt = h.fix_top(idx, true);
                            self.control_stack.push(BddControl::ApplyIte(
                                Ite(f_xt, g_xt, h_xt),
                            ));
                        }
                    }
                }
            }
        }

        pub fn run(&mut self, control: PubBddControl) -> () {
            match control {
                PubBddControl::PushVar(idx, compl) => {
                    let v = self.alloc.mk_var(idx, compl);
                    self.data_stack.push(BddData::Bdd(v))
                }
                PubBddControl::Ite => {
                    let f = self.data_stack.pop();
                    let g = self.data_stack.pop();
                    let h = self.data_stack.pop();
                    match (f, g, h) {
                        (Some(BddData::Bdd(f)), Some(BddData::Bdd(g)), Some(BddData::Bdd(h))) => {
                            self.control_stack.push(BddControl::ApplyIte(Ite(f, g, h)));
                        }
                        _ => panic!("invalid stack state"),
                    };
                    self.process()
                }
                PubBddControl::And => {
                    let f = self.data_stack.pop();
                    let g = self.data_stack.pop();
                    match (f, g) {
                        (Some(BddData::Bdd(f)), Some(BddData::Bdd(g))) => {
                            self.control_stack.push(BddControl::ApplyIte(
                                Ite(f, g, self.alloc.false_node()),
                            ));
                        }
                        _ => panic!("invalid stack state"),
                    };
                    self.process()
                }
                PubBddControl::Or => {
                    let f = self.data_stack.pop();
                    let g = self.data_stack.pop();
                    match (f, g) {
                        (Some(BddData::Bdd(f)), Some(BddData::Bdd(g))) => {
                            self.control_stack.push(BddControl::ApplyIte(
                                Ite(f, self.alloc.true_node(), g),
                            ));
                        }
                        _ => panic!("invalid stack state"),
                    };
                    self.process()

                }
            }
        }

        /// converts the top of the stack into a string
        pub fn print_bdd(&self) -> String {
            let s = self.data_stack.last().unwrap();
            match s {
                &BddData::Bdd(ref r) => string_of_bdd(r.clone()),
            }
        }

        /// Evaluates the top BDD of the stack with the provided value hashmap
        pub fn eval_bdd(&self, v: &HashMap<BddIndex, bool>) -> bool {
            let s = self.data_stack.last().unwrap();
            match s {
                &BddData::Bdd(ref r) => eval_bdd(r.clone(), &v),
            }
        }

        /// Tests if the top two elements of the stack are equal
        pub fn eq_bdd(&self) -> bool {
            let a = match self.data_stack.last() {
                Some(&BddData::Bdd(ref res)) => res.clone(),
                None => panic!("empty stack"),
            };
            let b = match &self.data_stack[self.data_stack.len() - 2] {
                &BddData::Bdd(ref r) => r,
            };
            a == *b
        }
    }
}
