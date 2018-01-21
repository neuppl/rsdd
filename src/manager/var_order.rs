use repr::bdd::*;
use repr::var_label::VarLabel;
use std::slice::Iter;
use util;

pub struct VarOrder {
    /// an associative array, each index corresponds to a variable. I.e., the
    /// position of variable i in the order is given by the value of the array at
    /// index i
    var_to_pos: Vec<usize>,
    /// The inverse of `var_to_pos`, each index `i` corresponds to a label
    pos_to_var: Vec<usize>
}

impl VarOrder {
    pub fn new(order: Vec<VarLabel>) -> VarOrder {
        let mut v = util::malloc_vec(order.len());
        let mut pos_to_var = Vec::new();
        // println!("order: {:?}", order);
        for i in 0..order.len() {
            v[order[i].value() as usize] = i;
            pos_to_var.push(order[i].value() as usize);
        }
        VarOrder { var_to_pos: v, pos_to_var: pos_to_var}
    }

    /// Generate a linear variable ordering
    pub fn linear_order(num_var_to_pos: usize) -> VarOrder {
        let mut v = Vec::new();
        for i in 0..num_var_to_pos {
            v.push(VarLabel::new(i as u64))
        }
        VarOrder::new(v)
    }

    pub fn len(&self) -> usize {
        self.var_to_pos.len()
    }

    /// get the position of `var` in the order
    pub fn get(&self, var: VarLabel) -> usize {
        self.var_to_pos[var.value() as usize]
    }

    /// true if `a` is before `b` in this ordering
    pub fn lt(&self, a: VarLabel, b: VarLabel) -> bool {
        self.var_to_pos[a.value() as usize] < self.var_to_pos[b.value() as usize]
    }

    /// returns whichever BddPtr occurs first in a given ordering
    pub fn first(&self, a: BddPtr, b: BddPtr) -> BddPtr {
        if a.is_const() {
            b
        } else if b.is_const() {
            a
        } else {
            let pa = self.get(VarLabel::new(a.var()));
            let pb = self.get(VarLabel::new(b.var()));
            if pa < pb { a } else { b }
        }
    }

    pub fn sort(&self, a: BddPtr, b: BddPtr) -> (BddPtr, BddPtr) {
        let pa = self.get(VarLabel::new(a.var()));
        let pb = self.get(VarLabel::new(b.var()));
        if pa < pb { (a, b) } else { (b, a) }
    }

    pub fn order_iter(&self) -> Iter<usize> {
        self.var_to_pos.iter()
    }

    /// Get the variable that appears above `a` in the current order
    pub fn above(&self, a: VarLabel) -> VarLabel {
        let this_level = self.var_to_pos[a.value() as usize];
        VarLabel::new(self.pos_to_var[this_level - 1] as u64)
    }

    /// get the first essential variable (i.e., the variable that comes first in
    /// the order) among `a`, `b`, `c`
    pub fn first_essential(&self, a: BddPtr, b: BddPtr, c: BddPtr) -> VarLabel {
        let f1 = self.first(a, b);
        let f2 = self.first(f1, c);
        f2.label()
    }

    pub fn get_vec(&self) -> Vec<usize> {
        self.var_to_pos.clone()
    }

    pub fn last_var(&self) -> VarLabel {
        VarLabel::new(*self.pos_to_var.last().unwrap() as u64)
    }
}

#[test]
fn var_order_basics() {
    let order = VarOrder::linear_order(10);
    let lbl1 = VarLabel::new(4);
    let lbl2 = VarLabel::new(5);
    assert_eq!(order.lt(lbl1, lbl2), true);
    assert_eq!(order.lt(lbl2, lbl1), false);
    assert_eq!(order.above(lbl2), lbl1);
}
