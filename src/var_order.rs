use bdd::*;
use std::slice::Iter;

pub struct VarOrder {
    /// an associative array, each index corresponds to a variable. I.e., the
    /// position of variable i in the order is given by the value of the array at
    /// index i
    vars: Vec<usize>,
}

impl VarOrder {
    pub fn new(order: Vec<VarLabel>) -> VarOrder {
        let mut v = Vec::new();
        for i in 0..order.len() {
            v.push(order[i].value() as usize)
        }
        VarOrder { vars: v }
    }

    /// Generate a linear variable ordering
    pub fn linear_order(num_vars: usize) -> VarOrder {
        let mut v = Vec::new();
        for i in 0..num_vars {
            v.push(i)
        }
        VarOrder { vars: v }
    }

    pub fn len(&self) -> usize {
        self.vars.len()
    }

    /// get the position of `var` in the order
    pub fn get(&self, var: VarLabel) -> usize {
        self.vars[var.value() as usize]
    }

    /// true if `a` is before `b` in this ordering
    pub fn lt(&self, a: VarLabel, b: VarLabel) -> bool {
        self.vars[a.value() as usize] < self.vars[b.value() as usize]
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
        self.vars.iter()
    }

    /// get the first essential variable (i.e., the variable that comes first in
    /// the order) among `a`, `b`, `c`
    pub fn first_essential(&self, a: BddPtr, b: BddPtr, c: BddPtr) -> VarLabel {
        let f1 = self.first(a, b);
        let f2 = self.first(f1, c);
        f2.label()
    }
}
