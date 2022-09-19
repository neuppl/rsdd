//! Stores the variable order for a the BDD manager Variables that occur first
//! in the order occur first in the BDD, starting from the root.
//! Lower numbers occur first in the order (i.e., closer to the root)
use crate::builder::repr::builder_bdd::*;
use crate::repr::var_label::VarLabel;
use crate::util;
use std::slice::Iter;

#[derive(Debug, Clone)]
pub struct VarOrder {
    /// an associative array, each index corresponds to a variable. I.e., the
    /// position of variable i in the order is given by the value of the array at
    /// index i
    var_to_pos: Vec<usize>,
    /// The inverse of `var_to_pos`, each index `i` corresponds to a label
    pos_to_var: Vec<usize>,
}

impl VarOrder {
    /// Creates a new variable order (elements that occur first in the vector
    /// occur first in the order)
    pub fn new(order: Vec<VarLabel>) -> VarOrder {
        let mut v = util::malloc_vec(order.len());
        let mut pos_to_var = Vec::new();
        for i in 0..order.len() {
            v[order[i].value() as usize] = i;
            pos_to_var.push(order[i].value() as usize);
        }
        VarOrder {
            var_to_pos: v,
            pos_to_var,
        }
    }

    /// Generate a linear variable ordering of size `num_var_to_pos`
    pub fn linear_order(num_var_to_pos: usize) -> VarOrder {
        let mut v = Vec::new();
        for i in 0..num_var_to_pos {
            v.push(VarLabel::new(i as u64))
        }
        VarOrder::new(v)
    }

    /// Gives the number of variables in the order
    /// ```
    /// # use rsdd::builder::var_order::VarOrder;
    /// let o = VarOrder::linear_order(10);
    /// assert_eq!(o.num_vars(), 10);
    /// ```
    pub fn num_vars(&self) -> usize {
        self.var_to_pos.len()
    }

    /// Get the position of `var` in the order
    pub fn get(&self, var: VarLabel) -> usize {
        self.var_to_pos[var.value() as usize]
    }

    /// Fetches the variable that it as the specified position in the order
    /// ```
    /// # use rsdd::builder::var_order::VarOrder;
    /// # use rsdd::repr::var_label::VarLabel;
    /// let o = VarOrder::linear_order(10);
    /// assert_eq!(o.var_at_level(4), VarLabel::new(4));
    /// ```
    pub fn var_at_level(&self, pos: usize) -> VarLabel {
        VarLabel::new(self.pos_to_var[pos] as u64)
    }

    /// True if `a` is before `b` in this ordering
    /// ```
    /// # use rsdd::builder::var_order::VarOrder;
    /// # use rsdd::repr::var_label::VarLabel;
    /// let o = VarOrder::linear_order(10);
    /// assert!(o.lt(VarLabel::new(3), VarLabel::new(4)));
    /// ```
    pub fn lt(&self, a: VarLabel, b: VarLabel) -> bool {
        self.var_to_pos[a.value() as usize] < self.var_to_pos[b.value() as usize]
    }

    /// True if `a` is before or equal to `b` in the ordering
    pub fn lte(&self, a: VarLabel, b: VarLabel) -> bool {
        self.var_to_pos[a.value() as usize] <= self.var_to_pos[b.value() as usize]
    }

    /// Returns the BddPtr whose top variable occurs first in a given
    /// ordering (ties broken by returning `a`)
    pub fn first(&self, a: BddPtr, b: BddPtr) -> BddPtr {
        if a.is_const() {
            b
        } else if b.is_const() {
            a
        } else {
            let pa = self.get(VarLabel::new(a.var()));
            let pb = self.get(VarLabel::new(b.var()));
            if pa < pb {
                a
            } else {
                b
            }
        }
    }

    /// Returns a sorted pair where the BddPtr whose top variable is first
    /// occurs first in the pair.
    pub fn sort(&self, a: BddPtr, b: BddPtr) -> (BddPtr, BddPtr) {
        if a.is_const() {
            (b, a)
        } else if b.is_const() {
            (a, b)
        } else {
            let pa = self.get(VarLabel::new(a.var()));
            let pb = self.get(VarLabel::new(b.var()));
            if pa < pb {
                (a, b)
            } else {
                (b, a)
            }
        }
    }

    /// Produces an iterator of var -> position, where the
    /// result[i] gives the position of variable i in the order
    pub fn order_iter(&self) -> Iter<usize> {
        self.var_to_pos.iter()
    }

    /// Iterate through the variables in the order in which they appear in the order
    pub fn in_order_iter<'a>(&'a self) -> impl Iterator<Item = VarLabel> + 'a {
        self.pos_to_var.iter().map(|x| VarLabel::new_usize(*x))
    }

    /// Iterate through the variables in the the reverse order in which they
    /// appear in the order
    pub fn reverse_in_order_iter<'a>(&'a self) -> impl Iterator<Item = VarLabel> + 'a {
        self.pos_to_var
            .iter()
            .map(|x| VarLabel::new_usize(*x))
            .rev()
    }

    /// Get the variable that appears above `a` in the current order; `None` if
    /// `a` is first in the order
    pub fn above(&self, a: VarLabel) -> Option<VarLabel> {
        let this_level = self.var_to_pos[a.value() as usize];
        if this_level == 0 {
            None
        } else {
            Some(VarLabel::new(self.pos_to_var[this_level - 1] as u64))
        }
    }

    /// Get the variable that appears after `a` in the current order; `None` if
    /// `a` is last in the order
    pub fn below(&self, a: VarLabel) -> Option<VarLabel> {
        let this_level = self.var_to_pos[a.value() as usize];
        if this_level + 1 >= self.pos_to_var.len() {
            None
        } else {
            Some(VarLabel::new(self.pos_to_var[this_level + 1] as u64))
        }
    }

    /// get the first essential variable (i.e., the variable that comes first in
    /// the order) among `a`, `b`, `c`
    pub fn first_essential(&self, a: BddPtr, b: BddPtr, c: BddPtr) -> VarLabel {
        let f1 = self.first(a, b);
        let f2 = self.first(f1, c);
        f2.label()
    }

    /// Produces a vector of variable positions indexed by
    pub fn get_var_to_pos_vec(&self) -> Vec<usize> {
        self.var_to_pos.clone()
    }

    /// Gets the variable that occurs last in the order
    pub fn last_var(&self) -> VarLabel {
        VarLabel::new(*self.pos_to_var.last().unwrap() as u64)
    }

    /// Push a new variable to the end of the order
    pub fn new_last(&mut self) -> VarLabel {
        let pos = self.pos_to_var.len();
        self.var_to_pos.push(pos);
        self.pos_to_var.push(pos);
        VarLabel::new(pos as u64)
    }

    /// Returns an iterator of all variables between [low_level..high_level)
    pub fn between_iter<'a>(
        &'a self,
        low_level: usize,
        high_level: usize,
    ) -> impl Iterator<Item = VarLabel> + 'a {
        assert!(low_level <= high_level);
        self.pos_to_var
            .iter()
            .skip(low_level)
            .take(high_level - low_level)
            .rev()
            .map(|x| VarLabel::new_usize(*x))
    }
}

#[test]
fn var_order_basics() {
    let order = VarOrder::linear_order(10);
    let lbl1 = VarLabel::new(4);
    let lbl2 = VarLabel::new(5);
    assert_eq!(order.lt(lbl1, lbl2), true);
    assert_eq!(order.lt(lbl2, lbl1), false);
    assert_eq!(order.above(lbl2).unwrap(), lbl1);
}
