//! Stores the variable order for a the BDD manager Variables that occur first
//! in the order occur first in the BDD, starting from the root.
//! Lower numbers occur first in the order (i.e., closer to the root)

use crate::repr::VarLabel;
use std::fmt::{Debug, Display};

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
    pub fn new(order: &[VarLabel]) -> VarOrder {
        let mut v = vec![0; order.len()];
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

    /// Generate a linear variable ordering of size `num_vars`
    /// ```
    /// # use rsdd::repr::VarOrder;
    /// let o = VarOrder::linear_order(10);
    /// assert_eq!(o.num_vars(), 10);
    /// ```
    pub fn linear_order(num_vars: usize) -> VarOrder {
        let v: Vec<VarLabel> = (0..num_vars).map(|i| VarLabel::new(i as u64)).collect();
        VarOrder::new(&v)
    }

    /// Gives the number of variables in the order
    /// ```
    /// # use rsdd::repr::VarOrder;
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
    /// # use rsdd::repr::VarOrder;
    /// # use rsdd::repr::VarLabel;
    /// let o = VarOrder::linear_order(10);
    /// assert_eq!(o.var_at_level(4), VarLabel::new(4));
    /// ```
    pub fn var_at_level(&self, pos: usize) -> VarLabel {
        VarLabel::new(self.pos_to_var[pos] as u64)
    }

    /// True if `a` is before `b` in this ordering
    /// ```
    /// # use rsdd::repr::VarOrder;
    /// # use rsdd::repr::VarLabel;
    /// let o = VarOrder::linear_order(10);
    /// assert!(o.lt(VarLabel::new(3), VarLabel::new(4)));
    /// ```
    pub fn lt(&self, a: VarLabel, b: VarLabel) -> bool {
        self.var_to_pos[a.value() as usize] < self.var_to_pos[b.value() as usize]
    }

    /// True if `a` is before or equal to `b` in the ordering
    /// ```
    /// # use rsdd::repr::VarOrder;
    /// # use rsdd::repr::VarLabel;
    /// let o = VarOrder::linear_order(10);
    /// assert!(o.lte(VarLabel::new(3), VarLabel::new(4)));
    /// assert!(o.lte(VarLabel::new(5), VarLabel::new(5)));
    /// ```
    pub fn lte(&self, a: VarLabel, b: VarLabel) -> bool {
        self.var_to_pos[a.value() as usize] <= self.var_to_pos[b.value() as usize]
    }

    /// Returns the item (with a partial label) whose top variable
    /// occurs first in a given ordering (ties broken by returning `a`)
    /// ```
    /// # use rsdd::repr::{BddNode, BddPtr};
    /// # use rsdd::repr::VarOrder;
    /// # use rsdd::repr::VarLabel;
    /// let o = VarOrder::linear_order(2);
    /// let n1 = BddNode::new(VarLabel::new(0), BddPtr::PtrTrue, BddPtr::PtrFalse);
    /// let n2 = BddNode::new(VarLabel::new(1), BddPtr::PtrFalse, BddPtr::PtrTrue);
    ///
    /// let v1 = BddPtr::Reg(&n1);
    /// let v2 = BddPtr::Reg(&n2);
    ///
    /// assert_eq!(o.first(&v1, &v2), &v1);
    ///
    /// assert_eq!(o.first(&BddPtr::PtrTrue, &v2), &v2);
    /// assert_eq!(o.first(&v1, &BddPtr::PtrFalse), &v1);
    /// ```
    pub fn first<'a, T: PartialVariableOrder>(&self, a: &'a T, b: &'a T) -> &'a T {
        match (a.var(), b.var()) {
            (None, _) => b,
            (_, None) => a,
            (Some(va), Some(vb)) => {
                let pa = self.get(va);
                let pb = self.get(vb);
                if pa < pb {
                    a
                } else {
                    b
                }
            }
        }
    }

    /// Returns a sorted pair where the item (with a partial label) whose top variable
    /// is first occurs first in the pair.
    /// ```
    /// # use rsdd::repr::{BddNode, BddPtr};
    /// # use rsdd::repr::VarOrder;
    /// # use rsdd::repr::VarLabel;
    /// let o = VarOrder::linear_order(2);
    /// let n1 = BddNode::new(VarLabel::new(0), BddPtr::PtrTrue, BddPtr::PtrFalse);
    /// let n2 = BddNode::new(VarLabel::new(1), BddPtr::PtrFalse, BddPtr::PtrTrue);
    ///
    /// let v1 = BddPtr::Reg(&n1);
    /// let v2 = BddPtr::Reg(&n2);
    ///
    /// assert_eq!(o.sort(&v2, &v1), (&v1, &v2));
    /// ```
    pub fn sort<'a, T: PartialVariableOrder>(&self, a: &'a T, b: &'a T) -> (&'a T, &'a T) {
        match (a.var(), b.var()) {
            (None, _) => (b, a),
            (_, None) => (a, b),
            (Some(va), Some(vb)) => {
                let pa = self.get(va);
                let pb = self.get(vb);
                if pa < pb {
                    (a, b)
                } else {
                    (b, a)
                }
            }
        }
    }

    /// Iterate through the variables in the order in which they appear in the order
    pub fn in_order_iter(&self) -> impl Iterator<Item = VarLabel> + '_ {
        self.pos_to_var.iter().map(|x| VarLabel::new_usize(*x))
    }

    /// Iterate through the variables in the the reverse order in which they
    /// appear in the order
    pub fn reverse_in_order_iter(&self) -> impl Iterator<Item = VarLabel> + '_ {
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

    /// get the first essential variable
    /// (i.e., the variable that comes first in the order) among `a`, `b`, `c`
    /// ```
    /// # use rsdd::repr::{BddNode, BddPtr};
    /// # use rsdd::repr::VarOrder;
    /// # use rsdd::repr::VarLabel;
    /// let o = VarOrder::linear_order(3);
    /// let n1 = BddNode::new(VarLabel::new(0), BddPtr::PtrTrue, BddPtr::PtrFalse);
    /// let n2 = BddNode::new(VarLabel::new(1), BddPtr::PtrFalse, BddPtr::PtrTrue);
    /// let n3 = BddNode::new(VarLabel::new(2), BddPtr::PtrFalse, BddPtr::PtrTrue);
    ///
    /// let v1 = BddPtr::Reg(&n1);
    /// let v2 = BddPtr::Reg(&n2);
    /// let v3 = BddPtr::Reg(&n3);
    ///
    /// assert_eq!(o.first_essential(&v2, &v3, &v1), VarLabel::new(0));
    /// ```
    pub fn first_essential<T: PartialVariableOrder + Debug>(
        &self,
        a: &T,
        b: &T,
        c: &T,
    ) -> VarLabel {
        let f1 = self.first(a, b);
        let f2 = self.first(f1, c);
        match f2.var() {
            Some(v) => v,
            None => panic!(
                "Could not find a valid first variable among a: {:?}, b: {:?}, c: {:?}",
                a, b, c
            ),
        }
    }

    /// Gets the variable that occurs last in the order
    /// ```
    /// # use rsdd::repr::VarOrder;
    /// let o = VarOrder::linear_order(10);
    /// assert_eq!(o.last_var().value(), 9) // labels are 0-indexed
    /// ```
    pub fn last_var(&self) -> VarLabel {
        VarLabel::new(*self.pos_to_var.last().unwrap() as u64)
    }

    /// Push a new variable to the end of the order
    /// ```
    /// # use rsdd::repr::VarOrder;
    /// let mut o = VarOrder::linear_order(10);
    /// o.new_last();
    /// assert_eq!(o.num_vars(), 11);
    /// assert_eq!(o.last_var().value(), 10)
    /// ```
    pub fn new_last(&mut self) -> VarLabel {
        let pos = self.pos_to_var.len();
        self.var_to_pos.push(pos);
        self.pos_to_var.push(pos);
        VarLabel::new(pos as u64)
    }

    /// Returns an iterator of all variables between [low_level..high_level)
    pub fn between_iter(
        &self,
        low_level: usize,
        high_level: usize,
    ) -> impl Iterator<Item = VarLabel> + '_ {
        assert!(low_level <= high_level);
        self.pos_to_var
            .iter()
            .skip(low_level)
            .take(high_level - low_level)
            .rev()
            .map(|x| VarLabel::new_usize(*x))
    }
}

impl Display for VarOrder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self.pos_to_var))
    }
}

/// Structs that implement this trait expose an optional VarLabel
/// with each item. These are then used for sorting and other
/// ordering operations; None (typically reserved for constants)
/// is considered to be "greater than" other objects (i.e. comes last),
/// and no guaranteed ordering exists between None objects.
pub trait PartialVariableOrder {
    fn var(&self) -> Option<VarLabel>;
}

#[test]
fn var_order_basics() {
    let order = VarOrder::linear_order(10);
    let lbl1 = VarLabel::new(4);
    let lbl2 = VarLabel::new(5);
    assert!(order.lt(lbl1, lbl2));
    assert!(!order.lt(lbl2, lbl1));
    assert_eq!(order.above(lbl2).unwrap(), lbl1);
}
