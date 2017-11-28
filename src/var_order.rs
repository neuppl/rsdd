use bdd::VarLabel;

pub struct VarOrder {
    /// an associative array, each index corresponds to a variable. I.e., the
    /// position of variable i in the order is given by the value of the array at
    /// index i
    vars: Vec<usize>
}

impl VarOrder {
    pub fn new(order: Vec<VarLabel>) -> VarOrder {
        let mut v = Vec::new();
        for i in 0..order.len() {
            v.push(order[i].value() as usize)
        }
        VarOrder {vars: v}
    }

    pub fn len(&self) -> usize {
        self.vars.len()
    }

    /// get the position of `var` in the order
    pub fn get(&self, var: VarLabel) -> usize {
        self.vars[var.value() as usize]
    }
}