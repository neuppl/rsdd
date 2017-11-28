use robin_hood::BackedRobinHoodTable;
use bdd::*;
use var_order::VarOrder;

const DEFAULT_SUBTABLE_SZ: usize = 65536;

pub struct BddTable {
    subtables: Vec<BackedRobinHoodTable>,
    order: VarOrder
}

impl BddTable {
    pub fn new(order: VarOrder) -> BddTable {
        let mut v = Vec::with_capacity(order.len());
        for i in 0..order.len() {
            v.push(BackedRobinHoodTable::new(
                DEFAULT_SUBTABLE_SZ, VarLabel::new(i as u32)));
        }
        BddTable {
            subtables: v,
            order: order
        }
    }
}