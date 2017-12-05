use robin_hood::BackedRobinHoodTable;
use bdd::*;
use var_order::VarOrder;

const DEFAULT_SUBTABLE_SZ: usize = 2000000;

/// The primary storage unit for binary decision diagram nodes
/// Each variable is associated with an individual subtable
pub struct BddTable {
    subtables: Vec<BackedRobinHoodTable>,
    order: VarOrder,
}

impl BddTable {
    pub fn new(order: VarOrder) -> BddTable {
        let mut v = Vec::with_capacity(order.len());
        for i in 0..order.len() {
            v.push(BackedRobinHoodTable::new(
                DEFAULT_SUBTABLE_SZ,
                VarLabel::new(i as u64),
            ));
        }
        BddTable {
            subtables: v,
            order: order,
        }
    }

    pub fn order(&self) -> &VarOrder {
        &self.order
    }

    pub fn get_or_insert(&mut self, bdd: Bdd) -> BddPtr {
        match bdd {
            Bdd::BddFalse => BddPtr::false_node(),
            Bdd::BddTrue => BddPtr::true_node(),
            Bdd::Node(n) =>
                self.subtables[n.var.value() as usize].get_or_insert(n.low, n.high)
        }

    }

    pub fn find(&self, bdd: Bdd) -> Option<BddPtr> {
        match bdd {
            Bdd::BddFalse => Some(BddPtr::false_node()),
            Bdd::BddTrue => Some(BddPtr::true_node()),
            Bdd::Node(n) =>
                self.subtables[n.var.value() as usize].find(n.low, n.high)
        }
    }

    pub fn deref(&self, ptr: BddPtr) -> Bdd {
        match ptr.ptr_type() {
            PointerType::PtrFalse => Bdd::BddFalse,
            PointerType::PtrTrue => Bdd::BddTrue,
            PointerType::PtrNode => {
                let topless = self.subtables[ptr.var() as usize].deref(ptr.clone());
                Bdd::new_node(topless.low, topless.high, VarLabel::new(ptr.var()))
            }
        }
    }
}


#[test]
fn test_insertion() {
    let mut tbl = BddTable::new(VarOrder::linear_order(100));
    for var in 0..50 {
        let bdd = Bdd::new_node(
            BddPtr::true_node(),
            BddPtr::false_node(),
            VarLabel::new(var)
        );
        let r = tbl.get_or_insert(bdd.clone());
        assert_eq!(bdd, tbl.deref(r))
    }
}
