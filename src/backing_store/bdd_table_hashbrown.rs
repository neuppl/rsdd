use backing_store::{BackingCacheStats};
use repr::bdd::*;
use repr::var_label::VarLabel;
use manager::var_order::VarOrder;
use std::collections::HashMap;

const DEFAULT_SUBTABLE_SZ: usize = 1 << 14;

/// The primary storage unit for binary decision diagram nodes
/// Each variable is associated with an individual subtable
pub struct BddTable {
    subtables: Vec<Vec<ToplessBdd>>,
    submaps: Vec<HashMap<ToplessBdd, usize>>,
    order: VarOrder,
}


impl BddTable {
    pub fn new(order: VarOrder) -> BddTable {
        let mut v = Vec::with_capacity(order.len());
        let mut subtables = Vec::with_capacity(order.len());
        for _ in 0..order.len() {
            v.push(HashMap::with_capacity(DEFAULT_SUBTABLE_SZ));
            subtables.push(Vec::with_capacity(DEFAULT_SUBTABLE_SZ));
        }

        BddTable {
            subtables: subtables,
            submaps: v,
            order: order,
        }
    }

    pub fn order(&self) -> &VarOrder {
        &self.order
    }

    /// Generate a new variable which was not in the original order. Places the
    /// new variable at the end of the current order. Returns the label of the
    /// new variable
    pub fn new_last(&mut self) -> VarLabel {
        let newlbl = self.order.new_last();
        self.submaps.push(HashMap::with_capacity(DEFAULT_SUBTABLE_SZ));
        self.subtables.push(Vec::with_capacity(DEFAULT_SUBTABLE_SZ));
        newlbl
    }

    pub fn get_or_insert(&mut self, bdd: Bdd) -> BddPtr {
        match bdd {
            Bdd::BddFalse => BddPtr::false_node(),
            Bdd::BddTrue => BddPtr::true_node(),
            Bdd::Node(n) => {
                let var = n.var.value();
                let elem = ToplessBdd::new(n.low, n.high);
                let ptr = self.submaps[var as usize].entry(elem).or_insert({
                    self.subtables[n.var.value() as usize].push(elem);
                    self.subtables[n.var.value() as usize].len() - 1
                });
                BddPtr::new(VarLabel::new(var), TableIndex::new(*ptr as u64))
            }
        }
    }

    pub fn deref(&self, ptr: BddPtr) -> Bdd {
        match ptr.ptr_type() {
            PointerType::PtrFalse => Bdd::BddFalse,
            PointerType::PtrTrue => Bdd::BddTrue,
            PointerType::PtrNode => {
                let topless = &self.subtables[ptr.var() as usize][ptr.idx() as usize];
                Bdd::new_node(topless.low, topless.high, VarLabel::new(ptr.var()))
            }
        }
    }

    pub fn num_nodes(&self) -> usize {
        let mut cnt = 0;
        // for tbl in self.subtables.iter() {
        //     cnt += tbl.num_nodes();
        // }
        cnt
    }


    pub fn get_stats(&self) -> BackingCacheStats {
        let mut st = BackingCacheStats::new();
        // for tbl in self.subtables.iter() {
        //     let cur_st = tbl.get_stats();
        //     st.hit_count += cur_st.hit_count;
        //     st.lookup_count += cur_st.lookup_count;
        //     st.num_elements += tbl.num_nodes();
        // }
        st
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



/// A caching data-structure for storing and looking up values associated with
/// BDD nodes
pub struct TraverseTable<T> {
    subtables: Vec<Vec<Option<T>>>
}

impl<T> TraverseTable<T> where T : Clone {
    pub fn new(tbl: &BddTable) -> TraverseTable<T> {
        // let v = tbl.subtables.iter().map(|x| vec![None; x.num_nodes()]).collect();
        TraverseTable {
            subtables: Vec::new()
        }
    }

    pub fn set(&mut self, ptr: &BddPtr, data: T) -> () {
        self.subtables[ptr.var() as usize][ptr.idx() as usize] = Some(data)
    }

    pub fn get(&self, ptr: &BddPtr) -> &Option<T> {
        &self.subtables[ptr.var() as usize][ptr.idx() as usize]
    }
}
