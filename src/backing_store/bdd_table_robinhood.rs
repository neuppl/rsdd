use crate::backing_store::robin_hood::*;
use crate::backing_store::{BackingCacheStats, BackingPtr};
use crate::builder::repr::builder_bdd::*;
use crate::repr::var_label::VarLabel;

const DEFAULT_SUBTABLE_SZ: usize = 16384;

#[derive(Clone)]
/// The primary storage unit for binary decision diagram nodes
/// Each variable is associated with an individual subtable
pub struct BddTable {
    subtables: Vec<BackedRobinHoodTable<ToplessBdd>>,
}

impl BddTable {
    pub fn new(num_vars: usize) -> BddTable {
        let mut v = Vec::with_capacity(num_vars);
        for _ in 0..num_vars {
            v.push(BackedRobinHoodTable::new(DEFAULT_SUBTABLE_SZ));
        }

        BddTable { subtables: v }
    }

    /// Generate a new variable which was not in the original order. Places the
    /// new variable at the end of the current order. Returns the label of the
    /// new variable
    pub fn new_last(&mut self) -> VarLabel {
        self.subtables
            .push(BackedRobinHoodTable::new(DEFAULT_SUBTABLE_SZ));
        VarLabel::new_usize(self.subtables.len() - 1)
    }

    pub fn get_or_insert(&mut self, bdd: Bdd) -> BddPtr {
        match bdd {
            Bdd::BddFalse => BddPtr::false_node(),
            Bdd::BddTrue => BddPtr::true_node(),
            Bdd::Node(n) => {
                let var = n.var.value();
                let elem = ToplessBdd::new(n.low, n.high);
                let ptr = self.subtables[var as usize].get_or_insert(&elem);
                BddPtr::new(VarLabel::new(var), TableIndex::new(ptr.0 as u64))
            }
        }
    }

    pub fn deref(&self, ptr: BddPtr) -> Bdd {
        match ptr.ptr_type() {
            PointerType::PtrFalse => Bdd::BddFalse,
            PointerType::PtrTrue => Bdd::BddTrue,
            PointerType::PtrNode => {
                let topless =
                    self.subtables[ptr.var() as usize].deref(BackingPtr(ptr.idx() as u32));
                Bdd::new_node(topless.low, topless.high, VarLabel::new(ptr.var()))
            }
        }
    }

    /// Set the scratch value for a BddPtr
    pub fn set_scratch(&mut self, ptr: BddPtr, v: Option<usize>) {
        self.subtables[ptr.var() as usize]
            .deref_mut(BackingPtr(ptr.idx() as u32))
            .scratch = v;
    }

    /// Set the scratch value for a BddPtr
    pub fn get_scratch(&mut self, ptr: BddPtr) -> Option<usize> {
        self.subtables[ptr.var() as usize]
            .deref_mut(BackingPtr(ptr.idx() as u32))
            .scratch
    }

    pub fn num_nodes(&self) -> usize {
        let mut cnt = 0;
        for tbl in self.subtables.iter() {
            cnt += tbl.num_nodes();
        }
        cnt
    }

    pub fn get_stats(&self) -> BackingCacheStats {
        let mut st = BackingCacheStats::new();
        for tbl in self.subtables.iter() {
            let cur_st = tbl.get_stats();
            st.hit_count += cur_st.hit_count;
            st.lookup_count += cur_st.lookup_count;
            st.num_elements += tbl.num_nodes();
            st.avg_offset += cur_st.avg_offset;
        }
        st.avg_offset /= self.subtables.len() as f64;
        st
    }
}

#[test]
fn test_insertion() {
    let mut tbl = BddTable::new(100);
    for var in 0..50 {
        let bdd = Bdd::new_node(
            BddPtr::true_node(),
            BddPtr::false_node(),
            VarLabel::new(var),
        );
        let r = tbl.get_or_insert(bdd.clone());
        assert_eq!(bdd, tbl.deref(r))
    }
}
