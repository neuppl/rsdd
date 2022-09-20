//! Caching backing store for the SddBuilder

use crate::{
    backing_store::robin_hood::*, backing_store::*, builder::bdd_builder::BddManager,
    builder::repr::builder_sdd::*, builder::var_order::VarOrder, repr::var_label::VarLabel,
    util::btree::*,
};

use std::collections::HashMap;

const DEFAULT_RH_SZ: usize = 32768;

/// The SDD table consists of two types of node allocators: BDD
/// allocators, and SDD allocators
enum SubTable {
    BddSubTable {
        man: BddManager,
        /// convert from BDD var labels to SDD var labels
        conv: HashMap<VarLabel, VarLabel>,
    },
    SddSubTable {
        tbl: BackedRobinHoodTable<Vec<(SddPtr, SddPtr)>>,
    },
}

/// Handles memory management for the SDD manager
pub struct SddTable {
    /// mapping between sdd and bdd variable labels
    pub sdd_to_bdd: HashMap<VarLabel, VarLabel>,
    tables: Vec<SubTable>,
}

impl SddTable {
    pub fn new(vtree: &VTree) -> SddTable {
        let mut t = SddTable {
            tables: Vec::new(),
            sdd_to_bdd: HashMap::new(),
        };

        for v in vtree.dfs_iter() {
            match v {
                &BTree::Leaf(ref o) => {
                    let mut new_order = Vec::new();
                    let mut m: HashMap<VarLabel, VarLabel> = HashMap::new();
                    for (var_idx, v) in o.iter().enumerate() {
                        t.sdd_to_bdd.insert(*v, VarLabel::new(var_idx as u64));
                        m.insert(VarLabel::new(var_idx as u64), *v);
                        new_order.push(VarLabel::new(var_idx as u64));
                    }
                    let man = BddManager::new(VarOrder::new(new_order));
                    t.tables.push(SubTable::BddSubTable { man, conv: m })
                }
                &BTree::Node(_, _, _) => {
                    let s = SubTable::SddSubTable {
                        tbl: BackedRobinHoodTable::new(DEFAULT_RH_SZ),
                    };
                    t.tables.push(s);
                }
            }
        }
        t
    }

    pub fn _is_sdd(&self, ptr: SddPtr) -> bool {
        match self.tables[ptr.vtree().value() as usize] {
            SubTable::SddSubTable { tbl: _ } => true,
            _ => false,
        }
    }

    /// Converts a SDD var label into its internal BDD var label; panics on failure
    pub fn sdd_to_bdd_label(&self, label: &VarLabel) -> &VarLabel {
        self.sdd_to_bdd.get(label).unwrap()
    }

    pub fn _is_bdd(&self, ptr: SddPtr) -> bool {
        match self.tables[ptr.vtree().value() as usize] {
            SubTable::BddSubTable { man: _, conv: _ } => true,
            _ => false,
        }
    }

    /// Get or insert a particular SDD node with vtree-node `vnode`
    pub fn get_or_insert_sdd(&mut self, sdd: &SddOr, vnode: usize) -> SddPtr {
        match &mut self.tables[vnode] {
            &mut SubTable::SddSubTable { ref mut tbl } => {
                let ptr = tbl.get_or_insert(&sdd.nodes);
                SddPtr::new_node(ptr.0 as usize, vnode as u16)
            }
            _ => panic!("invalid vnode: inserting SDD into BDD"),
        }
    }

    /// Fetch the slice for a set of or-nodes; panics if this is not an SDD node
    pub fn sdd_get_or(&self, ptr: SddPtr) -> &[(SddPtr, SddPtr)] {
        match &self.tables[ptr.vtree().value() as usize] {
            &SubTable::SddSubTable { ref tbl } => tbl.deref(BackingPtr(ptr.idx() as u32)),
            _ => panic!("dereferencing BDD into SDD"),
        }
    }

    /// Fetch the BDD manager for a particular SDD vtree `vtree_idx`
    /// Panics if it not a BDD
    pub fn bdd_man(&self, vtree_idx: usize) -> &BddManager {
        match &self.tables[vtree_idx] {
            &SubTable::BddSubTable { ref man, conv: _ } => man,
            _ => panic!("dereferencing SDD into BDD"),
        }
    }

    pub fn bdd_conv(&self, node: usize) -> &HashMap<VarLabel, VarLabel> {
        match &self.tables[node] {
            &SubTable::BddSubTable { man: _, ref conv } => conv,
            _ => panic!("dereferencing SDD into BDD"),
        }
    }

    /// Fetch the BDD manager for a particular SDD node level `node`
    /// Panics if it not a BDD
    pub fn bdd_man_mut(&mut self, node: usize) -> &mut BddManager {
        match &mut self.tables[node] {
            &mut SubTable::BddSubTable {
                ref mut man,
                conv: _,
            } => man,
            _ => panic!("dereferencing SDD into BDD"),
        }
    }
}
