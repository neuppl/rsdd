use sdd::*;
use manager::BddManager;
use robin_hood::*;
use btree::*;
use std::collections::HashMap;
use backing_store::*;
use bdd::VarLabel;
use std::slice::Iter;
use var_order;


const DEFAULT_RH_SZ: usize = 32768;

/// The SDD table consists of two types of node allocators: BDD
/// allocators, and SDD allocators
enum SubTable {
    BddSubTable{
        man: BddManager,
        /// convert from BDD var labels to SDD var labels
        conv: HashMap<VarLabel, VarLabel>,
    },
    SddSubTable {
        tbl: BackedRobinHoodTable<SddOr>
    }
}


/// Handles memory management for the SDD manager
pub struct SddTable {
    /// mapping between sdd and bdd variable labels
    pub sdd_to_bdd: HashMap<VarLabel, VarLabel>,
    tables: Vec<SubTable>
}


impl SddTable {
    pub fn new(vtree: &VTree) -> SddTable {
        let mut t = SddTable {
            tables: Vec::new(),
            sdd_to_bdd: HashMap::new(),
        };

        for (index, v) in vtree.in_order_iter().enumerate() {
            match v {
                &BTree::Leaf(ref o) => {
                    let man = BddManager::new(var_order::VarOrder::new(o.clone()));
                    let mut m : HashMap<VarLabel, VarLabel> = HashMap::new();
                    for (var_idx, v) in o.iter().enumerate() {
                        t.sdd_to_bdd.insert(
                            v.clone(),
                            VarLabel::new(var_idx as u64),
                        );
                        m.insert(VarLabel::new(var_idx as u64), v.clone());
                    }
                    t.tables.push(SubTable::BddSubTable { man: man, conv: m })
                },
                &BTree::Node(_, _, _) => {
                    let s = SubTable::SddSubTable {
                        tbl: BackedRobinHoodTable::new(DEFAULT_RH_SZ)
                    };
                    t.tables.push(s);
                }
            }
        }
        t
    }

    pub fn is_sdd(&self, ptr: SddPtr) -> bool {
        match self.tables[ptr.vtree() as usize] {
            SubTable::SddSubTable{tbl: _} => true,
            _ => false
        }
    }

    pub fn is_bdd(&self, ptr: SddPtr) -> bool {
        match self.tables[ptr.vtree() as usize] {
            SubTable::BddSubTable{man: _, conv: _} => true,
            _ => false
        }
    }


    /// get or insert a particular SDD node with vtree-node `vnode`
    pub fn get_or_insert_sdd(&mut self, sdd: SddOr, vnode: usize) -> SddPtr {
        match &mut self.tables[vnode] {
            &mut SubTable::SddSubTable{tbl: ref mut tbl} => {
                let ptr = tbl.get_or_insert(sdd);
                SddPtr::new_node(ptr.0 as usize, vnode as u16)
            },
            _ => panic!("invalid vnode: inserting SDD into BDD")
        }
    }


    /// Fetch the iterator for a particular SDD or-node
    /// used during application
    pub fn sdd_iter_or_panic(&self, ptr: SddPtr) -> Iter<(SddPtr, SddPtr)> {
        match &self.tables[ptr.vtree() as usize] {
            &SubTable::SddSubTable{tbl: ref tbl} => {
                tbl.deref(BackingPtr(ptr.idx() as u32)).nodes.iter()
            },
            _ => panic!("dereferencing SDD into BDD")
        }
    }

    /// Fetch the iterator for a particular SDD or-node
    /// used during application
    pub fn sdd_or_panic(&self, ptr: SddPtr) -> Vec<(SddPtr, SddPtr)> {
        match &self.tables[ptr.vtree() as usize] {
            &SubTable::SddSubTable{tbl: ref tbl} => {
                tbl.deref(BackingPtr(ptr.idx() as u32)).nodes.clone()
            },
            _ => panic!("dereferencing SDD into BDD")
        }
    }


    /// Fetch the BDD manager for a particular SDD node level `node`
    /// Panics if it not a BDD
    pub fn bdd_man(&self, node: usize) -> &BddManager {
        match &self.tables[node] {
            &SubTable::BddSubTable{man: ref man, conv: ref conv} => {
                man
            },
            _ => panic!("dereferencing SDD into BDD")
        }
    }

    pub fn bdd_conv(&self, node: usize) -> &HashMap<VarLabel, VarLabel> {
        match &self.tables[node] {
            &SubTable::BddSubTable{man: ref man, conv: ref conv} => {
                conv
            },
            _ => panic!("dereferencing SDD into BDD")
        }
    }

    /// Fetch the BDD manager for a particular SDD node level `node`
    /// Panics if it not a BDD
    pub fn bdd_man_mut(&mut self, node: usize) -> &mut BddManager {
        match &mut self.tables[node] {
            &mut SubTable::BddSubTable{man: ref mut man, conv: ref conv} => {
                man
            },
            _ => panic!("dereferencing SDD into BDD")
        }
    }
}
