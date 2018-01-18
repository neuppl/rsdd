use std::collections::{HashMap};
use std::hash::Hash;

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub struct ExternalRef(usize);

/// An internal data structure which tracks external references. Maps an external
/// ref to a particular internal pointer.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
struct ExternalRefElem<InternalRef> {
    r: ExternalRef,
    ptr: InternalRef,
    rc: usize,
}

/// Handles tracking external references
pub struct ExternalRefTable<InternalRef>
where
    InternalRef: Hash + PartialEq + Clone + Eq,
{
    ref_table: HashMap<ExternalRef, ExternalRefElem<InternalRef>>,
    pointer_table: HashMap<InternalRef, ExternalRef>,
    /// a unique counter for generating new nodes
    count: usize,
}

impl<InternalRef> ExternalRefTable<InternalRef>
where
    InternalRef: Hash
        + PartialEq
        + Clone
        + Eq,
{
    pub fn new() -> ExternalRefTable<InternalRef> {
        ExternalRefTable {
            ref_table: HashMap::new(),
            pointer_table: HashMap::new(),
            count: 0,
        }
    }


    /// generates a new ExternalRef, or increments an existing ref's counter and
    /// returns it
    pub fn gen_or_inc(&mut self, ptr: InternalRef) -> ExternalRef {
        let r = match self.pointer_table.get(&ptr) {
            None => None,
            Some(&a) => Some(a.clone()),
        };
        match r {
            None => {
                let new_ext = ExternalRef(self.count);
                let new_elem = ExternalRefElem {
                    r: new_ext,
                    ptr: ptr.clone(),
                    rc: 1,
                };
                self.count += 1;
                self.pointer_table.insert(ptr, new_ext);
                self.ref_table.insert(new_ext, new_elem);
                new_ext
            }
            Some(v) => {
                match self.ref_table.get_mut(&v) {
                    None => panic!("invalid state: external ref with no internal representation"),
                    Some(a) => {
                        a.rc += 1;
                        a.r.clone()
                    }
                }
            }
        }
    }

    /// increment the ref counter
    pub fn incref(&mut self, r: ExternalRef) -> () {
        match self.ref_table.get_mut(&r) {
            None => panic!("Incrementing reference for non-existent external ref"),
            Some(v) => v.rc += 1,
        }
    }

    /// decrement the ref counter
    pub fn decref(&mut self, r: ExternalRef) -> () {
        match self.ref_table.get_mut(&r) {
            None => panic!("Incrementing reference for non-existent external ref"),
            Some(v) => {
                v.rc -= 1;
            }
        }
    }

    pub fn into_internal(&self, r: ExternalRef) -> InternalRef {
        match self.ref_table.get(&r) {
            None => {
                panic!(
                    "dereferencing external pointer with no internal representation; did it get garbage collected?"
                )
            }
            Some(a) => a.ptr.clone(),
        }
    }
}
