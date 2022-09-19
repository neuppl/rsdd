//! A backing store based on robin-hood hashing

use crate::backing_store::*;
use crate::util::*;
use fnv::FnvHasher;
use std::hash::{Hash, Hasher};
use std::mem;

/// The load factor of the table, i.e. how full the table will be when it
/// automatically resizes
const LOAD_FACTOR: f64 = 0.5;

/// data structure stored inside of the hash table
#[derive(Clone, Debug, Copy)]
struct HashTableElement {
    data: u64,
}

BITFIELD!(HashTableElement data : u64 [
    occupied set_occupied[0..1], // whether or not the cell is occupied
    offset set_offset[1..6],     // the distance of this cell from its preferred location
    hash set_hash[6..16],        // the high-order hash bits of the BDD this cell maps to
    idx set_idx[16..64],         // the index into the backing store for this cell
]);

impl HashTableElement {
    fn new(idx: BackingPtr, hash: u64) -> HashTableElement {
        let mut init = HashTableElement { data: 0 };
        init.set_occupied(1);
        init.set_hash((hash >> 32) as u64); // grab some high-order bits of the hash
        init.set_idx(idx.0 as u64);
        init
    }
}

/// An element of the backing store; used for cache memoization
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct BackingElem<T>
where
    T: Hash + PartialEq + Eq + Clone,
{
    elem: T,
    hash_mem: usize, // store the hash value so that it is not recomputed
    mark: bool,      // a mark used during garbage collection
}

impl<T> BackingElem<T>
where
    T: Hash + PartialEq + Eq + Clone,
{
    fn hash(&self) -> usize {
        self.hash_mem
    }

    fn new(elem: T, hash: usize) -> BackingElem<T> {
        BackingElem {
            elem,
            hash_mem: hash,
            mark: false,
        }
    }
}

/// Insert an element into `tbl` without inserting into the backing table. This
/// is used during growing and after an element has been found during
/// `get_or_insert`
fn propagate(v: &mut [HashTableElement], cap: usize, itm: HashTableElement, pos: usize) {
    let mut searcher = itm;
    let mut pos = pos;
    loop {
        if v[pos].occupied() == 1 {
            let cur_itm = v[pos];
            // check if this item's position is closer than ours
            if cur_itm.offset() < searcher.offset() {
                // swap the searcher and this item
                v[pos] = searcher;
                searcher = cur_itm;
            }
            let off = searcher.offset() + 1;
            searcher.set_offset(off);
            pos = (pos + 1) % cap; // wrap to the beginning of the array
        } else {
            // place the element in the current spot, we're done
            v[pos] = searcher;
            return;
        }
    }
}

#[derive(Clone)]
/// Implements a mutable vector-backed robin-hood linear probing hash table,
/// whose keys are given by BDD pointers.
pub struct BackedRobinHoodTable<T>
where
    T: Hash + PartialEq + Eq + Clone,
{
    /// hash table which stores indexes in the elem vector
    tbl: Vec<HashTableElement>,
    /// backing store for BDDs
    elem: Vec<BackingElem<T>>,
    cap: usize,
    /// the length of `tbl`
    len: usize,
    stats: BackingCacheStats,
}

impl<T> BackedRobinHoodTable<T>
where
    T: Hash + PartialEq + Eq + Clone,
{
    /// reserve a robin-hood table capable of holding at least `sz` elements
    pub fn new(sz: usize) -> BackedRobinHoodTable<T> {
        let v: Vec<HashTableElement> = zero_vec(sz);

        BackedRobinHoodTable {
            elem: Vec::with_capacity(sz as usize),
            tbl: v,
            cap: sz,
            len: 0,
            stats: BackingCacheStats::new(),
        }
    }

    /// check if item at index `pos` is occupied
    fn is_occupied(&self, pos: usize) -> bool {
        // very oddly, this comparison is extremely costly!
        unsafe { mem::transmute::<u8, bool>(self.tbl[pos].occupied() as u8) }
        // self.tbl[pos].occupied() == 1
    }

    fn get_pos(&self, pos: usize) -> &T {
        &self.elem[self.tbl[pos].idx() as usize].elem
    }

    /// check the distance the element at index `pos` is from its desired location
    fn _probe_distance(&self, pos: usize) -> usize {
        self.tbl[pos].offset() as usize
    }

    /// Begin inserting `itm` from point `pos` in the hash table.
    fn propagate(&mut self, itm: HashTableElement, pos: usize) {
        propagate(&mut self.tbl, self.cap, itm, pos)
    }

    /// Get or insert a fresh (low, high) pair
    pub fn get_or_insert(&mut self, elem: &T) -> BackingPtr {
        if (self.len + 1) as f64 > (self.cap as f64 * LOAD_FACTOR) {
            self.grow();
        }
        self.stats.lookup_count += 1;
        let mut hasher = FnvHasher::default();
        elem.hash(&mut hasher);
        let hash_v = hasher.finish() as usize;
        let mut pos = hash_v % self.cap;
        let mut searcher = HashTableElement::new(BackingPtr(self.elem.len() as u32), hash_v as u64);
        loop {
            if self.is_occupied(pos) {
                let cur_itm = self.tbl[pos];
                // first check the hashes to see if these elements could
                // possibly be equal

                if cur_itm.hash() == searcher.hash() && elem.eq(self.get_pos(pos as usize)) {
                    self.stats.hit_count += 1;
                    return BackingPtr(cur_itm.idx() as u32);
                }
                // check if this item's position is closer than ours
                if cur_itm.offset() < searcher.offset() {
                    // insert the fresh item here
                    self.elem.push(BackingElem::new(elem.clone(), hash_v));
                    self.tbl[pos] = searcher;
                    self.len += 1;
                    // propagate the element we swapped for
                    self.propagate(cur_itm, pos);
                    return BackingPtr(searcher.idx() as u32);
                }
                let off = searcher.offset() + 1;
                searcher.set_offset(off);
                pos = (pos + 1) % self.cap; // wrap to the beginning of the array
            } else {
                // place the element in the current spot, we're done
                self.elem.push(BackingElem::new(elem.clone(), hash_v));
                self.len += 1;
                let idx = searcher.idx();
                self.tbl[pos] = searcher;
                return BackingPtr(idx as u32);
            }
        }
    }

    /// Finds the index for a particular bdd, none if it is not found
    /// Does not invalidate references.
    pub fn _find(&self, elem: T) -> Option<BackingPtr> {
        let mut hasher = FnvHasher::default();
        elem.hash(&mut hasher);
        let hash_v = hasher.finish();
        let mut pos = (hash_v as usize) % self.cap;
        let searcher = HashTableElement::new(BackingPtr(self.elem.len() as u32), hash_v);
        loop {
            let cur_itm = self.tbl[pos];
            if cur_itm.occupied() == 1 {
                // first check the hashes to see if these elements could
                // possibly be equal
                if cur_itm.hash() == searcher.hash() {
                    let this_bdd = self.get_pos(pos as usize);
                    if *this_bdd == elem {
                        return Some(BackingPtr(cur_itm.idx() as u32));
                    }
                }
                pos = (pos + 1) % self.cap;
            } else {
                return None;
            }
        }
    }

    /// Dereferences a BDD pointer that lives in this table
    pub fn deref(&self, ptr: BackingPtr) -> &T {
        &self.elem[ptr.0 as usize].elem
    }

    /// Dereferences a BDD pointer that lives in this table
    pub fn deref_mut(&mut self, ptr: BackingPtr) -> &mut T {
        &mut self.elem[ptr.0 as usize].elem
    }

    /// Expands the capacity of the hash table
    pub fn grow(&mut self) {
        let new_sz = (self.cap + 1).next_power_of_two();
        self.cap = new_sz;
        self.tbl = zero_vec(new_sz);
        let c = self.cap;
        for (idx, i) in self.elem.iter().enumerate() {
            let hash_v = i.hash();
            let hashelem = HashTableElement::new(BackingPtr(idx as u32), hash_v as u64);
            propagate(&mut self.tbl, self.cap, hashelem, hash_v % c);
        }
    }

    pub fn _capacity(&self) -> usize {
        self.elem.len()
    }

    pub fn average_offset(&self) -> f64 {
        let total = self.tbl.iter().fold(0, |sum, cur| cur.offset() + sum);
        (total as f64) / (self.len as f64)
    }

    pub fn num_nodes(&self) -> usize {
        self.len
    }

    pub fn get_stats(&self) -> BackingCacheStats {
        let mut r = self.stats.clone();
        r.avg_offset = self.average_offset();
        r
    }
}

////////////////////////////////////////////////////////////////////////////////
// tests
#[cfg(test)]
mod tests {
    use crate::builder::repr::builder_bdd::{BddPtr, TableIndex, ToplessBdd};
    use crate::repr::var_label::VarLabel;

    use super::BackedRobinHoodTable;
    fn _mk_ptr(idx: u64) -> BddPtr {
        BddPtr::new(VarLabel::new(0), TableIndex::new(idx))
    }

    #[test]
    fn rh_simple() {
        let mut store: BackedRobinHoodTable<ToplessBdd> = BackedRobinHoodTable::new(5000);
        for i in 0..1000000 {
            let e = ToplessBdd::new(_mk_ptr(i), _mk_ptr(i));
            let v = store.get_or_insert(&e);
            let v_2 = store.get_or_insert(&e);
            assert_eq!(store.deref(v), store.deref(v_2));
        }
    }
}
