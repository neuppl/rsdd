use std::ptr;
use std::hash::Hasher;
use std::mem;
use std::hash::Hash;
use twox_hash::XxHash;
use fnv::FnvHasher;
use std::hash::BuildHasherDefault;
#[macro_use]
use util::*;

const LOAD_FACTOR: f64 = 0.7;
const GROWTH_RATE: usize = 16;

/// Pointer into the backing store
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BackingPtr(pub u32);

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
        init.set_hash((hash >> 32) as u64); // grab some bits of the hash
        init.set_idx(idx.0 as u64);
        return init;
    }
}


/// Implements a mutable vector-backed robin-hood linear probing hash table,
/// whose keys are given by BDD pointers.
pub struct BackedRobinHoodTable<T>
where
    T: Hash + PartialEq + Eq + Clone,
{
    /// hash table which stores indexes in the elem vector
    tbl: Vec<HashTableElement>,
    /// backing store for BDDs
    elem: Vec<T>,
    cap: usize,
    /// the length of `tbl`
    len: usize,
}

impl<T> BackedRobinHoodTable<T>
where
    T: Hash + PartialEq + Eq + Clone,
{
    /// reserve a robin-hood table capable of holding at least `sz` elements
    pub fn new(sz: usize) -> BackedRobinHoodTable<T> {
        let v: Vec<HashTableElement> = zero_vec(sz);
        let r = BackedRobinHoodTable {
            elem: Vec::with_capacity(sz as usize),
            tbl: v,
            cap: sz,
            len: 0,
        };
        return r;
    }


    /// check if item at index `pos` is occupied
    fn is_occupied(&self, pos: usize) -> bool {
        // very oddly, this comparison is extremely costly!
        unsafe { mem::transmute::<u8, bool>(self.tbl[pos].occupied() as u8) }
        // self.tbl[pos].occupied() == 1
    }

    fn get_pos(&self, pos: usize) -> T {
        self.elem[self.tbl[pos].idx() as usize].clone()
    }

    /// check the distance the element at index `pos` is from its desired location
    fn probe_distance(&self, pos: usize) -> usize {
        self.tbl[pos].offset() as usize
    }

    /// Begin inserting `itm` from point `pos` in the hash table.
    fn propagate(&mut self, itm: HashTableElement, pos: usize) -> () {
        let mut searcher = itm;
        let mut pos = pos;
        loop {
            if self.is_occupied(pos) {
                let cur_itm = self.tbl[pos].clone();
                // check if this item's position is closer than ours
                if cur_itm.offset() < searcher.offset() {
                    // swap the searcher and this item
                    self.tbl[pos] = searcher;
                    searcher = cur_itm;
                }
                let off = searcher.offset() + 1;
                searcher.set_offset(off);
                pos = (pos + 1) % self.cap; // wrap to the beginning of the array
            } else {
                // place the element in the current spot, we're done
                self.tbl[pos] = searcher.clone();
                return ();
            }
        }
    }

    /// Get or insert a fresh (low, high) pair
    pub fn get_or_insert(&mut self, elem: T) -> BackingPtr {
        if (self.len + 1) as f64 > (self.cap as f64 * LOAD_FACTOR) {
            self.grow();
        }

        let mut hasher = XxHash::default();
        elem.hash(&mut hasher);
        let hash_v = hasher.finish();
        let mut pos = (hash_v as usize) % self.cap;
        let mut searcher = HashTableElement::new(BackingPtr(self.elem.len() as u32), hash_v);
        loop {
            if self.is_occupied(pos) {
                let cur_itm = self.tbl[pos].clone();
                // first check the hashes to see if these elements could
                // possibly be equal

                if cur_itm.hash() == searcher.hash() {
                    let this_bdd = self.get_pos(pos as usize);
                    if this_bdd == elem {
                        return BackingPtr(cur_itm.idx() as u32);
                    } else {
                    }
                }
                // check if this item's position is closer than ours
                if cur_itm.offset() < searcher.offset() {
                    // insert the fresh item here
                    self.elem.push(elem);
                    self.tbl[pos] = searcher;
                    self.len += 1;
                    // propagate the element we swapped for
                    self.propagate(cur_itm, pos);
                    return BackingPtr(searcher.idx() as u32)
                }
                let off = searcher.offset() + 1;
                searcher.set_offset(off);
                pos = (pos + 1) % self.cap; // wrap to the beginning of the array
            } else {
                // place the element in the current spot, we're done
                self.elem.push(elem);
                self.len += 1;
                self.tbl[pos] = searcher.clone();
                return BackingPtr(searcher.idx() as u32);
            }
        }
    }


    /// Finds the index for a particular bdd, none if it is not found
    /// Does not invalidate references.
    pub fn find(&self, elem: T) -> Option<BackingPtr> {
        let mut hasher = XxHash::default();
        elem.hash(&mut hasher);
        let hash_v = hasher.finish();
        let mut pos = (hash_v as usize) % self.cap;
        let searcher = HashTableElement::new(BackingPtr(self.elem.len() as u32), hash_v);
        loop {
            let cur_itm = self.tbl[pos].clone();
            if cur_itm.occupied() == 1 {
                // first check the hashes to see if these elements could
                // possibly be equal
                if cur_itm.hash() == searcher.hash() {
                    let this_bdd = self.get_pos(pos as usize);
                    if this_bdd == elem {
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
    #[inline(always)]
    pub fn deref(&self, ptr: BackingPtr) -> &T {
        &self.elem[ptr.0 as usize]
    }

    /// Expands the capacity of the hash table
    pub fn grow(&mut self) -> () {
        let new_sz = (self.cap + 1).next_power_of_two();
        let mut new_tbl = BackedRobinHoodTable::new(new_sz);
        for itm in self.elem.iter() {
            new_tbl.get_or_insert(itm.clone());
        }
        self.elem = new_tbl.elem;
        self.cap = new_tbl.cap;
        self.len = new_tbl.len;
        self.tbl = new_tbl.tbl;
    }

    pub fn average_offset(&self) -> f64 {
        let total = self.tbl.iter().fold(0, |sum, ref cur| cur.offset() + sum);
        (total as f64) / (self.len as f64)
    }

    pub fn num_nodes(&self) -> usize {
        self.len
    }
}

////////////////////////////////////////////////////////////////////////////////
// tests
use bdd::{BddPtr, VarLabel, TableIndex, ToplessBdd};
fn mk_ptr(idx: u64) -> BddPtr {
    BddPtr::new(VarLabel::new(0), TableIndex::new(idx))
}

#[test]
fn rh_simple() {
    let mut store: BackedRobinHoodTable<ToplessBdd> = BackedRobinHoodTable::new(5000);
    for i in 0..100000 {
        let e = ToplessBdd::new(mk_ptr(i), mk_ptr(i));
        let v = store.get_or_insert(e.clone());
        match store.find(e) {
            None => assert!(false, "Could not find {:?}", e),
            Some(a) => {
                assert_eq!(v, a);
                assert_eq!(store.deref(v), store.deref(a));
            }
        }
    }
    println!("average offset: {}", store.average_offset());
}
