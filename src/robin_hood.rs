use bdd::*;
use std::ptr;
use std::hash::Hasher;
use twox_hash;
use std::mem;
#[macro_use]
use util::*;

const LOAD_FACTOR: f64 = 0.8;


/// data structure stored inside of the hash table
#[derive(Clone, Debug)]
struct HashTableElement {
    data: u64,
}

BITFIELD!(HashTableElement data : u64 [
    occupied set_occupied[0..1], // whether or not the cell is occupied
    offset set_offset[1..6],     // the distance of this cell from its preferred location
    hash set_hash[6..10],        // the high-order hash bits of the BDD this cell maps to
    idx set_idx[10..64],         // the index into the backing store for this cell
]);

impl HashTableElement {
    fn new(idx: TableIndex, hash: u64) -> HashTableElement {
        let mut init = HashTableElement { data: 0 };
        init.set_occupied(1);
        init.set_hash((hash >> 32) as u64); // grab some bits of the hash
        init.set_idx(idx.value());
        return init;
    }
}

/// Implements a mutable vector-backed robin-hood linear probing hash table,
/// whose keys are given by BDD pointers.
pub struct BackedRobinHoodTable {
    /// hash table which stores indexes in the elem vector
    tbl: Vec<HashTableElement>,
    /// backing store for BDDs
    elem: Vec<ToplessBdd>,
    /// the variable which this table corresponds with
    var: VarLabel,
    /// the capacity of `tbl`; given as a particular power of 2
    cap: usize,
    /// the length of `tbl`
    len: usize,
}

#[inline]
fn hash_pair(low: BddPtr, high: BddPtr) -> u64 {
    let mut hasher = twox_hash::XxHash::with_seed(0xdeadbeef);
    hasher.write_u64(low.raw());
    hasher.write_u64(high.raw());
    hasher.finish()
}

impl BackedRobinHoodTable {
    /// reserve a robin-hood table capable of holding at least `sz` elements
    pub fn new(sz: usize, var: VarLabel) -> BackedRobinHoodTable {
        let tbl_sz = ((sz as f64 * (1.0 + LOAD_FACTOR)) as usize).next_power_of_two();
        let mut r = BackedRobinHoodTable {
            elem: Vec::with_capacity(sz as usize),
            tbl: Vec::with_capacity(tbl_sz as usize),
            var: var,
            cap: tbl_sz,
            len: 0,
        };
        // zero the vector and set its length
        unsafe {
            let vec_ptr = r.tbl.as_mut_ptr();
            ptr::write_bytes(vec_ptr, 0, tbl_sz as usize);
            r.tbl.set_len(tbl_sz);
        }
        return r;
    }


    /// check if item at index `pos` is occupied
    #[inline(never)]
    fn is_occupied(&self, pos: usize) -> bool {
        // very oddly, this comparison is extremely costly!
        unsafe {
            mem::transmute::<u8, bool>(self.tbl[pos].occupied() as u8)
        }
        // self.tbl[pos].occupied() == 1
    }

    /// get the BDD pointed to at `pos`
    #[inline]
    fn get_pos(&self, pos: usize) -> ToplessBdd {
        self.elem[self.tbl[pos].idx() as usize].clone()
    }

    /// check the distance the element at index `pos` is from its desired location
    #[inline]
    fn probe_distance(&self, pos: usize) -> usize {
        self.tbl[pos].offset() as usize
    }

    /// Get or insert a fresh (low, high) pair
    pub fn get_or_insert(&mut self, low: BddPtr, high: BddPtr) -> BddPtr {
        // ensure available capacity
        let sz = (((self.len + 1) as f64) * LOAD_FACTOR) / (self.cap as f64);
        assert!(sz + 1.0 < self.cap as f64);
        let mut found: Option<BddPtr> = None; // holds location of inserted element
        let hash_v = hash_pair(low.clone(), high.clone());
        let mut pos = (hash_v as usize) % self.cap;
        let mut searcher = HashTableElement::new(
            TableIndex::new(self.elem.len() as u64), hash_v);
        loop {
            if self.is_occupied(pos) {
                let cur_itm = self.tbl[pos].clone();
                // first check the hashes to see if these elements could
                // possibly be equal
                if cur_itm.hash() == searcher.hash() {
                    let this_bdd = self.get_pos(pos as usize);
                    if this_bdd.low == low && this_bdd.high == high {
                        return BddPtr::new(
                            self.var.clone(), TableIndex::new(cur_itm.idx()));
                    } else {
                    }
                }
                // check if this item's position is closer than ours
                if cur_itm.offset() < searcher.offset() {
                    // check if we have inserted a fresh item; if we have not, then
                    // we need to insert the item into the backing store
                    if found.is_none() {
                        self.elem.push(ToplessBdd::new(
                            low.clone(),
                            high.clone(),
                            GcBits::new(),
                        ));
                        self.len += 1;
                        found = Some(BddPtr::new(
                            self.var.clone(),
                            TableIndex::new(searcher.idx()),
                        ));
                    } else {
                    }
                    // swap out our position for this one
                    self.tbl[pos] = searcher;
                    searcher = cur_itm;
                }
                let off = searcher.offset() + 1;
                searcher.set_offset(off);
                pos = (pos + 1) % self.cap; // wrap to the beginning of the array
            } else {
                // place the element in the current spot, we're done
                if found.is_none() {
                    self.elem.push(ToplessBdd::new(low, high, GcBits::new()));
                    self.len += 1;
                    self.tbl[pos] = searcher.clone();
                    return BddPtr::new(self.var.clone(),
                                       TableIndex::new(searcher.idx()));
                } else {
                    return found.unwrap();
                }
            }
        }
    }




    /// Finds the index for a particular bdd, none if it is not found
    /// Does not invalidate references.
    pub fn find(&self, low: BddPtr, high: BddPtr) -> Option<BddPtr> {
        let hash_v = hash_pair(low.clone(), high.clone());
        let mut pos = (hash_v as usize) % self.cap;
        let searcher = HashTableElement::new(
            TableIndex::new(self.elem.len() as u64), hash_v);
        loop {
            let cur_itm = self.tbl[pos].clone();
            if cur_itm.occupied() == 1 {
                // first check the hashes to see if these elements could
                // possibly be equal
                if cur_itm.hash() == searcher.hash() {
                    let this_bdd = self.get_pos(pos as usize);
                    if this_bdd.low == low && this_bdd.high == high {
                        return Some(BddPtr::new(
                            self.var.clone(),
                            TableIndex::new(cur_itm.idx()),
                        ));
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
    pub fn deref(&self, ptr: BddPtr) -> ToplessBdd {
        assert!(VarLabel::new(ptr.var()) == self.var);
        self.elem[ptr.idx() as usize].clone()
    }

    pub fn average_offset(&self) -> f64 {
        let total = self.tbl.iter().fold(0, |sum, ref cur| cur.offset() + sum);
        (total as f64) / (self.len as f64)
    }
}

////////////////////////////////////////////////////////////////////////////////
// tests

fn mk_ptr(idx: u64) -> BddPtr {
    BddPtr::new(VarLabel::new(0), TableIndex::new(idx))
}

#[test]
fn test_simple() {
    let mut store = BackedRobinHoodTable::new(100000, VarLabel::new(0));
    for i in 0..100000 {
        let v = store.get_or_insert(mk_ptr(i), mk_ptr(i));
        match store.find(mk_ptr(i), mk_ptr(i)) {
            None => assert!(false),
            Some(a) => {
                assert_eq!(v, a);
                assert_eq!(store.deref(v), store.deref(a));
            }
        }
    }
    println!("average offset: {}", store.average_offset());
}
