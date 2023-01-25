//! A unique table based on a bump allocator and robin-hood hashing
//! this is the primary unique table for storing all nodes

use super::UniqueTable;
use bumpalo::Bump;
use std::hash::{Hash, Hasher};
use std::mem;

use crate::util::*;

use rustc_hash::FxHasher;

/// The load factor of the table, i.e. how full the table will be when it
/// automatically resizes
const LOAD_FACTOR: f64 = 0.7;
const DEFAULT_SIZE: usize = 131072;

/// data structure stored inside of the hash table
#[derive(Clone, Debug, Copy)]
struct HashTableElement<T: Clone> {
    /// pointer into allocator
    ptr: *mut T,
    /// precomputed hash for T
    hash: u64,
    /// the psl is the *probe sequence length*: it is the distance of this item
    /// from the location that it hashes to in the table.
    psl: u8,
}

impl<T: Clone> Default for HashTableElement<T> {
    fn default() -> Self {
        HashTableElement {
            ptr: 0 as *mut T,
            hash: 0,
            psl: 0,
        }
    }
}

impl<T: Clone> HashTableElement<T> {
    pub fn new(ptr: *mut T, hash: u64, psl: u8) -> HashTableElement<T> {
        HashTableElement {
            ptr,
            hash,
            psl: psl,
        }
    }

    pub fn is_occupied(&self) -> bool {
        return self.ptr != 0 as *mut T;
    }
}

/// Insert an element into `tbl` without inserting into the backing table. This
/// is used during growing and after an element has been found during
/// `get_or_insert`
fn propagate<T: Clone>(
    v: &mut Vec<HashTableElement<T>>,
    cap: usize,
    itm: HashTableElement<T>,
    pos: usize,
) {
    let mut searcher = itm;
    let mut pos = pos;
    loop {
        if v[pos].is_occupied() {
            let cur_itm = v[pos].clone();
            // check if this item's position is closer than ours
            if cur_itm.psl < searcher.psl {
                // swap the searcher and this item
                v[pos] = searcher;
                searcher = cur_itm;
            }
            let off = searcher.psl + 1;
            searcher.psl = off;
            pos = (pos + 1) % cap; // wrap to the beginning of the array
        } else {
            // place the element in the current spot, we're done
            v[pos] = searcher;
            return;
        }
    }
}

/// Implements a mutable vector-backed robin-hood linear probing hash table,
/// whose keys are given by BDD pointers.
pub struct BackedRobinhoodTable<T>
where
    T: Hash + PartialEq + Eq + Clone,
{
    /// hash table which stores indexes in the elem vector
    tbl: Vec<HashTableElement<T>>,
    /// backing store for BDDs
    alloc: Bump,
    cap: usize,
    /// the length of `tbl`
    len: usize,
}

impl<T: Clone> BackedRobinhoodTable<T>
where
    T: Hash + PartialEq + Eq + Clone,
{
    /// reserve a robin-hood table capable of holding at least `sz` elements
    pub fn new() -> BackedRobinhoodTable<T> {
        let v: Vec<HashTableElement<T>> = zero_vec(DEFAULT_SIZE);

        BackedRobinhoodTable {
            tbl: v,
            alloc: Bump::new(),
            cap: DEFAULT_SIZE,
            len: 0,
        }
    }

    /// check if item at index `pos` is occupied
    fn is_occupied(&self, pos: usize) -> bool {
        self.tbl[pos].is_occupied()
    }

    fn get_pos(&self, pos: usize) -> *mut T {
        self.tbl[pos].ptr
    }

    /// check the distance the element at index `pos` is from its desired location
    fn _probe_distance(&self, pos: usize) -> usize {
        self.tbl[pos].psl as usize
    }

    /// Begin inserting `itm` from point `pos` in the hash table.
    fn propagate(&mut self, itm: HashTableElement<T>, pos: usize) {
        propagate(&mut self.tbl, self.cap, itm, pos)
    }

    /// Expands the capacity of the hash table
    pub fn grow(&mut self) {
        let new_sz = (self.cap + 1).next_power_of_two();
        self.cap = new_sz;
        let old = mem::replace(&mut self.tbl, zero_vec(new_sz));
        let c = self.cap;
        for i in old.iter() {
            propagate(&mut self.tbl, self.cap, i.clone(), (i.hash as usize) % c);
        }
    }

    // pub fn average_offset(&self) -> f64 {
    //     let total = self.tbl.iter().fold(0, |sum, cur| cur.offset() + sum);
    //     (total as f64) / (self.len as f64)
    // }

    pub fn num_nodes(&self) -> usize {
        self.len
    }
}

impl<T: Eq + PartialEq + Hash + Clone> UniqueTable<T> for BackedRobinhoodTable<T> {
    fn get_or_insert(&mut self, elem: T) -> *mut T {
        if (self.len + 1) as f64 > (self.cap as f64 * LOAD_FACTOR) {
            self.grow();
        }
        let mut hasher = FxHasher::default();
        elem.hash(&mut hasher);
        let elem_hash = hasher.finish();

        // the current index into the array
        let mut pos: usize = (elem_hash as usize) % self.cap;
        // the distance this item is from its desired location
        let mut psl = 0;

        loop {
            if self.is_occupied(pos) {
                let cur_itm = self.tbl[pos].clone();
                // first check the hashes to see if these elements could
                // possibly be equal; if they are, check if the items are
                // equal and return the found pointer if so
                if elem_hash == cur_itm.hash {
                    let found: &T = unsafe { &*cur_itm.ptr };
                    if *found == elem {
                        return cur_itm.ptr;
                    }
                }

                // not equal; begin probing
                if cur_itm.psl < psl {
                    // elem is not in the table; insert it at pos and propagate
                    // the item that is currently here
                    self.propagate(cur_itm, pos);
                    let ptr = self.alloc.alloc(elem);
                    let entry = HashTableElement::new(ptr, elem_hash, psl);
                    self.len += 1;
                    self.tbl[pos] = entry;
                    return ptr;
                }
                psl += 1;
                pos = (pos + 1) % self.cap; // wrap to the beginning of the array
            } else {
                // this element is unique, so place it in the current spot
                let ptr = self.alloc.alloc(elem);
                let entry = HashTableElement::new(ptr, elem_hash, psl);
                self.len += 1;
                self.tbl[pos] = entry;
                return ptr;
            }
        }
    }
}
