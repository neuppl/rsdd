//! A unique table based on a bump allocator and robin-hood hashing
//! this is the primary unique table for storing all nodes

use crate::backing_store::UniqueTable;
use bumpalo::Bump;
use rustc_hash::FxHasher;
use std::{
    hash::{Hash, Hasher},
    mem,
};

/// The load factor of the table, i.e. how full the table will be when it
/// automatically resizes
const LOAD_FACTOR: f64 = 0.7;
const DEFAULT_SIZE: usize = 131072;

/// data structure stored inside of the hash table
#[derive(Clone, Debug, Copy)]
struct HashTableElement<'a, T: Clone> {
    /// pointer into allocator
    ptr: Option<&'a T>,
    /// precomputed hash for T
    hash: u64,
    /// the psl is the *probe sequence length*: it is the distance of this item
    /// from the location that it hashes to in the table.
    psl: u8,
}

impl<'a, T: Clone> Default for HashTableElement<'a, T> {
    fn default() -> Self {
        HashTableElement {
            ptr: None,
            hash: 0,
            psl: 0,
        }
    }
}

impl<'a, T: Clone> HashTableElement<'a, T> {
    pub fn new(ptr: &'a T, hash: u64, psl: u8) -> HashTableElement<'a, T> {
        HashTableElement {
            ptr: Some(ptr),
            hash,
            psl,
        }
    }

    #[inline]
    pub fn is_occupied(&self) -> bool {
        self.ptr.is_some()
    }
}

/// Insert an element into `tbl` without inserting into the backing table. This
/// is used during growing and after an element has been found during
/// `get_or_insert`
fn propagate<'a, T: Clone>(
    v: &mut [HashTableElement<'a, T>],
    cap: usize,
    itm: HashTableElement<'a, T>,
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
#[derive(Debug)]
pub struct BackedRobinhoodTable<'a, T>
where
    T: Hash + PartialEq + Clone,
{
    /// hash table which stores indexes in the elem vector
    tbl: Vec<HashTableElement<'a, T>>,
    /// backing store for BDDs
    alloc: Bump,
    cap: usize,
    /// the length of `tbl`
    len: usize,
    /// # cache hits
    hits: usize,
}

impl<'a, T: Clone> BackedRobinhoodTable<'a, T>
where
    T: Hash + PartialEq + Eq + Clone,
{
    /// reserve a robin-hood table capable of holding at least `sz` elements
    pub fn new() -> BackedRobinhoodTable<'a, T> {
        let v: Vec<HashTableElement<T>> = vec![HashTableElement::default(); DEFAULT_SIZE];

        BackedRobinhoodTable {
            tbl: v,
            alloc: Bump::new(),
            cap: DEFAULT_SIZE,
            len: 0,
            hits: 0,
        }
    }

    /// check if item at index `pos` is occupied
    fn is_occupied(&self, pos: usize) -> bool {
        self.tbl[pos].is_occupied()
    }

    /// check the distance the element at index `pos` is from its desired location
    fn _probe_distance(&self, pos: usize) -> usize {
        self.tbl[pos].psl as usize
    }

    /// Begin inserting `itm` from point `pos` in the hash table.
    fn propagate(&mut self, itm: HashTableElement<'a, T>, pos: usize) {
        propagate(&mut self.tbl, self.cap, itm, pos)
    }

    /// Expands the capacity of the hash table
    pub fn grow(&mut self) {
        let new_sz = (self.cap + 1).next_power_of_two();
        self.cap = new_sz;
        let old = mem::replace(&mut self.tbl, vec![HashTableElement::default(); new_sz]);
        let c = self.cap;
        for i in old.iter() {
            propagate(&mut self.tbl, self.cap, i.clone(), (i.hash as usize) % c);
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &'a T> + '_ {
        self.tbl
            .iter()
            .filter(|x| x.is_occupied())
            .map(|x| x.ptr.unwrap())
    }

    pub fn num_nodes(&self) -> usize {
        self.len
    }

    pub fn hits(&self) -> usize {
        self.hits
    }
}

impl<'a, T: Eq + Hash + Clone> UniqueTable<'a, T> for BackedRobinhoodTable<'a, T> {
    fn get_or_insert(&'a mut self, elem: T) -> &'a T {
        let mut hasher = FxHasher::default();
        elem.hash(&mut hasher);
        let hash = hasher.finish();
        self.get_or_insert_by_hash(hash, elem, false)
    }
}

impl<'a, T: Eq + Hash + Clone> BackedRobinhoodTable<'a, T> {
    /// use a hash to both allocate space in table, *and* form equality
    pub fn get_or_insert_by_hash(
        &'a mut self,
        hash: u64,
        elem: T,
        equality_by_hash: bool,
    ) -> &'a T {
        if (self.len + 1) as f64 > (self.cap as f64 * LOAD_FACTOR) {
            self.grow();
        }

        // the current index into the array
        let mut pos: usize = (hash as usize) % self.cap;
        // the distance this item is from its desired location
        let mut psl = 0;

        loop {
            if self.is_occupied(pos) {
                let cur_itm = self.tbl[pos].clone();
                // first check the hashes to see if these elements could
                // possibly be equal; if they are, check if the items are
                // equal and return the found pointer if so
                if hash == cur_itm.hash {
                    let found: &T = cur_itm.ptr.unwrap();
                    if equality_by_hash || *found == elem {
                        self.hits += 1;
                        return found;
                    }
                }

                // not equal; begin probing
                if cur_itm.psl < psl {
                    // elem is not in the table; insert it at pos and propagate
                    // the item that is currently here
                    self.propagate(cur_itm, pos);
                    let ptr = self.alloc.alloc(elem);
                    let entry = HashTableElement::new(ptr, hash, psl);
                    self.len += 1;
                    self.tbl[pos] = entry;
                    return ptr;
                }
                psl += 1;
                pos = (pos + 1) % self.cap; // wrap to the beginning of the array
            } else {
                // this element is unique, so place it in the current spot
                let ptr = self.alloc.alloc(elem);
                let entry = HashTableElement::new(ptr, hash, psl);
                self.len += 1;
                self.tbl[pos] = entry;
                return ptr;
            }
        }
    }

    pub fn get_by_hash(&'a mut self, hash: u64) -> Option<&'a T> {
        // the current index into the array
        let mut pos: usize = (hash as usize) % self.cap;
        // the distance this item is from its desired location
        let mut psl = 0;

        loop {
            if self.is_occupied(pos) {
                let cur_itm = self.tbl[pos].clone();
                if hash == cur_itm.hash {
                    self.hits += 1;
                    return cur_itm.ptr;
                }

                if cur_itm.psl < psl {
                    return None;
                }
                psl += 1;
                pos = (pos + 1) % self.cap;
            } else {
                return None;
            }
        }
    }
}

impl<'a, T: Hash + Eq + Clone> Default for BackedRobinhoodTable<'a, T> {
    fn default() -> Self {
        Self::new()
    }
}
