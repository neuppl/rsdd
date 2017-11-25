use bdd::*;
use std::mem::transmute;
use std::mem;
use std::ptr::{self, drop_in_place};
use std::hash::{Hash, Hasher};
use std::io::Cursor;
use twox_hash;
use std;


/// Load-factor for table cells
const TABLE_LOAD_FACTOR : f64 = 0.9;
/// Default table size for table cells
const DEFAULT_TABLE_SIZE : TableIndex = 8192;
const MAX_TABLE_SIZE : TableIndex = std::u16::MAX;


/// A ToplessBdd which tracks a reference count; this is stored inside the BDD
/// table
#[derive(Debug, Clone, PartialEq, Eq)]
struct ToplessBddRc {
    low: BddPtr,
    high: BddPtr,
    rc: RefCount
}

/// Proper heap allocation is still unstable, so we allocate a vector and then
/// forget it.
unsafe fn alloc<T>(count: usize, zero: bool) -> *mut T {
    let mut dummy: Vec<T> = Vec::with_capacity(count);
    let ptr = dummy.as_mut_ptr();
    if zero {
        ptr::write_bytes(ptr, 0, count);
    }
    mem::forget(dummy);
    return ptr;
}

/// Re-cast the pointer to a vector so that it can be properly deallocated
unsafe fn dealloc<T>(p: *mut T, count: usize) {
    let _dummy: Vec<T> = Vec::from_raw_parts(p, 0, count);
    // Dummy is dropped and the memory is freed
}

/// Stores a set of BDDs with a particular top variables
/// ## Summary
/// Implements a robin-hood linear probing hash table. All elements are
/// ToplessBdds. A table cell is empty if it has a reference count of 0, which
/// avoids utilizing an additional bit to represent occupied table cells.
struct RobinHoodTable {
    ptr: *mut ToplessBddRc,
    cap: TableIndex,
    len: TableIndex 
}

/// Tracks whether or not an insert succeeded. 
/// If it succeeds, then it returns Okay(result). If it fails, it returns
/// Failure, and the table must be resized.
#[derive(Debug, PartialEq, Eq)]
enum InsertResult {
    Okay(ToplessBddRc),
    CapacityError
}



/// Compute the desired position in a hash table of size `cap` for a BDD
/// with low pointer `low` and high pointer `high`.
fn compute_pos(cap: TableIndex, low: BddPtr, high: BddPtr) -> TableIndex {
    let mut hasher = twox_hash::XxHash::with_seed(0xdeadbeef);
    hasher.write_u16(low.idx);
    hasher.write_u16(low.var);
    hasher.write_u16(high.idx);
    hasher.write_u16(high.var);
    (hasher.finish() % (cap as u64)) as TableIndex
}


impl RobinHoodTable {
    fn new(sz : TableIndex) -> RobinHoodTable {
        unsafe {
            RobinHoodTable {
                ptr: alloc(sz as usize, true),
                cap: sz,
                len: 0
            }
        }
    }


    /// check if item at index `pos` is occupied
    fn is_occupied(&self, pos: isize) -> bool {
        assert!(pos < self.cap as isize);
        unsafe {
            let bdd = (&*self.ptr.offset(pos)).clone();
            bdd.rc > 0
        }
    }

    /// get the BDD at `pos`
    fn get(&self, pos: isize) -> ToplessBddRc {
        unsafe {
            assert!(pos < self.cap as isize);
            let r = &*self.ptr.offset(pos);
            r.clone()
        }
    }

    /// check the distance the element at index `pos` is from its desired location
    fn probe_distance(&self, pos: isize) -> TableIndex {
        unsafe {
            let bdd = (*self.ptr.offset(pos)).clone();
            let desired_pos = compute_pos(self.cap, bdd.low, bdd.high);
            // we subtract here since we always probe forwards, so the desired position
            // is always less than or equal to the current position
            if desired_pos > (pos as TableIndex) {
                desired_pos - (pos as TableIndex)
            } else { 
                // we wrapped around
                (self.cap as TableIndex - desired_pos) + (pos as TableIndex)
            }
        }
    }

    fn set_value(&mut self, pos: isize, v: ToplessBddRc) -> () {
        assert!(pos < self.cap as isize);
        unsafe {
            *self.ptr.offset(pos) = v;
        }
    }

    fn get_or_insert(&mut self, bdd: ToplessBddRc) -> InsertResult {
        let sz = (((self.len + 1) as f64) * TABLE_LOAD_FACTOR) / (self.cap as f64);
        if sz > self.cap as f64 {
            return InsertResult::CapacityError;
        } else {
            self.len += 1;
            // capacity is met, find an adjacent spot by probing forwards until one is found.
            let mut pos = compute_pos(self.cap.clone(), bdd.low.clone(), bdd.high.clone());
            let mut cur_v = bdd.clone();
            let mut cur_probe_dist = 0; // track how far the current item has probed
            loop {
                if self.is_occupied(pos as isize) {
                    // If we find an element which has probed less than the current element, 
                    // place the current element in that position and find a new place for the 
                    // swapped element
                    let this_bdd = self.get(pos as isize);
                    if this_bdd.low == bdd.low && this_bdd.high == bdd.high {
                        return InsertResult::Okay(this_bdd) // we found the BDD in the table
                    } else {
                        // check if this item's position is closer than ours
                        let existing_dist = self.probe_distance(pos as isize);
                        if existing_dist < cur_probe_dist {
                            // swap out our position for this one
                            let tmp = self.get(pos as isize);
                            self.set_value(pos as isize, this_bdd);
                            cur_v = tmp;
                            cur_probe_dist = existing_dist;
                        }
                    }
                    pos = (pos + 1) % (self.cap as TableIndex) // wrap to the beginning of the array
                } else {
                    // place the element in the current spot, we're done
                    self.set_value(pos as isize, cur_v.clone());
                    return InsertResult::Okay(bdd)
                }
            }
        }
        panic!("impossible state")
    }

    /// Insert a fresh BDD (without any RC), or find the existing one 
    fn get_or_insert_fresh(&mut self, low: BddPtr, high: BddPtr) -> InsertResult {
        self.get_or_insert(ToplessBddRc {low: low, high: high, rc: 1})
    }

    /// Finds the index for a particular bdd, none if it is not found
    pub fn find(&self, bddlow: BddPtr, bddhigh: BddPtr) -> Option<TableIndex> {
        let mut pos = compute_pos(self.cap.clone(), bddlow.clone(), bddhigh.clone());
        loop {
            if self.is_occupied(pos as isize) {
                let cur = self.get(pos as isize);
                if cur.low == bddlow && cur.high  == bddhigh {
                    return Some(pos);
                } else {
                    pos += 1;
                }
            } else {
                return None
            }
        }
    }

    /// increment reference counter for BDD at `pos`
    fn incref(&mut self, bddlow: BddPtr, bddhigh: BddPtr) -> () {
        let pos = match self.find(bddlow, bddhigh) {
            Some(a) => a,
            None => panic!("incrementing non-existent BDD ref")
        };
        let mut bdd = self.get(pos as isize);
        bdd.rc += 1;
        self.set_value(pos as isize, bdd)
    }

    /// decrement reference counter for BDD at `pos`
    fn decref(&mut self, bddlow: BddPtr, bddhigh: BddPtr) -> () {
        let pos = match self.find(bddlow, bddhigh) {
            Some(a) => a,
            None => panic!("decrementing non-existent BDD ref")
        };
        let mut bdd = self.get(pos as isize);
        bdd.rc -= 1;
        self.set_value(pos as isize, bdd)
    }

    /// Increase the capacity and deallocate the old hash table
    fn grow(&mut self) ->  () {
        assert!(self.cap << 1 <= MAX_TABLE_SIZE);
        let new_cap = self.cap << 1;
        // insert each element of the old vector into the new one
        let mut new_map = RobinHoodTable::new(new_cap);
        for i in 0..self.cap {
            if self.is_occupied(i as isize) {
                let b = self.get(i as isize);
                new_map.get_or_insert(b);
            }
        }
        unsafe { dealloc(self.ptr, self.cap as usize) };
        self.ptr = new_map.ptr;
        self.cap = new_map.cap;
        self.len = new_map.len;
    }
}

/// Stores the table of all generated BDDs
/// 
/// Each BDD is associated with a sub-table called a TableCell
pub struct BddTable { 
    tables: Vec<RobinHoodTable>
}

impl BddTable {
    fn new(num_variables: usize) -> BddTable {
        let mut v = Vec::with_capacity(num_variables);
        for _ in 0..num_variables {
            v.push(RobinHoodTable::new(DEFAULT_TABLE_SIZE));
        }
        BddTable{ 
            tables: v
        }
    }

    // fn get_or_insert(&mut self, bdd: Bdd) -> BddRc {
    //     let cur_tbl = &self.tables[bdd.var as usize];
        
    // }
}

#[test]
fn test_simple_cell_insert() {
    fn mk_ptr(i: u16) -> BddPtr {
        BddPtr {var: 0, idx: i}
    }
    let mut cell = RobinHoodTable::new(DEFAULT_TABLE_SIZE);
    cell.get_or_insert_fresh(mk_ptr(0), mk_ptr(0));
    cell.get_or_insert_fresh(mk_ptr(0), mk_ptr(0));
    cell.get_or_insert_fresh(mk_ptr(0), mk_ptr(1));
    cell.get_or_insert_fresh(mk_ptr(1), mk_ptr(0));
    cell.incref(mk_ptr(0), mk_ptr(0));
    let r = match cell.get_or_insert_fresh(mk_ptr(0), mk_ptr(0)) {
        InsertResult::Okay(a) => a,
        _ => panic!("could not find")
    };
    assert_eq!(r.rc, 2);
}

#[test]
fn test_many_cell_insert() {
    fn mk_ptr(i: u16) -> BddPtr {
        BddPtr {var: 0, idx: i}
    }
    let mut cell = RobinHoodTable::new(DEFAULT_TABLE_SIZE);
    for i in 0..3000 {
        let r1 = cell.get_or_insert_fresh(mk_ptr(i), mk_ptr(i));
        let r2 = cell.get_or_insert_fresh(mk_ptr(i), mk_ptr(i));
        assert_eq!(r1, r2);
    }
}

#[test]
fn test_grow_cell() {
    fn mk_ptr(i: u16) -> BddPtr {
        BddPtr {var: 0, idx: i}
    }
    let mut cell = RobinHoodTable::new(8192);
    for i in 0..3000 {
        let r1 = cell.get_or_insert_fresh(mk_ptr(i), mk_ptr(i));
        let r2 = cell.get_or_insert_fresh(mk_ptr(i), mk_ptr(i));
        assert_eq!(r1, r2);
    }
    cell.grow();
    for i in 0..6000 {
        let r1 = cell.get_or_insert_fresh(mk_ptr(i), mk_ptr(i));
        let r2 = cell.get_or_insert_fresh(mk_ptr(i), mk_ptr(i));
        assert_eq!(r1, r2);
    }
    cell.grow();
    for i in 0..10000{
        let r1 = cell.get_or_insert_fresh(mk_ptr(i), mk_ptr(i));
        let r2 = cell.get_or_insert_fresh(mk_ptr(i), mk_ptr(i));
        assert_eq!(r1, r2);
    }
}