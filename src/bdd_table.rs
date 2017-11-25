use bdd::*;
use std::mem::transmute;
use std::mem;
use std::ptr::{self, drop_in_place};
use std::hash::{Hash, Hasher};
use std::io::Cursor;
use twox_hash;
use std;

/////////////////////////////////////////////////////////////////////////////////
/// Constants

/// Load-factor for robin-hood tables 
const RH_LOAD_FACTOR : f64 = 0.9;
/// Default table size for robin hood tables
const DEFAULT_RH_SIZE : TableIndex = 8192;
const LINEAR_LOAD_FACTOR : f64 = 0.7;
const LINEAR_TABLE_SIZE : TableIndex = 8192;
const MAX_TABLE_SIZE : TableIndex = std::u32::MAX >> 8;

/// Tracks the current cache level for the BDD node
type CacheLevel = u8;
const CACHE_EMPTY : u8 = 0;
const CACHE_COLD : u8 = 1;
const CACHE_MED : u8 = 2;
const CACHE_HOT : u8 = 3;
const CACHE_DEFAULT : u8 = CACHE_MED;

////////////////////////////////////////////////////////////////////////////////
/// Base values

/// The primary unit of storage in the BDD hash table. Q: Why does it not store a
/// variable? Because each hash-table is associated with a particular variable.
/// 
/// ## Subtables
/// Each table index in a BddPtr is associated with a particular sub-table. The
/// high 8 bits of each idx specify a sub-table. A sub-table of 0 is the
/// long-term storage, which is a robin-hood hash which is occasionally regrown.
/// A non-zero sub-table is a linear hash, which maintains a consistent load
/// factor and spawns a new sub-table when that load factor is reached.
/// 
/// ## Cache levels
/// The cache level of a node specifies how hot a node is. A hotness of 0 means
/// the node has never been allocated (e.g., is an empty element in a hashtable).
/// Nodes with non-zero hotness are collected probabilistically if they have a
/// reference count of 0, with a higher hotness meaning a lower probability of
/// being collected.
#[derive(Debug, Clone, PartialEq, Eq)]
struct ToplessBddRc {
    low: BddPtr,
    high: BddPtr,
    rc: RefCount,
    level: CacheLevel
}

/// Implement some helper functions which allow us to easily access the bits 
/// which specify the subtable and index of a BddPtr
impl BddPtr {
    fn get_subtable(&self) -> TableIndex {
        self.idx >> 24
    }
    fn get_index(&self) -> TableIndex {
        self.idx & 0xffffff
    }
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

/// Compute the desired position in a hash table of size `cap` for a BDD
/// with low pointer `low` and high pointer `high`.
fn compute_pos(cap: TableIndex, low: TableIndex, high: TableIndex) -> TableIndex {
    let mut hasher = twox_hash::XxHash::with_seed(0xdeadbeef);
    hasher.write_u32(low);
    hasher.write_u32(high);
    (hasher.finish() % (cap as u64)) as TableIndex
}

/// Tracks whether or not an insert succeeded. 
/// If it succeeds, then it returns Okay(result). If it fails, it returns
/// Failure, and the table must be resized.
#[derive(Debug, PartialEq, Eq)]
enum InsertResult {
    Ok,
    CapacityError
}

////////////////////////////////////////////////////////////////////////////////
/// Robin-hood hashing

/// Implements a robin-hood linear probing hash table for storing BDDs. All
/// elements are ToplessBdds. 
/// 
/// ## Reference validity
/// Inserting elements and growing the table results in all incoming references
/// to the table being invalidated. Looking up elements is stable. Thus, whenever
/// this table is updated, *all* incoming pointers must be correspondingly
/// updated to the new pointer values. This means that inserting is very
/// expensive. However, this table is extremely good average lookup performance
/// and can handle very high load ratios (up to 0.9), which makes it a good
/// choice for long-term storage.
struct RobinHoodTable {
    ptr: *mut ToplessBddRc,
    cap: TableIndex,
    len: TableIndex 
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
        assert!(pos < self.cap as isize, "failure: pos {} should be less than capacity {}", pos, self.cap);
        unsafe {
            let bdd = (&*self.ptr.offset(pos)).clone();
            bdd.level > CACHE_EMPTY
        }
    }

    /// get the BDD at `pos`
    fn get_pos(&self, pos: isize) -> ToplessBddRc {
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
            let desired_pos = compute_pos(self.cap, bdd.low.get_index(), bdd.high.get_index());
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

    /// Inserts an element from the table
    /// Invalidates references
    fn insert(&mut self, bdd: ToplessBddRc) -> InsertResult {
        let sz = (((self.len + 1) as f64) * RH_LOAD_FACTOR) / (self.cap as f64);
        if sz > self.cap as f64 {
            return InsertResult::CapacityError;
        } else {
            self.len += 1;
            // capacity is met, find an adjacent spot by probing forwards until one is found.
            let mut pos = compute_pos(self.cap.clone(), bdd.low.get_index(), bdd.high.get_index());
            let mut cur_v = bdd.clone();
            let mut cur_probe_dist = 0; // track how far the current item has probed
            loop {
                if self.is_occupied(pos as isize) {
                    // If we find an element which has probed less than the current element, 
                    // place the current element in that position and find a new place for the 
                    // swapped element
                    let this_bdd = self.get_pos(pos as isize);
                    if this_bdd.low == bdd.low && this_bdd.high == bdd.high {
                        return InsertResult::Ok // we found the BDD in the table
                    } else {
                        // check if this item's position is closer than ours
                        let existing_dist = self.probe_distance(pos as isize);
                        if existing_dist < cur_probe_dist {
                            // swap out our position for this one
                            let tmp = self.get_pos(pos as isize);
                            self.set_value(pos as isize, this_bdd);
                            cur_v = tmp;
                            cur_probe_dist = existing_dist;
                        }
                    }
                    pos = (pos + 1) % (self.cap as TableIndex) // wrap to the beginning of the array
                } else {
                    // place the element in the current spot, we're done
                    self.set_value(pos as isize, cur_v.clone());
                    return InsertResult::Ok
                }
            }
        }
        panic!("impossible state")
    }

    /// Insert a fresh BDD (without any RC), or find the existing one 
    /// Invalidates references
    fn insert_fresh(&mut self, low: BddPtr, high: BddPtr) -> InsertResult {
        self.insert(ToplessBddRc {low: low, high: high, rc: 1, level: CACHE_DEFAULT})
    }

    /// Finds the index for a particular bdd, none if it is not found
    /// Does not invalidate references.
    pub fn find(&self, bddlow: BddPtr, bddhigh: BddPtr) -> Option<TableIndex> {
        let mut pos = compute_pos(self.cap.clone(), bddlow.get_index(), bddhigh.get_index());
        loop {
            if self.is_occupied(pos as isize) {
                let cur = self.get_pos(pos as isize);
                if cur.low == bddlow && cur.high  == bddhigh {
                    return Some(pos);
                } else {
                    pos = (pos + 1) % self.cap;
                }
            } else {
                return None
            }
        }
    }

    /// Fetch the desired element from the hash table. Returns None if not found.
    /// Does not invalidate references.
    pub fn get(&self, bddlow: BddPtr, bddhigh: BddPtr) -> Option<ToplessBddRc> {
        match self.find(bddlow, bddhigh) {
            Some(a) => Some(self.get_pos(a as isize)),
            None => None
        }
    }

    /// increment reference counter for BDD at `pos`
    fn incref(&mut self, bddlow: BddPtr, bddhigh: BddPtr) -> () {
        let pos = match self.find(bddlow, bddhigh) {
            Some(a) => a,
            None => panic!("incrementing non-existent BDD ref")
        };
        let mut bdd = self.get_pos(pos as isize);
        bdd.rc += 1;
        self.set_value(pos as isize, bdd)
    }

    /// decrement reference counter for BDD at `pos`
    fn decref(&mut self, bddlow: BddPtr, bddhigh: BddPtr) -> () {
        let pos = match self.find(bddlow, bddhigh) {
            Some(a) => a,
            None => panic!("decrementing non-existent BDD ref")
        };
        let mut bdd = self.get_pos(pos as isize);
        bdd.rc -= 1;
        self.set_value(pos as isize, bdd)
    }

    /// Increase the capacity and deallocate the old hash table
    /// Invalidates references.
    fn grow(&mut self) ->  () {
        assert!(self.cap << 1 <= MAX_TABLE_SIZE);
        let new_cap = self.cap << 1;
        // insert each element of the old vector into the new one
        let mut new_map = RobinHoodTable::new(new_cap);
        for i in 0..self.cap {
            if self.is_occupied(i as isize) {
                let b = self.get_pos(i as isize);
                new_map.insert(b);
            }
        }
        unsafe { dealloc(self.ptr, self.cap as usize) };
        self.ptr = new_map.ptr;
        self.cap = new_map.cap;
        self.len = new_map.len;
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Linear table

/// Implements a linear hash table with stable storage (will not move elements)
struct LinearBddTable {
    ptr: *mut ToplessBddRc,
    cap: TableIndex,
    len: TableIndex 
}

////////////////////////////////////////////////////////////////////////////////
/// Core table

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
            v.push(RobinHoodTable::new(DEFAULT_RH_SIZE));
        }
        BddTable{ 
            tables: v
        }
    }

    // fn get_or_insert(&mut self, bdd: Bdd) -> BddRc {
    //     let cur_tbl = &self.tables[bdd.var as usize];
        
    // }
}


////////////////////////////////////////////////////////////////////////////////
/// Tests
#[test]
fn test_simple_cell_insert() {
    fn mk_ptr(i: TableIndex) -> BddPtr {
        BddPtr {var: 0, idx: i}
    }
    let mut cell = RobinHoodTable::new(DEFAULT_RH_SIZE);
    cell.insert_fresh(mk_ptr(0), mk_ptr(0));
    cell.insert_fresh(mk_ptr(0), mk_ptr(0));
    cell.insert_fresh(mk_ptr(0), mk_ptr(1));
    cell.insert_fresh(mk_ptr(1), mk_ptr(0));
    cell.incref(mk_ptr(0), mk_ptr(0));
    let r = match cell.get(mk_ptr(0), mk_ptr(0)) {
        Some(a) => a,
        _ => panic!("could not find")
    };
    assert_eq!(r.rc, 2);
}

#[test]
fn test_many_cell_insert() {
    fn mk_ptr(i: TableIndex) -> BddPtr {
        BddPtr {var: 0, idx: i}
    }
    let mut cell = RobinHoodTable::new(DEFAULT_RH_SIZE);
    for i in 0..3000 {
        println!("inserting {}", i);
        cell.insert_fresh(mk_ptr(i), mk_ptr(i));
        println!("getting {}", i);
        let r1 = cell.get(mk_ptr(i), mk_ptr(i));
        cell.insert_fresh(mk_ptr(i), mk_ptr(i));
        let r2 = cell.get(mk_ptr(i), mk_ptr(i));
        assert_eq!(r1, r2);
    }
}

#[test]
fn test_grow_cell() {
    fn mk_ptr(i: TableIndex) -> BddPtr {
        BddPtr {var: 0, idx: i}
    }
    let mut cell = RobinHoodTable::new(8192);
    println!("initial");
    for i in 0..3000 {
        cell.insert_fresh(mk_ptr(i), mk_ptr(i));
        let r1 = cell.get(mk_ptr(i), mk_ptr(i));
        cell.insert_fresh(mk_ptr(i), mk_ptr(i));
        let r2 = cell.get(mk_ptr(i), mk_ptr(i));
        assert_eq!(r1, r2);
    }
    println!("growing");
    cell.grow();
    for i in 0..6000 {
        cell.insert_fresh(mk_ptr(i), mk_ptr(i));
        let r1 = cell.get(mk_ptr(i), mk_ptr(i));
        cell.insert_fresh(mk_ptr(i), mk_ptr(i));
        let r2 = cell.get(mk_ptr(i), mk_ptr(i));
        assert_eq!(r1, r2);
    }
    cell.grow();
    for i in 0..10000{
        cell.insert_fresh(mk_ptr(i), mk_ptr(i));
        let r1 = cell.get(mk_ptr(i), mk_ptr(i));
        cell.insert_fresh(mk_ptr(i), mk_ptr(i));
        let r2 = cell.get(mk_ptr(i), mk_ptr(i));
        assert_eq!(r1, r2);
    }
}