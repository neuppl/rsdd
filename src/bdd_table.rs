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
const DEFAULT_LINEAR_SIZE: TableIndex = 8192;
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
#[derive(Debug, Clone, PartialEq, Eq)]
struct ToplessBdd {
    low: BddPtr,
    high: BddPtr,
    level: CacheLevel,
    mark: u8
}

impl ToplessBdd {
    fn new(low: BddPtr, high: BddPtr) -> ToplessBdd {
        ToplessBdd {low: low, high: high, mark: 0, level: CACHE_DEFAULT}
    }
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
    /// Generate a new BddPtr for a particular table at index idx
    fn new(var: VarLabel, table: TableIndex, idx: TableIndex) -> BddPtr {
        let new_idx = (table << 24) | (idx & 0xffffff);
        BddPtr {var: var, idx: new_idx}
    }
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

/// Implements a mutable robin-hood linear probing hash table for storing BDDs.
/// All elements are ToplessBdds.
struct MutRobinHoodTable {
    elem: Vec<ToplessBdd>,
    var: VarLabel,
    tbl_id: TableIndex,
    cap: TableIndex,
    len: TableIndex
}


impl MutRobinHoodTable {
    fn new(sz : TableIndex, var: VarLabel, tbl_id: TableIndex) -> MutRobinHoodTable {
        let mut r = MutRobinHoodTable {
            elem: Vec::with_capacity(sz as usize),
            var: var,
            tbl_id: tbl_id,
            cap: sz,
            len: 0
        };
        // zero the vector and set its length
        unsafe {
            let vec_ptr = r.elem.as_mut_ptr();
            ptr::write_bytes(vec_ptr, 0, sz as usize);
            r.elem.set_len(sz as usize);
        }
        return r
    }


    /// check if item at index `pos` is occupied
    fn is_occupied(&self, pos: usize) -> bool {
        self.elem[pos].level > CACHE_EMPTY
    }

    /// get the BDD at `pos`
    fn get_pos(&self, pos: usize) -> ToplessBdd {
        self.elem[pos].clone()
    }

    /// check the distance the element at index `pos` is from its desired location
    fn probe_distance(&self, pos: usize) -> TableIndex {
        unsafe {
            let bdd = self.get_pos(pos);
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

    fn set_value(&mut self, pos: usize, v: ToplessBdd) -> () {
        self.elem[pos] = v
    }

    /// Inserts an element from the table. Panics if same element is inserted
    /// twice, as this is likely an error. Invalidates references.
    fn insert(&mut self, bdd: ToplessBdd) -> () {
        let sz = (((self.len + 1) as f64) * RH_LOAD_FACTOR) / (self.cap as f64);
        assert!(sz < self.cap as f64);
        self.len += 1;
        // capacity is met, find an adjacent spot by probing forwards until one is found.
        let mut pos = compute_pos(self.cap.clone(), bdd.low.get_index(), bdd.high.get_index());
        let mut cur_v = bdd.clone();
        let mut cur_probe_dist = 0; // track how far the current item has probed
        loop {
            if self.is_occupied(pos as usize) {
                // If we find an element which has probed less than the current element, 
                // place the current element in that position and find a new place for the 
                // swapped element
                let this_bdd = self.get_pos(pos as usize);
                if this_bdd.low == bdd.low && this_bdd.high == bdd.high {
                    return () // we found the BDD in the table, do nothing
                } else {
                    // check if this item's position is closer than ours
                    let existing_dist = self.probe_distance(pos as usize);
                    if existing_dist < cur_probe_dist {
                        // swap out our position for this one
                        let tmp = self.get_pos(pos as usize);
                        self.set_value(pos as usize, this_bdd);
                        cur_v = tmp;
                        cur_probe_dist = existing_dist;
                    }
                }
                pos = (pos + 1) % (self.cap as TableIndex) // wrap to the beginning of the array
            } else {
                // place the element in the current spot, we're done
                self.set_value(pos as usize, cur_v.clone());
                return ()
            }
        }
        panic!("impossible state")
    }

    /// Insert a fresh BDD (without any RC), or find the existing one 
    /// Invalidates references
    fn insert_fresh(&mut self, low: BddPtr, high: BddPtr) -> () {
        self.insert(ToplessBdd::new(low, high))
    }

    /// Finds the index for a particular bdd, none if it is not found
    /// Does not invalidate references.
    pub fn find(&self, bddlow: BddPtr, bddhigh: BddPtr) -> Option<BddPtr> {
        let mut pos = compute_pos(self.cap.clone(), bddlow.get_index(), bddhigh.get_index());
        loop {
            if self.is_occupied(pos as usize) {
                let cur = self.get_pos(pos as usize);
                if cur.low == bddlow && cur.high  == bddhigh {
                    return Some(BddPtr::new(self.var, self.tbl_id, pos));
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
    pub fn get(&self, bddlow: BddPtr, bddhigh: BddPtr) -> Option<ToplessBdd> {
        match self.find(bddlow, bddhigh) {
            Some(a) => Some(self.get_pos(a.get_index() as usize)),
            None => None
        }
    }

    /// Increase the capacity and deallocate the old hash table
    /// Invalidates references.
    fn grow(orig: MutRobinHoodTable) -> MutRobinHoodTable {
        assert!(orig.cap << 1 <= MAX_TABLE_SIZE);
        let new_cap = orig.cap << 1;
        // insert each element of the old vector into the new one
        let mut new_map = MutRobinHoodTable::new(new_cap, orig.var, orig.tbl_id);
        for i in 0..orig.cap {
            if orig.is_occupied(i as usize) {
                let b = orig.get_pos(i as usize);
                new_map.insert(b);
            }
        }
        new_map
    }

    /// converts an immutable table into a mutable table
    fn from_immut(tbl: ImmutRobinHoodTable) -> MutRobinHoodTable {
        MutRobinHoodTable {
            elem: tbl.elem,
            var: tbl.var,
            tbl_id: tbl.tbl_id,
            cap: tbl.cap,
            len: tbl.len
        }
    }
}

/// Immutable robin-hood table; only supports look-ups, no insertion
struct ImmutRobinHoodTable {
    elem: Vec<ToplessBdd>,
    var: VarLabel,
    tbl_id: TableIndex,
    cap: TableIndex,
    len: TableIndex
}

impl ImmutRobinHoodTable {
    pub fn from_mut(tbl: MutRobinHoodTable) -> ImmutRobinHoodTable {
        ImmutRobinHoodTable {
            elem: tbl.elem,
            var: tbl.var,
            tbl_id: tbl.tbl_id,
            cap: tbl.cap,
            len: tbl.len
        }
    }

    pub fn new(var: VarLabel, tbl_id: TableIndex) -> ImmutRobinHoodTable {
        ImmutRobinHoodTable {
            elem: Vec::new(),
            var: var,
            tbl_id: tbl_id,
            cap: 0, 
            len: 0
        }
    }

    /// check if item at index `pos` is occupied
    fn is_occupied(&self, pos: usize) -> bool {
        self.elem[pos].level > CACHE_EMPTY
    }

    /// get the BDD at `pos`
    fn get_pos(&self, pos: usize) -> ToplessBdd {
        self.elem[pos].clone()
    }

    /// Finds the index for a particular bdd, none if it is not found
    /// Does not invalidate references.
    pub fn find(&self, bddlow: BddPtr, bddhigh: BddPtr) -> Option<BddPtr> {
        let mut pos = compute_pos(self.cap.clone(), bddlow.get_index(), bddhigh.get_index());
        loop {
            if self.is_occupied(pos as usize) {
                let cur = self.get_pos(pos as usize);
                if cur.low == bddlow && cur.high  == bddhigh {
                    return Some(BddPtr::new(self.var, self.tbl_id, pos));
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
    pub fn get(&self, bddlow: BddPtr, bddhigh: BddPtr) -> Option<ToplessBdd> {
        match self.find(bddlow, bddhigh) {
            Some(a) => Some(self.get_pos(a.get_index() as usize)),
            None => None
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Linear table

/// Implements a linear hash table with stable storage. Does not resize, elements are 
/// never moved once they are placed.
struct LinearBddTable {
    elem: Vec<ToplessBdd>,
    cap: TableIndex,
    var: VarLabel,
    tbl_id: TableIndex,
    len: TableIndex
}

impl LinearBddTable {
    fn new(sz : TableIndex, var: VarLabel, tbl_id: TableIndex) -> LinearBddTable {
        let mut r = LinearBddTable {
                elem: Vec::with_capacity(sz as usize),
                cap: sz,
                var: var,
                tbl_id: tbl_id,
                len: 0
        };
        // zero the vector and set its length
        unsafe {
            let vec_ptr = r.elem.as_mut_ptr();
            ptr::write_bytes(vec_ptr, 0, sz as usize);
            r.elem.set_len(sz as usize);
        }
        r
    }

    fn get_pos(&self, pos: usize) -> ToplessBdd {
        self.elem[pos].clone()
    }

    fn set_pos(&mut self, pos: usize, bdd: ToplessBdd) -> () {
        self.elem[pos] = bdd
    }

    fn is_occupied(&self, pos: usize) -> bool {
        let b = self.get_pos(pos);
        b.level > CACHE_EMPTY
    }

    /// either fetches an existing BDD at the location, or inserts a fresh BDD at
    /// that location
    fn get_or_insert(&mut self, bddlow: BddPtr, bddhigh: BddPtr) -> BddPtr {
        assert!((self.elem.len() + 1) as f64 * LINEAR_LOAD_FACTOR < self.cap as f64);
        let mut pos = compute_pos(self.cap, bddlow.get_index(), bddhigh.get_index());
        // probe forward from pos until an open slot is found, or we find the
        // inserted BDD
        loop {
            let b = self.get_pos(pos as usize);
            if b.level == CACHE_EMPTY {
                // found an empty slot, insert our guy
                let n = ToplessBdd::new(bddlow.clone(), bddhigh.clone());
                self.set_pos(pos as usize, n.clone());
                self.len += 1;
                return BddPtr::new(self.var, self.tbl_id, pos)
            } else {
                if b.low == bddlow && b.high == bddhigh {
                    return BddPtr::new(self.var, self.tbl_id, pos)
                } else { /* do nothing */ }
            }
            pos = (pos + 1) % (self.cap);
        }
    }

    fn find(&self, bddlow: BddPtr, bddhigh: BddPtr) -> Option<BddPtr> {
        let mut pos = compute_pos(self.cap, bddlow.get_index(), bddhigh.get_index());
        // probe forward from pos until an open slot is found, or we find the
        // inserted BDD
        loop {
            let b = self.get_pos(pos as usize);
            if b.level == CACHE_EMPTY {
                // found an empty slot, element not found
                return None
            } else {
                if b.low == bddlow && b.high == bddhigh {
                    return Some(BddPtr::new(self.var, self.tbl_id, pos))
                } else { /* do nothing */ }
            }
            pos = (pos + 1) % (self.cap);
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
/// SubTable, stores a single variable

/// Stores the table of all generated BDDs for a particular variable
/// 
/// Each BDD is initially inserted into the short_store. Then, when garbage is 
/// collected, BDDs are transferred into the main_store.
pub struct BddSubTable {
    main_store: ImmutRobinHoodTable,
    short_store: Vec<LinearBddTable>,
    var: VarLabel
}

impl BddSubTable {
    fn new(var: VarLabel) -> BddSubTable {
        BddSubTable {
            main_store: ImmutRobinHoodTable::new(var, 0),
            short_store: vec!(LinearBddTable::new(DEFAULT_LINEAR_SIZE, var, 1)),
            var: var
        }
    }

    pub fn find(&self, bddlow: BddPtr, bddhigh: BddPtr) -> Option<BddPtr> {
        // check if the bdd is in the main store
        match self.main_store.find(bddlow.clone(), bddhigh.clone()) {
            Some(a) => return Some(a),
            None => {
                // look in each short_store
                for i in 0..self.short_store.len() - 1 {
                    match self.short_store[i].find(bddlow.clone(), bddhigh.clone()) {
                        Some(a) => return Some(a),
                        None => {
                            ()
                        }
                    }
                }
            }
        }
        return None
    }

    /// Inserts an element into the BDD table without moving any other elements
    pub fn stable_insert(&mut self, bddlow: BddPtr, bddhigh: BddPtr) -> BddPtr {
        match self.find(bddlow.clone(), bddhigh.clone()) {
            Some(a) => a,
            None => {
                let l = self.short_store.last().unwrap().elem.len() as usize;
                let cap = self.short_store[l].cap as f64;
                if (l as f64) < (LINEAR_LOAD_FACTOR * cap) {
                    self.short_store[l].get_or_insert(bddlow, bddhigh)
                } else {
                    // allocate a new short store and push into it
                    self.short_store.push(
                        LinearBddTable::new(DEFAULT_LINEAR_SIZE, self.var, (l + 1) as TableIndex));
                    self.short_store[l+1].get_or_insert(bddlow, bddhigh)
                }
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
/// Core table, stores all variables for a BDD


////////////////////////////////////////////////////////////////////////////////
/// Tests
fn mk_ptr(i: TableIndex) -> BddPtr {
    BddPtr {var: 0, idx: i}
}

#[test]
fn test_simple_rh_insert() {
    let mut cell = MutRobinHoodTable::new(DEFAULT_RH_SIZE, 0, 0);
    cell.insert_fresh(mk_ptr(0), mk_ptr(0));
    cell.insert_fresh(mk_ptr(0), mk_ptr(0));
    cell.insert_fresh(mk_ptr(0), mk_ptr(1));
    cell.insert_fresh(mk_ptr(1), mk_ptr(0));
    // cell.incref(mk_ptr(0), mk_ptr(0));
    let r = match cell.get(mk_ptr(0), mk_ptr(0)) {
        Some(a) => a,
        _ => panic!("could not find")
    };
    assert_eq!(r.level, CACHE_DEFAULT);
}

#[test]
fn test_many_rh_insert() {
    let mut cell = MutRobinHoodTable::new(DEFAULT_RH_SIZE, 0, 0);
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
fn test_grow_rh() {
    let mut cell = MutRobinHoodTable::new(8192, 0, 0);
    println!("initial");
    for i in 0..3000 {
        cell.insert_fresh(mk_ptr(i), mk_ptr(i));
        let r1 = cell.get(mk_ptr(i), mk_ptr(i));
        cell.insert_fresh(mk_ptr(i), mk_ptr(i));
        let r2 = cell.get(mk_ptr(i), mk_ptr(i));
        assert_eq!(r1, r2);
    }
    println!("growing");
    cell = MutRobinHoodTable::grow(cell);
    for i in 0..6000 {
        cell.insert_fresh(mk_ptr(i), mk_ptr(i));
        let r1 = cell.get(mk_ptr(i), mk_ptr(i));
        cell.insert_fresh(mk_ptr(i), mk_ptr(i));
        let r2 = cell.get(mk_ptr(i), mk_ptr(i));
        assert_eq!(r1, r2);
    }
    cell = MutRobinHoodTable::grow(cell);
    for i in 0..10000{
        cell.insert_fresh(mk_ptr(i), mk_ptr(i));
        let r1 = cell.get(mk_ptr(i), mk_ptr(i));
        cell.insert_fresh(mk_ptr(i), mk_ptr(i));
        let r2 = cell.get(mk_ptr(i), mk_ptr(i));
        assert_eq!(r1, r2);
    }
}

#[test]
fn test_linear_tbl() {
    let mut tbl = LinearBddTable::new(DEFAULT_LINEAR_SIZE, 0, 0);
    for i in 0..DEFAULT_LINEAR_SIZE {
        tbl.get_or_insert(mk_ptr(i), mk_ptr(i));
        tbl.find(mk_ptr(i), mk_ptr(i));
    }
}