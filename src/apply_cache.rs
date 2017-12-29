//! Stores BDD applications in an LRU cache.
use bdd::*;
use util::*;
use std::hash::{Hasher, Hash};
use twox_hash::XxHash;
use fnv::FnvHasher;

const LOAD_FACTOR: f64 = 0.7;
const INITIAL_CAPACITY: usize = 16392;
const GROWTH_RATE: usize = 16;
const MAX_OFFSET: usize = 8;

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct ApplyOp(pub Op, pub BddPtr, pub BddPtr);

/// Data structure stored in the subtables
#[derive(Debug, Hash, Clone)]
struct Element<K, V>
where
    K: Hash + Clone + Eq + PartialEq,
    V: Eq + PartialEq + Clone,
{
    key: K,
    val: V,
    occupied: bool,
    offset: u32,
}

impl<K, V> Element<K, V>
where
    K: Hash + Clone + Eq + PartialEq,
    V: Eq + PartialEq + Clone,
{
    fn new(key: K, val: V) -> Element<K, V> {
        Element {
            key: key,
            val: val,
            occupied: true,
            offset: 0,
        }
    }
}

/// Each variable has an associated sub-table
pub struct SubTable<K, V>
where
    K: Hash + Clone + Eq + PartialEq,
    V: Eq + PartialEq + Clone,
{
    tbl: Vec<Element<K, V>>,
    len: usize,
    cap: usize,
}

struct SubTableIter<'a, K, V>
where
    K: Hash + Clone + Eq + PartialEq + 'a,
    V: Eq + PartialEq + Clone + 'a,
{
    tbl: &'a SubTable<K, V>,
    pos: usize,
}

impl<'a, K, V> Iterator for SubTableIter<'a, K, V>
where
    K: Hash + Clone + Eq + PartialEq + 'a,
    V: Eq + PartialEq + Clone + 'a,
{
    type Item = &'a Element<K, V>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.tbl.tbl.len() {
            None
        } else {
            self.pos += 1;
            let itm = self.tbl.tbl[self.pos - 1].clone();
            if itm.occupied {
                Some(&self.tbl.tbl[self.pos - 1])
            } else {
                self.next()
            }
        }
    }
}

#[derive(PartialEq, Eq)]
enum InsertResult {
    InsufficientSpace,
    MaxProbeHit,
    Ok,
}

impl<K, V> SubTable<K, V>
where
    K: Hash + Clone + Eq + PartialEq,
    V: Eq + PartialEq + Clone,
{
    pub fn new(minimum_size: usize) -> SubTable<K, V> {
        let tbl_sz = ((minimum_size as f64 * (1.0 + LOAD_FACTOR)) as usize).next_power_of_two();
        let v: Vec<Element<K, V>> = zero_vec(tbl_sz);
        SubTable {
            tbl: v,
            len: 0,
            cap: tbl_sz,
        }
    }

    pub fn insert(&mut self, key: K, val: V) -> () {
        let mut cur_status = InsertResult::MaxProbeHit;
        while cur_status != InsertResult::Ok {
            let r = self.insert_helper(key.clone(), val.clone());
            match r {
                InsertResult::Ok => cur_status = InsertResult::Ok,
                InsertResult::InsufficientSpace => {
                    let mut grow_status = InsertResult::MaxProbeHit;
                    while grow_status != InsertResult::Ok {
                        grow_status = self.grow();
                    }
                }
                InsertResult::MaxProbeHit => {
                    let mut grow_status = InsertResult::MaxProbeHit;
                    while grow_status != InsertResult::Ok {
                        grow_status = self.grow();
                    }
                }
            }
        }
    }

    fn insert_helper(&mut self, key: K, val: V) -> InsertResult {
        if (self.len + 1) as f64 > (self.cap as f64 * LOAD_FACTOR) {
            return InsertResult::InsufficientSpace;
        }

        let mut hasher = XxHash::default();
        key.hash(&mut hasher);
        let hash_v = hasher.finish();
        let mut pos = (hash_v as usize) % self.cap;
        let mut searcher = Element::new(key.clone(), val.clone());
        let mut cur_status = InsertResult::Ok;
        loop {
            if self.tbl[pos].occupied {
                // first, check if they are equal.
                if self.tbl[pos].key == key {
                    return cur_status;
                } else {
                }
                // they are not equal, see if we should swap for the closer one
                if self.tbl[pos].offset < searcher.offset {
                    // swap the searcher with the current element
                    let tmp = searcher;
                    searcher = self.tbl[pos].clone();
                    self.tbl[pos] = tmp;
                } else {
                    searcher.offset += 1;
                }

                if searcher.offset > MAX_OFFSET as u32 {
                    cur_status = InsertResult::MaxProbeHit;
                }
            } else {
                // found an open spot, insert
                self.tbl[pos] = searcher;
                self.len += 1;
                return cur_status;
            }
            pos = (pos + 1) % self.cap;
        }
    }

    fn iter<'a>(&'a self) -> SubTableIter<'a, K, V> {
        SubTableIter { tbl: self, pos: 0 }
    }

    pub fn get(&self, key: K) -> Option<V> {
        let mut hasher = XxHash::default();
        key.hash(&mut hasher);
        let hash_v = hasher.finish();
        let mut pos = (hash_v as usize) % self.cap;
        for _ in 0..MAX_OFFSET {
            if self.tbl[pos].key == key {
                return Some(self.tbl[pos].val.clone());
            }
            pos = (pos + 1) % self.cap;
        }
        return None;
    }

    /// grow the hashtable to accomodate more elements
    fn grow(&mut self) -> InsertResult {
        let new_sz = self.cap * GROWTH_RATE;
        let new_v = zero_vec(new_sz);
        let mut new_tbl = SubTable {
            tbl: new_v,
            len: 0,
            cap: new_sz,
        };

        for i in self.iter() {
            match new_tbl.insert_helper(i.key.clone(), i.val.clone()) {
                InsertResult::Ok => (),
                InsertResult::InsufficientSpace => panic!("growth rate too low"),
                InsertResult::MaxProbeHit => {
                    return InsertResult::MaxProbeHit;
                }
            }
        }

        // copy new_tbl over the current table
        self.tbl = new_tbl.tbl;
        self.cap = new_tbl.cap;
        self.len = new_tbl.len;
        return InsertResult::Ok;
    }

    fn avg_offset(&self) -> f64 {
        let mut offs: usize = 0;
        for i in self.iter() {
            offs += i.offset as usize;
        }
        offs as f64 / (self.len as f64)
    }
}

/// The top-level data structure which caches applications
pub struct BddApplyTable {
    or_tables: Vec<SubTable<(BddPtr, BddPtr), BddPtr>>,
    and_tables: Vec<SubTable<(BddPtr, BddPtr), BddPtr>>,
}

impl BddApplyTable {
    pub fn new(num_vars: usize) -> BddApplyTable {
        let mut tbl = BddApplyTable {
            or_tables: Vec::with_capacity(num_vars),
            and_tables: Vec::with_capacity(num_vars),
        };
        for _ in 0..num_vars {
            tbl.or_tables.push(SubTable::new(INITIAL_CAPACITY));
            tbl.and_tables.push(SubTable::new(INITIAL_CAPACITY));
        }
        tbl
    }

    /// Insert an operation into the apply table. Note that operations are
    /// normalized by first sorting the sub-BDDs such that BDD A occurs first
    /// in the ordering; this increases cache hit rate and decreases duplicate
    /// storage
    pub fn insert(&mut self, op: ApplyOp, res: BddPtr) -> () {
        let ApplyOp(op, a, b) = op;
        let tbl = a.var() as usize;
        match op {
            Op::BddAnd => self.and_tables[tbl].insert((a, b), res),
            Op::BddOr => self.or_tables[tbl].insert((a, b), res),
        }
    }

    pub fn get(&self, op: ApplyOp) -> Option<BddPtr> {
        let ApplyOp(op, a, b) = op;
        let tbl = a.var() as usize;
        match op {
            Op::BddAnd => self.and_tables[tbl].get((a, b)),
            Op::BddOr => self.or_tables[tbl].get((a, b)),
        }
    }
}

#[test]
fn apply_cache_simple() {
    let mut tbl = BddApplyTable::new(10);
    for var in 0..10 {
        for i in 0..100000 {
            let op = ApplyOp(
                Op::BddAnd,
                BddPtr::new(VarLabel::new(var), TableIndex::new(i)),
                BddPtr::new(VarLabel::new(var + 1), TableIndex::new(i)),
            );
            let result = BddPtr::new(VarLabel::new(var), TableIndex::new(i));
            tbl.insert(op, result);
        }
    }
    for var in 0..10 {
        for i in 0..100000 {
            let op = ApplyOp(
                Op::BddAnd,
                BddPtr::new(VarLabel::new(var), TableIndex::new(i)),
                BddPtr::new(VarLabel::new(var + 1), TableIndex::new(i)),
            );
            let result = BddPtr::new(VarLabel::new(var), TableIndex::new(i));
            assert_eq!(tbl.get(op).unwrap(), result);
        }
    }
}
