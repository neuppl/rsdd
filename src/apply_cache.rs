//! Stores BDD applications in an LRU cache.
use bdd::*;
use std::fmt::Debug;
use util::*;
use std::hash::{Hasher, Hash};
use twox_hash::XxHash;
use fnv::FnvHasher;

const LOAD_FACTOR: f64 = 0.96;
const MAX_OFFSET: usize = 6;

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct ApplyCacheStats {
    pub lookup_count: usize,
    pub miss_count: usize,
}

impl ApplyCacheStats {
    pub fn new() -> ApplyCacheStats {
        ApplyCacheStats {
            miss_count: 0,
            lookup_count: 0,
        }
    }
}

#[inline]
fn mask(p: usize) -> usize {
    (1 << p) - 1
}

#[inline]
fn wrap_add(v: usize, p: usize) -> usize {
    (v + 1) & mask(p)
}

/// cap `v` at 2^`p`
#[inline]
fn pow_cap(v: usize, p: usize) -> usize {
    v & mask(p)
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct ApplyOp(pub Op, pub BddPtr, pub BddPtr);

/// Data structure stored in the subtables
#[derive(Debug, Hash, Clone)]
struct Element<K, V>
where
    K: Hash + Clone + Eq + PartialEq + Debug,
    V: Eq + PartialEq + Clone,
{
    key: K,
    val: V,
    occupied: bool,
    offset: u8,
}

impl<K, V> Element<K, V>
where
    K: Hash + Clone + Eq + PartialEq + Debug,
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
    K: Hash + Clone + Eq + PartialEq + Debug,
    V: Eq + PartialEq + Clone,
{
    tbl: Vec<Element<K, V>>,
    len: usize,
    cap: usize, // a particular power of 2
    stat: ApplyCacheStats,
}

struct SubTableIter<'a, K, V>
where
    K: Hash + Clone + Eq + PartialEq + Debug + 'a,
    V: Eq + PartialEq + Clone + 'a,
{
    tbl: &'a SubTable<K, V>,
    pos: usize,
}

impl<'a, K, V> Iterator for SubTableIter<'a, K, V>
where
    K: Hash + Clone + Eq + PartialEq + Debug + 'a,
    V: Eq + PartialEq + Clone + 'a,
{
    type Item = &'a Element<K, V>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.tbl.tbl.len() {
            None
        } else {
            self.pos += 1;
            if self.tbl.tbl[self.pos - 1].occupied {
                Some(&self.tbl.tbl[self.pos - 1])
            } else {
                self.next()
            }
        }
    }
}

impl<K, V> SubTable<K, V>
where
    K: Hash + Clone + Eq + PartialEq + Debug,
    V: Eq + PartialEq + Clone,
{
    /// create a new bdd cache with capacity `cap`, given as a power of 2
    pub fn new(cap: usize) -> SubTable<K, V> {
        let v: Vec<Element<K, V>> = zero_vec(1 << cap);
        SubTable {
            tbl: v,
            len: 0,
            cap: cap,
            stat: ApplyCacheStats::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    /// the capacity for this table (as a power of 2)
    pub fn cap(&self) -> usize {
        self.cap
    }


    pub fn insert(&mut self, key: K, val: V) -> () {
        if (self.len + 1) as f64 > ((1 << self.cap) as f64 * LOAD_FACTOR) {
            self.grow();
            return self.insert(key, val);
        }

        let mut hasher = FnvHasher::default();
        key.hash(&mut hasher);
        let hash_v = hasher.finish();
        let mut pos = pow_cap(hash_v as usize, self.cap);
        let mut searcher = Element::new(key.clone(), val.clone());
        loop {
            assert!(pos < (1 << self.cap));
            if searcher.offset > MAX_OFFSET as u8 {
                self.grow();
                return self.insert(searcher.key, searcher.val)
            }
            // println!("pos: {}, cap: {}", pos, 1 << self.cap);
            if self.tbl[pos].occupied {
                // first, check if they are equal.
                if self.tbl[pos].key == key {
                    return ();
                } else {
                }
                // they are not equal, see if we should swap for the closer one
                if self.tbl[pos].offset < searcher.offset {
                    // swap the searcher with the current element
                    let tmp = searcher;
                    searcher = self.tbl[pos].clone();
                    self.tbl[pos] = tmp;
                } else {
                }

            } else {
                // found an open spot, insert
                self.tbl[pos] = searcher;
                self.len += 1;
                return ();
            }
            searcher.offset += 1;
            pos = wrap_add(pos, self.cap)
        }
    }

    fn iter<'a>(&'a self) -> SubTableIter<'a, K, V> {
        SubTableIter { tbl: self, pos: 0 }
    }

    pub fn get(&mut self, key: K) -> Option<V> {
        // self.stat.lookup_count += 1;
        let mut hasher = FnvHasher::default();
        key.hash(&mut hasher);
        let hash_v = hasher.finish();
        let mut pos = pow_cap(hash_v as usize, self.cap);
        for _ in 0..MAX_OFFSET+1 {
            if self.tbl[pos].key == key {
                return Some(self.tbl[pos].val.clone());
            }
            pos = wrap_add(pos, self.cap);
        }
        // self.stat.miss_count += 1;
        return None;
    }

    /// grow the hashtable to accomodate more elements
    fn grow(&mut self) -> () {
        let new_sz = self.cap + 1;
        let new_v = zero_vec(1 << new_sz);
        let mut new_tbl = SubTable {
            tbl: new_v,
            len: 0,
            cap: new_sz,
            stat: ApplyCacheStats::new(),
        };

        for i in self.iter() {
            new_tbl.insert(i.key.clone(), i.val.clone());
        }

        // copy new_tbl over the current table
        self.tbl = new_tbl.tbl;
        self.cap = new_tbl.cap;
        self.len = new_tbl.len;
        // don't update the stats; we want to keep those
    }

    pub fn avg_offset(&self) -> f64 {
        let mut offs: usize = 0;
        if self.len == 0 {
            return 0.0;
        }

        for i in self.iter() {
            offs += i.offset as usize;
        }
        offs as f64 / (self.len as f64)
    }

    pub fn get_stats(&self) -> ApplyCacheStats {
        self.stat.clone()
    }
}

#[test]
fn test_cache() {
    let mut c : SubTable<u64, u64> = SubTable::new(14);
    for i in 0..10000 {
        c.insert(i, i);
    }
    for i in 0..10000 {
        if c.get(i).is_none() {
            panic!("could not find {}", i);
        }
        assert_eq!(c.get(i).unwrap(), i)
    }
}
