//! A closed-address cuckoo backing store.

use backing_store::*;
use util::zero_vec;
use std::hash::{Hasher, Hash};
use fnv::FnvHasher;
use std::fmt::Debug;

/// the size of each sub-cuckoo-table, as a power of two
const TBL_SZ: usize = 19;
/// number of sub-tables
const NUM_TBL: usize = 2;

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

struct StoreElem<T> {
    cur: T,
    /// the next T in the chain from the cuckoo if it is 0, then there is no
    /// next element
    next: Option<u32>,
}

#[inline]
fn rehash(v: usize) -> usize {
    let mut hasher = FnvHasher::default();
    v.hash(&mut hasher);
    hasher.finish() as usize
    // knuth variant on division
    // v.wrapping_mul(v.wrapping_add(3))
}


struct Elem<T>
where
    T: Hash + PartialEq + Eq + Clone + Debug,
{
    e: T,
    next: Option<BackingPtr>,
}

impl<T> Elem<T>
where
    T: Hash + PartialEq + Eq + Clone + Debug,
{
    fn empty(e: T) -> Elem<T> {
        Elem {
            e: e,
            next: Option::None,
        }
    }

    fn with_next(e: T, next: BackingPtr) -> Elem<T> {
        Elem {
            e: e,
            next: Some(next),
        }
    }
}

pub struct CuckooStore<T>
where
    T: Hash + PartialEq + Eq + Clone + Debug,
{
    /// hash table which stores indexes in the elem vector
    tbl: [Vec<Option<BackingPtr>>; NUM_TBL],
    /// backing store for BDDs
    store: Vec<Elem<T>>,
    stats: BackingCacheStats,
}

impl<T> CuckooStore<T>
where
    T: Hash + PartialEq + Eq + Clone + Debug,
{
    pub fn new() -> CuckooStore<T> {
        CuckooStore {
            tbl: [
                zero_vec(1 << TBL_SZ),
                // zero_vec(1 << TBL_SZ),
                // zero_vec(1 << TBL_SZ),
                zero_vec(1 << TBL_SZ),
            ],
            store: Vec::with_capacity(1 << (TBL_SZ + 3)),
            stats: BackingCacheStats::new(),
        }
    }

    /// push a new element into the backing store
    fn push_elem(&mut self, elem: Elem<T>) -> BackingPtr {
        self.store.push(elem);
        BackingPtr((self.store.len() - 1) as u32)
    }

    pub fn deref(&self, ptr: BackingPtr) -> T {
        self.store[ptr.0 as usize].e.clone()
    }

    fn deref_elem(&self, ptr: BackingPtr) -> &Elem<T> {
        &self.store[ptr.0 as usize]
    }


    pub fn get_or_insert(&mut self, elem: T) -> BackingPtr {
        let mut hasher = FnvHasher::default();
        elem.hash(&mut hasher);
        let mut hash_v = hasher.finish() as usize;
        // cur_idx tracks the current index in each sub-table to begin
        // searching from
        let mut cur_idx: [BackingPtr; NUM_TBL] = [BackingPtr(0),
                                                  // BackingPtr(0),
                                                  // BackingPtr(0),
                                                  BackingPtr(0),
        ];
        // index into the hash table for updates
        let mut hash_idx : [usize; NUM_TBL] = [0, 0];
        for idx in 0..NUM_TBL {
            hash_v = rehash(hash_v);
            let loc = pow_cap(hash_v, TBL_SZ);
            hash_idx[idx] = loc;
            let v1 = self.tbl[idx][loc].clone();
            if v1.is_none() {
                // element doesn't exist, insert
                let store_loc = self.push_elem(Elem::empty(elem));
                self.tbl[idx][loc as usize] = Some(store_loc);
                return store_loc;
            } else {
                // check if we found the element
                let e = self.deref(v1.unwrap());
                if e == elem {
                    return v1.unwrap();
                } else {
                    cur_idx[idx] = v1.unwrap();
                }
            }
        }
        loop {
            // advance each pointer in the order of the tables, and insert at
            // the first location possible
            for i in 0..NUM_TBL {
                // advance the pointer
                if self.store[cur_idx[i].0 as usize].next.is_none() {
                    let cur_v = self.tbl[i][hash_idx[i]].unwrap();
                    let new_v = self.push_elem(Elem::with_next(elem, cur_v));
                    // push the new value to the front
                    self.tbl[i][hash_idx[i]] = Some(new_v);
                    return new_v;
                } else {
                    let idx = self.store[cur_idx[i].0 as usize].next.unwrap();
                    if self.deref(idx) == elem {
                        return idx;
                    } else {
                        cur_idx[i] = idx;
                    }
                }
            }
        }
    }

    /// the percentage of occupied buckets in each subtable
    fn fill_ratio(&self) -> f64 {
        let mut cnt = 0;
        let total = (1 << TBL_SZ) * NUM_TBL;
        for tbl in self.tbl.iter() {
            for v in tbl.iter() {
                if v.is_some() {
                    cnt += 1;
                }
            }
        }
        (cnt as f64) / (total as f64)
    }

    /// length of a chain beginning at `ptr`
    fn chain_len(&self, ptr: BackingPtr) -> usize {
        let mut cur = self.deref_elem(ptr);
        let mut cnt = 0;
        while cur.next.is_some() {
            cur = self.deref_elem(cur.next.unwrap());
            cnt += 1;
        }
        cnt
    }

    /// the longest chain
    fn max_chain(&self) -> usize {
        let mut max = 0;
        for i in 0..self.store.len() {
            let cnt = self.chain_len(BackingPtr(i as u32));
            if cnt > max {
                max = cnt;
            }
        }
        max
    }

    fn avg_chain(&self) -> f64 {
        let mut total_chain = 0;
        let mut num_elem = 0;
        for tbl in self.tbl.iter() {
            for elem in tbl.iter() {
                if elem.is_some() {
                    total_chain += self.chain_len(elem.unwrap());
                    num_elem += 1;
                }
            }
        }
        (total_chain as f64) / (num_elem as f64)
    }

    pub fn num_nodes(&self) -> usize {
        self.store.len()
    }


    pub fn get_stats(&self) -> BackingCacheStats {
        BackingCacheStats::new()
    }
}


#[test]
fn cuckoo_simple() {
    use repr::bdd::{BddPtr, TableIndex, ToplessBdd};
    use repr::var_label::VarLabel;
    fn mk_ptr(idx: u64) -> BddPtr {
        BddPtr::new(VarLabel::new(0), TableIndex::new(idx))
    }
    let mut store: CuckooStore<ToplessBdd> = CuckooStore::new();
    for i in 0..1000000 {
        let e = ToplessBdd::new(mk_ptr(i), mk_ptr(i));
        let v = store.get_or_insert(e.clone());
        let v_res = store.get_or_insert(e);
        assert_eq!(v, v_res);
        assert_eq!(store.deref(v), store.deref(v_res));
    }
    println!("fill: {}", store.fill_ratio());
    println!("max chain: {}", store.max_chain());
    println!("avg chain: {}", store.avg_chain());

}
