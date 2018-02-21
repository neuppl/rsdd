//! Apply cache for BDD operations
use manager::cache::lru::*;
use repr::bdd::*;
use repr::var_label::VarLabel;

const INITIAL_CAPACITY: usize = 17; // given as a power of two

/// The top-level data structure which caches applications
pub struct BddApplyTable {
    /// a vector of applications, indexed by the top label of the first pointer.
    table: Vec<Lru<(BddPtr, BddPtr), BddPtr>>,
}

impl BddApplyTable {
    pub fn new(num_vars: usize) -> BddApplyTable {
        let mut tbl = BddApplyTable {
            table: Vec::with_capacity(num_vars),
        };
        for _ in 0..num_vars {
            tbl.table.push(Lru::new(INITIAL_CAPACITY));
        }
        tbl
    }

    /// Insert an operation into the apply table. Note that operations are
    /// normalized by first sorting the sub-BDDs such that BDD A occurs first
    /// in the ordering; this increases cache hit rate and decreases duplicate
    /// storage
    pub fn insert(&mut self, f: BddPtr, g: BddPtr, res: BddPtr) -> () {
        let tbl = f.var() as usize;
        self.table[tbl].insert((f, g), res);
    }

    pub fn get(&mut self, f: BddPtr, g: BddPtr) -> Option<BddPtr> {
        let tbl = f.var() as usize;
        self.table[tbl].get((f, g))
    }

    pub fn get_stats(&self) -> Vec<ApplyCacheStats> {
        let mut r = Vec::new();
        for tbl in self.table.iter() {
            r.push(tbl.get_stats());
        }
        r
    }

    /// Push a new application table to the back of the list
    pub fn new_last(&mut self) -> () {
        self.table.push(Lru::new(INITIAL_CAPACITY));
    }
}

#[test]
fn apply_cache_simple() {
    let mut tbl = BddApplyTable::new(11);
    for var in 0..10 {
        for i in 0..100000 {
            let f = BddPtr::new(VarLabel::new(var), TableIndex::new(i));
            let g = BddPtr::new(VarLabel::new(var + 1), TableIndex::new(i));
            let result = BddPtr::new(VarLabel::new(var), TableIndex::new(i));
            tbl.insert(f, g, result);
        }
    }
    for var in 0..10 {
        for i in 0..100000 {
            let f = BddPtr::new(VarLabel::new(var), TableIndex::new(i));
            let g = BddPtr::new(VarLabel::new(var + 1), TableIndex::new(i));
            let result = BddPtr::new(VarLabel::new(var), TableIndex::new(i));
            tbl.insert(f, g, result);
            assert_eq!(tbl.get(f, g).unwrap(), result);
        }
    }
}
