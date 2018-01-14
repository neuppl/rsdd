use apply_cache::*;
use bdd::*;

const INITIAL_CAPACITY: usize = 17; // given as a power of two

/// The top-level data structure which caches applications
pub struct BddApplyTable {
    /// a table of Ite triples
    table: Vec<SubTable<(BddPtr, BddPtr), BddPtr>>,
}

impl BddApplyTable {
    pub fn new(num_vars: usize) -> BddApplyTable {
        let mut tbl = BddApplyTable {
            table: Vec::with_capacity(num_vars),
        };
        for _ in 0..num_vars {
            tbl.table.push(SubTable::new(INITIAL_CAPACITY));
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
