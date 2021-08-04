//! Apply cache for BDD operations
use manager::cache::lru::*;
use repr::bdd::*;
use repr::var_label::VarLabel;

const INITIAL_CAPACITY: usize = 20; // given as a power of two

/// An Ite structure, assumed to be in standard form.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Ite {
    pub f: BddPtr,
    pub g: BddPtr,
    pub h: BddPtr,
}

impl Ite {
    /// Returns a new Ite and a Bool indicating whether to complement the Ite
    pub fn new(f: BddPtr, g: BddPtr, h: BddPtr) -> (Ite, bool) {
        // standardize the ite
        // See pgs. 115-117 of "Algorithms and Data Structures in VLSI Design"
        // first, introduce constants if possible
        let (f, g, h) = match (f, g, h) {
            (f, g, h) if g == h => (f, BddPtr::true_node(), g),
            (f, g, h) if f == h => (f, g, BddPtr::false_node()),
            (f, g, h) if f == h.neg() => (f, g, BddPtr::true_node()),
            (f, g, h) if f == g.neg() => (f, BddPtr::false_node(), g),
            _ => (f, g, h)
        };

        // now, standardize for negation: ensure f and g are non-negated
        let (f, g, h, compl) = match(f, g, h) {
            (f, g, h) if f.is_compl() && !g.is_compl() => (f, h, g, false),
            (f, g, h) if !f.is_compl() && g.is_compl() => (f, g.neg(), h.neg(), true),
            (f, g, h) if f.is_compl() && g.is_compl() => (f.neg(), h.neg(), g.neg(), true),
            _ => (f, g, h, false)
        };
        (Ite {f, g, h}, compl)
    }
}

/// The top-level data structure that caches applications
pub struct BddApplyTable {
    /// a vector of applications, indexed by the top label of the first pointer.
    table: Lru<Ite, BddPtr>,
}

impl BddApplyTable {
    pub fn new() -> BddApplyTable {
        let tbl = BddApplyTable {
            table: Lru::new(INITIAL_CAPACITY),
        };
        tbl
    }

    /// Insert an ite (f, g, h) into the apply table
    pub fn insert(&mut self, f: BddPtr, g: BddPtr, h: BddPtr, res: BddPtr) -> () {
        let (ite, compl) = Ite::new(f, g, h);
        self.table.insert(ite, if compl {res.neg()} else {res});
    }

    pub fn get(&mut self, f: BddPtr, g: BddPtr, h: BddPtr) -> Option<BddPtr> {
        let (ite, compl) = Ite::new(f, g, h);
        let r = self.table.get(ite);
        if compl { r.map(|v| v.neg()) } else { r }
    }

    pub fn get_stats(&self) -> ApplyCacheStats {
        self.table.get_stats()
    }

    /// Push a new application table to the back of the list
    pub fn new_last(&mut self) -> () {
    }
}
