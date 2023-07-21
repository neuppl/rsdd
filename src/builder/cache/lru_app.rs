//! Apply cache for ITEs that uses a dynamically-expanding LRU cache
use crate::{
    builder::cache::{Ite, IteTable},
    repr::ddnnf::DDNNFPtr,
    util::lru::*,
};
use rustc_hash::FxHasher;
use std::hash::{Hash, Hasher};

const INITIAL_CAPACITY: usize = 16; // given as a power of two

/// The top-level data structure that caches applications
pub struct LruIteTable<T: Eq + PartialEq + Clone + Hash + std::fmt::Debug> {
    /// a vector of applications, indexed by the top label of the first pointer.
    table: Lru<(T, T, T), T>,
}

impl<'a, T: DDNNFPtr<'a>> IteTable<'a, T> for LruIteTable<T> {
    /// Insert an ite (f, g, h) into the apply table
    fn insert(&mut self, ite: Ite<T>, res: T, hash: u64) {
        match ite {
            Ite::IteChoice { f, g, h } | Ite::IteComplChoice { f, g, h } => {
                let compl = ite.is_compl_choice();
                self.table
                    .insert((f, g, h), if compl { res.neg() } else { res }, hash);
            }
            Ite::IteConst(_) => (), // do not cache base-cases
        }
    }

    fn get(&self, ite: Ite<T>, hash: u64) -> Option<T> {
        match ite {
            Ite::IteChoice { f, g, h } | Ite::IteComplChoice { f, g, h } => {
                let r = self.table.get((f, g, h), hash);
                let compl = ite.is_compl_choice();
                if compl {
                    r.map(|v| v.neg())
                } else {
                    r
                }
            }
            Ite::IteConst(f) => Some(f),
        }
    }

    fn hash(&self, ite: &Ite<T>) -> u64 {
        match ite {
            Ite::IteChoice { f, g, h } | Ite::IteComplChoice { f, g, h } => {
                let mut hasher: FxHasher = Default::default();
                f.hash(&mut hasher);
                g.hash(&mut hasher);
                h.hash(&mut hasher);
                hasher.finish()
            }
            Ite::IteConst(_) => 0, // do not cache base-cases
        }
    }
}

impl<'a, T: DDNNFPtr<'a>> LruIteTable<T> {
    fn new() -> LruIteTable<T> {
        LruIteTable {
            table: Lru::new(INITIAL_CAPACITY),
        }
    }
}

impl<'a, T: DDNNFPtr<'a>> Default for LruIteTable<T> {
    fn default() -> Self {
        Self::new()
    }
}
