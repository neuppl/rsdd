//! Apply cache for ITEs that uses a dynamically-expanding LRU cache
use std::hash::Hasher;

use rustc_hash::FxHasher;

use crate::{
    repr::{ddnnf::DDNNFPtr},
    util::lru::*,
};

use super::{ite::Ite, LruTable};

const INITIAL_CAPACITY: usize = 16; // given as a power of two

/// The top-level data structure that caches applications
pub struct BddApplyTable<T: DDNNFPtr> {
    /// a vector of applications, indexed by the top label of the first pointer.
    table: Lru<(T, T, T), T>,
}

impl<T: DDNNFPtr> LruTable<T> for BddApplyTable<T> {
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

    fn get(&mut self, ite: Ite<T>, hash: u64) -> Option<T> {
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
                return hasher.finish();
            }
            Ite::IteConst(_) => 0, // do not cache base-cases
        }
    }
}

impl<T: DDNNFPtr> BddApplyTable<T> {
    pub fn new(_num_vars: usize) -> BddApplyTable<T> {
        BddApplyTable {
            table: Lru::new(INITIAL_CAPACITY),
        }
    }
}
