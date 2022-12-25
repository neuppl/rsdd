//! Apply cache for ITEs that uses a dynamically-expanding LRU cache
use crate::{repr::{bdd::BddPtr, ddnnf::DDNNFPtr}, util::lru::*};

use super::{ite::Ite, LruTable};

const INITIAL_CAPACITY: usize = 8; // given as a power of two

/// An Ite structure, assumed to be in standard form.

/// The top-level data structure that caches applications
pub struct BddApplyTable<T: DDNNFPtr> {
    /// a vector of applications, indexed by the top label of the first pointer.
    table: Lru<(T, T, T), T>,
}

impl<T: DDNNFPtr> LruTable<T> for BddApplyTable<T> {
    /// Insert an ite (f, g, h) into the apply table
    fn insert(&mut self, ite: Ite<T>, res: T) {
        match ite {
            Ite::IteChoice { f, g, h } | Ite::IteComplChoice { f, g, h } => {
                let compl = ite.is_compl_choice();
                self.table.insert((f, g, h), if compl { res.neg() } else { res });
            }
            Ite::IteConst(_) => (), // do not cache base-cases
        }
    }

    fn get(&mut self, ite: Ite<T>) -> Option<T> {
        match ite {
            Ite::IteChoice { f, g, h } | Ite::IteComplChoice { f, g, h } => {
                let r = self.table.get((f, g, h));
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
}

impl<T: DDNNFPtr> BddApplyTable<T> {
    pub fn new(num_vars: usize) -> BddApplyTable<T> {
        BddApplyTable {
            table: Lru::new(INITIAL_CAPACITY),
        }
    }
}
