//! Apply cache for BDD operations that stores all ITEs



use rustc_hash::FxHashMap;

use crate::repr::{ddnnf::DDNNFPtr};

use super::{ite::Ite, LruTable};

/// An Ite structure, assumed to be in standard form.
/// The top-level data structure that caches applications
pub struct AllTable<T: DDNNFPtr> {
    /// a vector of applications, indexed by the top label of the first pointer.
    table: FxHashMap<(T, T, T), T>,
}

impl<T: DDNNFPtr> LruTable<T> for AllTable<T> {
    fn hash(&self, _ite: &Ite<T>) -> u64 {
        // do nothing; the all-cache uses a hashbrown table that caches all applies
        0
    }

    /// Insert an ite (f, g, h) into the apply table
    fn insert(&mut self, ite: Ite<T>, res: T, _hash: u64) {
        match ite {
            Ite::IteChoice { f, g, h } | Ite::IteComplChoice { f, g, h } => {
                // convert the ITE into a canonical form
                let compl = ite.is_compl_choice();
                self.table
                    .insert((f, g, h), if compl { res.neg() } else { res });
            }
            Ite::IteConst(_) => (), // do not cache base-cases
        }
    }

    fn get(&mut self, ite: Ite<T>, _hash: u64) -> Option<T> {
        match ite {
            Ite::IteChoice { f, g, h } | Ite::IteComplChoice { f, g, h } => {
                let r = self.table.get(&(f, g, h));
                let compl = ite.is_compl_choice();
                if compl {
                    r.map(|v| v.neg())
                } else {
                    r.cloned()
                }
            }
            Ite::IteConst(f) => Some(f),
        }
    }
}

impl<T: DDNNFPtr> AllTable<T> {
    pub fn new() -> AllTable<T> {
        AllTable {
            table: FxHashMap::default(),
        }
    }
}
