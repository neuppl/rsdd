//! Apply cache for BDD operations that stores all ITEs

use crate::{
    builder::cache::{Ite, IteTable},
    repr::DDNNFPtr,
};
use rustc_hash::FxHashMap;

/// An Ite structure, assumed to be in standard form.
/// The top-level data structure that caches applications
#[derive(Debug)]
pub struct AllIteTable<T> {
    /// a vector of applications, indexed by the top label of the first pointer.
    table: FxHashMap<(T, T, T), T>,
}

impl<'a, T: DDNNFPtr<'a>> IteTable<'a, T> for AllIteTable<T> {
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

    fn get(&self, ite: Ite<T>, _hash: u64) -> Option<T> {
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

impl<'a, T: DDNNFPtr<'a>> AllIteTable<T> {
    fn new() -> AllIteTable<T> {
        AllIteTable {
            table: FxHashMap::default(),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&(T, T, T), &T)> + '_ {
        self.table.iter()
    }

    pub fn insert_directly(&mut self, k: (T, T, T), v: T) {
        self.table.insert(k, v);
    }
}

impl<'a, T: DDNNFPtr<'a>> Default for AllIteTable<T> {
    fn default() -> AllIteTable<T> {
        Self::new()
    }
}
