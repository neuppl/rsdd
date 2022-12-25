use crate::repr::{bdd::BddPtr, ddnnf::DDNNFPtr};

use self::ite::Ite;

pub mod all_app;
pub mod lru_app;
pub mod ite;
pub mod sdd_apply_cache;

pub trait LruTable<T: DDNNFPtr> {
    fn insert(&mut self, ite: Ite<T>, res: T);
    fn get(&mut self, ite: Ite<T>) -> Option<T>;
}
