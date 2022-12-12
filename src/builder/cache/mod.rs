use crate::repr::bdd::BddPtr;

use self::ite::Ite;

pub mod bdd_lru_app;
pub mod bdd_all_app;
pub mod ite;

pub trait LruTable {
    fn push_table(&mut self);
    fn insert(&mut self, ite: Ite, res: BddPtr);
    fn get(&mut self, ite: Ite) -> Option<BddPtr>;
}