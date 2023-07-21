use crate::repr::ddnnf::DDNNFPtr;

use self::ite::Ite;

pub mod all_app;
pub mod ite;
pub mod lru_app;

pub trait IteTable<'a, T: DDNNFPtr<'a>> {
    fn hash(&self, ite: &Ite<T>) -> u64;
    fn insert(&mut self, ite: Ite<T>, res: T, hash: u64);
    fn get(&self, ite: Ite<T>, hash: u64) -> Option<T>;
}
