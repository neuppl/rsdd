use crate::repr::DDNNFPtr;

mod all_app;
mod ite;
mod lru_app;

pub use self::all_app::*;
pub use self::ite::*;
pub use self::lru_app::*;

pub trait IteTable<'a, T: DDNNFPtr<'a>> {
    fn hash(&self, ite: &Ite<T>) -> u64;
    fn insert(&mut self, ite: Ite<T>, res: T, hash: u64);
    fn get(&self, ite: Ite<T>, hash: u64) -> Option<T>;
}
