//! Internal module for unique tables, used for memoizing decision diagram nodes
use std::hash::Hash;

struct UniquePtr<T: Hash + Eq + PartialEq>(*const T);

/// Core UniqueTable trait that stores and memoizes unique nodes
trait UniqueTable<T: Hash + Eq + PartialEq> {
    fn insert(t: T) -> UniquePtr<T>;
    fn get(t: &UniquePtr<T>) -> &T;
}