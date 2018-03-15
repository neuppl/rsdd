#[macro_use] mod util;
pub mod manager;
pub mod repr;
mod backing_store;

extern crate rand;
extern crate fnv;
extern crate twox_hash;
extern crate quickersort;
extern crate fasthash;
extern crate dimacs;
extern crate pretty;
extern crate num;
#[macro_use] extern crate maplit;
