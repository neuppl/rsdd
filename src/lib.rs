//! Defines exports and the C api
extern crate dimacs;
extern crate pretty;
extern crate primal;
extern crate rand;
#[macro_use]
extern crate serde;
extern crate bit_set;
extern crate bumpalo;
extern crate petgraph;
extern crate quickcheck;
extern crate rand_chacha;
extern crate rustc_hash;
extern crate segment_tree;

#[macro_use]
pub mod util;
mod backing_store;
pub mod builder;
pub mod constants;
pub mod plan;
pub mod repr;
pub mod serialize;

#[cfg(target_arch = "wasm32")]
pub mod wasm;
