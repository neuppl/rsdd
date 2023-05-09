//! Defines exports and the C api
#![allow(clippy::all)]
extern crate dimacs;
extern crate dot;
extern crate fnv;
extern crate libc;
extern crate maplit;
extern crate num;
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
extern crate tinyvec;

#[macro_use]
pub mod util;
mod backing_store;
pub mod builder;
pub mod repr;
pub mod sample;
pub mod serialize;
pub mod unique_table;
pub mod wasm;
