[![Build](https://github.com/pmall-neu/rsdd/actions/workflows/rust.yml/badge.svg)](https://github.com/pmall-neu/rsdd/actions/workflows/rust.yml)

# RSDD

This library is a  `rust` implementation of decision diagrams ([binary decision
diagrams](https://en.wikipedia.org/wiki/Binary_decision_diagram) and [sentential
decision diagrams](http://reasoning.cs.ucla.edu/sdd/)). The goal of this library
is to be a *efficient*, *safe*, and *modern* implementation of decision
diagrams.  Ease of integration, performance, and ease of experimentation are
core design concerns: the hope is that this library will be a useful
platform for experimentation and iteration on new ideas for these important
data structures.

To add this in your rust project, use:

```toml
[dependencies]
rsdd = { git = "https://github.com/pmall-neu/rsdd" }
```

This project is under active research development; if you are interested in
contributing please contact [Steven Holtzen](http://web.cs.ucla.edu/~sholtzen/)
at `s.holtzen@northeastern.edu`.

## Building

After cloning the repository, the following commands are available:

* `cargo build`: build a dynamically linkable library in `target/debug/`
* `cargo build --release`: build a release build in `target/release`
* `cargo test`: run the test suite

# Example Usage

The following demo shows how to quickly build a BDD, after adding `rsdd` to your
project. The goal is to prove that, for the logical variables `v1`, `v2`, and
`v3`, it holds that `exists v2. v1 && v2 && v3 == v1 && v3`. We can prove this
using BDDs in the following way:

```rust
let mut man = BddManager::new_default_order(3); // create a new manager
let v1 = man.var(VarLabel::new(0), true);       // initialize v0 to be true
let v2 = man.var(VarLabel::new(1), true);
let v3 = man.var(VarLabel::new(2), true);
let a1 = man.and(v1, v2);                       // compute v1 && v2
let r1 = man.and(a1, v3);                       // compute v1 && v2 && v3
let r_expected = man.and(v1, v3);               // comput expected result: v1 && v3
let res = man.exists(r1, VarLabel::new(1));     // quantify out v2 from v1 && v2 && v3
assert!(
    man.eq_bdd(r_expected, res),
    "Got:\nOne: {}\nExpected: {}",
    man.print_bdd(res),                         // this prints the BDD if the assertion fails
    man.print_bdd(r_expected)
);
```

Further usage examples can be found in the tests in the manager files and in `tests/test.rs`.

# Features and comparison with existing libraries

This library is similar in purpose well-known decision diagram libraries like
[`cudd`](https://github.com/ivmai/cudd),
[`sylvan`](https://github.com/utwente-fmt/sylvan) and the [UCLA SDD
library](http://reasoning.cs.ucla.edu/sdd/). This library is quite different
from these for the following reasons:

* The API does not use reference counting for memory management, and instead
  relies on the user to actively relinquish decision diagram nodes.
* SDDs use complemented edges internally to minimize memory usage. In addition,
  SDDs specialize to use BDDs for right-linear v-trees.
* These libraries are written in pure safe rust (no `unsafe`). This makes it easier
  to quickly and safely experiment when compared `C++` or `C`, in particular with
  multithreading or with alternative hash-table back-ends.
* A C API is provided for ease of integration (see `src/lib.rs`)
