[![CI](https://github.com/neuppl/rsdd/actions/workflows/ci.yml/badge.svg)](https://github.com/neuppl/rsdd/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/neuppl/rsdd/branch/main/graph/badge.svg?token=ZPA9C38VPT)](https://codecov.io/gh/neuppl/rsdd)

[Documentation and tutorial](https://neuppl.github.io/rsdd-docs/)

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
rsdd = { git = "https://github.com/neuppl/rsdd" }
```

This project is under active research development; if you are interested in
contributing please contact [Steven Holtzen](https://www.khoury.northeastern.edu/home/sholtzen/)
at `s.holtzen@northeastern.edu`.

## Building

After cloning the repository, the following commands are available:

* `cargo build`: build a dynamically linkable library in `target/debug/`
* `cargo build --release`: build a release build in `target/release`
* `cargo test`: run the test suite
