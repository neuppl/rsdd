//! A generic data structure for tracking variable labels throughout the library

/// number of bits allocated for variable label (limit on total number of
/// variables)
pub const VAR_BITS: usize = 11;

/// a label for each distinct variable in the BDD
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct VarLabel(u64);

impl VarLabel {
    #[inline]
    pub fn new(v: u64) -> VarLabel {
        assert!(v < 1 << VAR_BITS - 1, "Variable identifier overflow");
        VarLabel(v)
    }
    #[inline]
    pub fn value(&self) -> u64 {
        self.0
    }
}
