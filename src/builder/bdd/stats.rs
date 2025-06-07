/// An auxiliary data structure for tracking statistics about BDD manager
/// performance (for fine-tuning)
#[derive(Debug)]
pub struct BddBuilderStats {
    /// For now, always track the number of recursive calls. In the future,
    /// this should probably be gated behind a debug build (since I suspect
    /// it may have non-trivial performance overhead and synchronization cost)
    pub num_recursive_calls: usize,
}

impl BddBuilderStats {
    pub fn new() -> BddBuilderStats {
        BddBuilderStats {
            num_recursive_calls: 0,
        }
    }
}

impl Default for BddBuilderStats {
    fn default() -> Self {
        Self::new()
    }
}
