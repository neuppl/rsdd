//! A generic importance sampling interface with methods for automated testing

use super::probability::Probability;
use super::random::Random;

/// An importance sampler takes two types in order to support (optional)
/// collapsed sampling:
/// (1) The `Sample` type, which is the type of the data that is generated
/// during sampling (for example, a partially collapsed BDD)
/// (2) the `State` type, which is the sample space of the probability
/// distribution (for example, a set of models or partial models)
pub trait ImportanceSampler<Sample: std::fmt::Debug + Clone, State: std::fmt::Debug + Clone> {
    /// Gives an iterator through all possible states that can be reached by the chain
    /// Used for automated testing, this can be inefficient
    fn state_vector(&mut self) -> Vec<State>;

    /// Gives a unique index that corresponds to this particular state
    /// Used for automated testing, this can be inefficient
    fn get_state_index(&self, state: &State) -> usize;

    /// Generates a vector of the probability of every state indexed by the
    /// state index
    /// Used for automated testing, this can be inefficient
    fn probability_vector(&mut self) -> Vec<Probability>;

    /// Get the total number of states (i.e., the size of the domain)
    /// Used for testing, this can be inefficient
    fn num_states(&self) -> usize;

    /// Compute the unnormalized probability of the state
    fn unnormalized_prob(&mut self, state: &Sample) -> Probability;

    /// True if the importance sampler is valid (i.e., the distribution defined by the
    /// sampler matches the true distribution given in `probability_vector`)
    ///
    /// This is used for testing -- its runtime is `O(# states)`
    /// TODO: resolve unused
    #[allow(unused)]
    fn is_valid(&mut self) -> bool {
        todo!();
        let proposals = self.propose(false);
        let mut is_prob = vec![0; self.num_states()];
        for (proposal, outer_prob) in proposals.vec().iter() {
            let collapsed = self.collapse(proposal);
            let unnormalized = self.unnormalized_prob(proposal);
            for (state, inner_prob) in collapsed.vec().iter() {}
        }
        false
    }

    /// Collapse a sample into a distribution on states
    fn collapse(&mut self, state: &Sample) -> Random<State>;

    /// Propose a transition
    /// `sampled` is true if this is current a sampled proposal, it is false
    /// if it is enumerative
    ///
    /// Returns a distribution over the set of possible samples (i.e., draws
    /// a sample from `q(x)`)
    fn propose(&mut self, sampled: bool) -> Random<Sample>;
}
