//! Defines a monadic-style random value
use rand::prelude::*;
use super::probability::Probability;

/// Carries a possibly symbolic random computation (in monadic style)
#[derive(Debug, Clone)]
pub struct Random<T> {
    val: Vec<(T, Probability)>,
}

impl<T> Random<T> {
    /// Initialize a random value from a vector of `(value, probability)` pairs
    pub fn from_vec(val: Vec<(T, Probability)>) -> Random<T> {
        Random { val }
    }

    /// Generate a Boolean random value, call `f` with it, and return the result
    /// wrapped in a Random<T>
    pub fn bool<F: FnMut(bool) -> T>(sampled: bool, prob: Probability, mut f: F) -> Random<T> {
        if sampled {
            let mut rng = rand::thread_rng();
            let v = rng.gen_bool(prob.as_f64());
            if v {
                Random {
                    val: vec![(f(true), prob)],
                }
            } else {
                Random {
                    val: vec![(f(false), Probability::new(1.0) -prob)],
                }
            }
        } else {
            Random {
                val: vec![(f(true), prob), (f(false), Probability::new(1.0) - prob)],
            }
        }
    }

    /// Choose a uniform int between [low..high)
    pub fn uniform_int<F: FnMut(usize) -> T>(
        sampled: bool,
        low: usize,
        high: usize,
        mut f: F,
    ) -> Random<T> {
        assert!(high > low);
        if sampled {
            let mut c = rand::thread_rng();
            let v = c.gen_range(low..high);
            Random {
                val: vec![(f(v), Probability::new(1.0) / Probability::new((high - low) as f64))],
            }
        } else {
            let prob = Probability::new(1.0 / (high - low) as f64);
            let v: Vec<(T, Probability)> = (low..high).map(|x| (f(x), prob)).collect();
            Random { val: v }
        }
    }

    pub fn flatten(v: Random<Random<T>>) -> Random<T> {
        let mut r: Vec<(T, Probability)> = Vec::new();
        for (v1, prob1) in v.val.into_iter() {
            for (v2, prob2) in v1.val.into_iter() {
                r.push((v2, prob1 * prob2));
            }
        }
        // assert!(tot > 0.0 && tot <= 1.0);
        return Random { val: r };
    }

    /// Apply `f` to each component.
    pub fn map<R, F: FnMut(&T) -> Random<R>>(&self, f: &mut F) -> Random<R> {
        let v : Vec<(Random<R>, Probability)> = self.vec().iter().map(|(x, p)| (f(x), *p)).collect();
        let n = Random::from_vec(v);
        return Random::flatten(n)
    }

    /// Generate a Dirac delta at value `v`
    pub fn delta(v: T) -> Random<T> {
        Random {
            val: vec![(v, Probability::new(1.0))],
        }
    }

    /// Extract a Diract delta
    pub fn unwrap(&self) -> &T {
        assert!(self.val.len() == 1);
        return &self.val[0].0;
    }

    /// Retrieve the underlying vector of (value, probability) pairs
    pub fn vec(&self) -> &[(T, Probability)] {
        &self.val
    }
}


#[test]
fn test_random() {
    let b1 = Random::bool(false, 0.4, |b| {
        Random::uniform_int(false, 0, 4, |x| if b { 0 } else { x })
    });
    let f = Random::flatten(b1);
    assert_eq!(
        f.val,
        vec![
            (0, 0.1),
            (0, 0.1),
            (0, 0.1),
            (0, 0.1),
            (0, 0.15),
            (1, 0.15),
            (2, 0.15),
            (3, 0.15)
        ]
    );
}
