//! Defines a monadic-style random value
use super::probability::Probability;
use rand::prelude::*;

pub trait Rand: IntoIterator<Item = (Self::Val, Probability)> {
    type Val;
    /// Initialize a random value from a vector of `(value, probability)` pairs
    fn from_vec(val: Vec<(Self::Val, Probability)>) -> Self;

    /// Retrieve the underlying vector of (value, probability) pairs
    fn vec(&self) -> &[(Self::Val, Probability)];

    /// Generate a Boolean random value, call `f` with it, and return the result
    /// wrapped in a `Random<T>`
    fn bool<F: FnMut(bool) -> Self::Val>(sampled: bool, prob: Probability, mut f: F) -> Self
    where
        Self: Sized,
    {
        if sampled {
            let mut rng = rand::thread_rng();
            let v = rng.gen_bool(prob.as_f64());
            if v {
                Rand::from_vec(vec![(f(true), prob)])
            } else {
                Rand::from_vec(vec![(f(false), Probability::new(1.0) - prob)])
            }
        } else {
            Rand::from_vec(vec![
                (f(true), prob),
                (f(false), Probability::new(1.0) - prob),
            ])
        }
    }
    /// Choose a uniform int between [low..high)
    fn uniform_int<F: FnMut(usize) -> Self::Val>(
        sampled: bool,
        low: usize,
        high: usize,
        mut f: F,
    ) -> Self
    where
        Self: Sized,
    {
        assert!(high > low);
        if sampled {
            let mut c = rand::thread_rng();
            let v = c.gen_range(low..high);
            Rand::from_vec(vec![(
                f(v),
                Probability::new(1.0) / Probability::new((high - low) as f64),
            )])
        } else {
            let prob = Probability::new(1.0 / (high - low) as f64);
            let v: Vec<(Self::Val, Probability)> = (low..high).map(|x| (f(x), prob)).collect();
            Rand::from_vec(v)
        }
    }
    fn flatten<U: Rand<Val = RR>, RR: Rand<Val = X>, X>(v: U) -> RR {
        let mut r: Vec<(RR::Val, Probability)> = Vec::new();
        for (v1, prob1) in v.into_iter() {
            for (v2, prob2) in v1.into_iter() {
                r.push((v2, prob1 * prob2));
            }
        }
        // assert!(tot > 0.0 && tot <= 1.0);
        Rand::from_vec(r)
    }

    // /// applies `f` to each component
    // fn bind<U:Rand<T=X>, X>(&self, f: &mut dyn FnMut(&Self::Val) -> U) -> U {
    //     let v: Vec<(U, Probability)> = self.vec().iter().map(|(x, p)| (f(x), *p)).collect();
    //     let n : dyn Rand<T=X> = Rand::from_vec(v);
    //     Rand::flatten(n)
    // }

    fn map<U: Rand<Val = X>, X>(&self, f: &dyn Fn(&Self::Val) -> X) -> U {
        Rand::from_vec(self.vec().iter().map(|(t, p)| (f(t), *p)).collect())
    }

    /// Generate a Dirac delta at value `v`
    fn delta(v: Self::Val) -> Self
    where
        Self: Sized,
    {
        Rand::from_vec(vec![(v, Probability::new(1.0))])
    }

    /// Extract a Diract delta
    fn unwrap(&self) -> &Self::Val {
        assert!(self.vec().len() == 1);
        &self.vec()[0].0
    }
}

/// Carries a possibly symbolic random computation (in monadic style)
#[derive(Debug, Clone)]
pub struct Random<T> {
    val: Vec<(T, Probability)>,
}

impl<T> IntoIterator for Random<T> {
    type Item = (T, Probability);
    type IntoIter = std::vec::IntoIter<(T, Probability)>;

    fn into_iter(self) -> Self::IntoIter {
        self.val.into_iter()
    }
}

impl<T> Rand for Random<T> {
    type Val = T;
    /// Initialize a random value from a vector of `(value, probability)` pairs
    fn from_vec(val: Vec<(T, Probability)>) -> Random<T> {
        Random { val }
    }

    /// Retrieve the underlying vector of (value, probability) pairs
    fn vec(&self) -> &[(T, Probability)] {
        &self.val
    }
}

#[test]
fn test_random() {
    let b1 = Random::bool(false, Probability::new(0.4), |b| {
        Random::uniform_int(false, 0, 4, |x| if b { 0 } else { x })
    });
    let f = Random::<usize>::flatten(b1);
    assert_eq!(
        f.val,
        vec![
            (0, Probability::new(0.1)),
            (0, Probability::new(0.1)),
            (0, Probability::new(0.1)),
            (0, Probability::new(0.1)),
            (0, Probability::new(0.15)),
            (1, Probability::new(0.15)),
            (2, Probability::new(0.15)),
            (3, Probability::new(0.15))
        ]
    );
}
