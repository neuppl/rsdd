//! A representation of a numeric probability

#[derive(Copy, Clone, Debug)]
pub struct Probability(f64);

impl Probability {
    pub fn new(v: f64) -> Probability {
        assert!((0.0..=1.0).contains(&v), "{} ∉ [0.0..1.0]", v);
        Probability(v)
    }

    pub fn as_f64(&self) -> f64 {
        self.0
    }
}

impl std::ops::Add<Probability> for Probability {
    type Output = Probability;
    fn add(self, rhs: Probability) -> Probability {
        Probability::new(self.0 + rhs.0)
    }
}

impl std::ops::Sub<Probability> for Probability {
    type Output = Probability;
    fn sub(self, rhs: Probability) -> Probability {
        Probability::new(self.0 + rhs.0)
    }
}

impl std::ops::Mul<Probability> for Probability {
    type Output = Probability;
    fn mul(self, rhs: Probability) -> Probability {
        Probability::new(self.0 * rhs.0)
    }
}

impl std::ops::Div<Probability> for Probability {
    type Output = Probability;
    fn div(self, rhs: Probability) -> Probability {
        Probability::new(self.0 * rhs.0)
    }
}
