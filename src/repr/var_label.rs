//! A generic data structure for tracking variable labels throughout the library
use serde::Serialize;
use std::fmt;

extern crate quickcheck;
use bit_set::BitSet;

use self::quickcheck::{Arbitrary, Gen};

/// a label for each distinct variable in the BDD
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash, Copy, PartialOrd, Ord)]
pub struct VarLabel(u64);

impl VarLabel {
    #[inline]
    pub fn new(v: u64) -> VarLabel {
        // assert!(v < 1 << VAR_BITS, "Variable identifier overflow");
        VarLabel(v)
    }

    #[inline]
    pub fn value(&self) -> u64 {
        self.0
    }

    pub fn new_usize(v: usize) -> VarLabel {
        VarLabel::new(v as u64)
    }

    pub fn value_usize(&self) -> usize {
        self.0 as usize
    }
}

/// Literal, a variable label and its corresponding truth assignment
#[derive(Clone, PartialEq, Eq, Hash, Copy, Serialize, Deserialize)]
pub struct Literal {
    data: u64,
}

BITFIELD!(Literal data : u64 [
    label set_label[0..63],
    polarity set_polarity[63..64],
]);

impl Literal {
    pub fn get_label(&self) -> VarLabel {
        VarLabel(self.label())
    }

    pub fn get_polarity(&self) -> bool {
        self.polarity() == 1
    }

    pub fn new(label: VarLabel, polarity: bool) -> Literal {
        let mut ret = Literal { data: 0 };
        ret.set_label(label.0);
        ret.set_polarity(if polarity { 1 } else { 0 });
        ret
    }

    pub fn implies_true(&self, other: &Literal) -> bool {
        self.get_label() == other.get_label() && self.get_polarity() == other.get_polarity()
    }

    pub fn implies_false(&self, other: &Literal) -> bool {
        self.get_label() == other.get_label() && self.get_polarity() != other.get_polarity()
    }

    pub fn negated(&self) -> Literal {
        Literal::new(self.get_label(), !self.get_polarity())
    }
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Literal")
            .field("label", &self.get_label())
            .field("polarity", &self.get_polarity())
            .finish()
    }
}

impl Arbitrary for Literal {
    fn arbitrary(g: &mut Gen) -> Literal {
        let varlbl = u64::arbitrary(g) % 16;
        Literal::new(VarLabel::new(varlbl), bool::arbitrary(g))
    }
}

/// A structure that contains sets of variables
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VarSet {
    b: BitSet,
}

impl Serialize for VarSet {
    fn serialize<S>(&self, _serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        todo!()
    }
}

impl VarSet {
    pub fn new() -> VarSet {
        VarSet { b: BitSet::new() }
    }

    /// unions self with other in-place
    pub fn union_with(&mut self, other: &VarSet) {
        self.b.union_with(&other.b);
    }

    /// unions self with other in-place
    pub fn iter(&self) -> impl Iterator<Item = VarLabel> + '_ {
        self.b.iter().map(VarLabel::new_usize)
    }

    /// unions self with other
    pub fn union(&self, other: &VarSet) -> VarSet {
        VarSet {
            b: self.b.union(&other.b).collect(),
        }
    }

    /// returns self \ other, where "\"" is the "set minus" operator
    pub fn minus(&self, other: &VarSet) -> VarSet {
        VarSet {
            b: self.b.difference(&other.b).collect(),
        }
    }

    pub fn insert(&mut self, v: VarLabel) {
        self.b.insert(v.value_usize());
    }

    pub fn contains(&self, v: VarLabel) -> bool {
        self.b.contains(v.value_usize())
    }

    pub fn intersect<'a>(&'a self, other: &'a VarSet) -> bit_set::Intersection<'a, u32> {
        self.b.intersection(&other.b)
    }

    pub fn intersect_varset<'a>(&'a self, other: &'a VarSet) -> VarSet {
        return VarSet {
            b: self.b.intersection(&other.b).collect(),
        };
    }

    pub fn is_empty(&self) -> bool {
        self.b.is_empty()
    }
}

impl Default for VarSet {
    fn default() -> Self {
        Self::new()
    }
}

#[test]
fn test_varset() {
    let mut v1 = VarSet::new();
    let mut v2 = VarSet::new();
    let mut v3 = VarSet::new();
    v1.insert(VarLabel::new(0));
    v1.insert(VarLabel::new(1));
    v2.insert(VarLabel::new(0));
    v3.insert(VarLabel::new(1));
    // assert {0,1} \ {0} = {1}
    let v1minusv2 = v1.minus(&v2);
    assert_eq!(v3, v1minusv2);
}
