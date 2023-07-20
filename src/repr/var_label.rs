//! A generic data structure for tracking variable labels throughout the library
use bit_set::BitSet;
use quickcheck::{Arbitrary, Gen};
use serde::Serialize;
use std::fmt::{self, Display};

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
#[derive(Clone, PartialEq, Eq, Hash, Copy, Serialize, Deserialize, PartialOrd, Ord)]
pub struct Literal {
    data: u64,
}

BITFIELD!(Literal data : u64 [
    raw_label set_label[0..63],
    raw_polarity set_polarity[63..64],
]);

impl Literal {
    /// create a new Literal, which is a logical variable initialized with a polarity.
    /// ```
    /// use rsdd::repr::var_label::{Literal, VarLabel};
    ///
    /// let lit = Literal::new(VarLabel::new(0), true);
    /// ```
    pub fn new(label: VarLabel, polarity: bool) -> Literal {
        let mut ret = Literal { data: 0 };
        ret.set_label(label.0);
        ret.set_polarity(if polarity { 1 } else { 0 });
        ret
    }

    /// ```
    /// use rsdd::repr::var_label::{Literal, VarLabel};
    ///
    /// let lit = Literal::new(VarLabel::new(0), true);
    ///
    /// assert_eq!(lit.label(), VarLabel::new(0))
    /// ```
    pub fn label(&self) -> VarLabel {
        VarLabel(self.raw_label())
    }

    /// ```
    /// use rsdd::repr::var_label::{Literal, VarLabel};
    ///
    /// let lit1 = Literal::new(VarLabel::new(0), true);
    /// let lit2 = Literal::new(VarLabel::new(0), false);
    ///
    /// assert!(lit1.polarity());
    /// assert!(!lit2.polarity());
    /// ```
    pub fn polarity(&self) -> bool {
        self.raw_polarity() == 1
    }

    /// ```
    /// use rsdd::repr::var_label::{Literal, VarLabel};
    ///
    /// let lit1 = Literal::new(VarLabel::new(0), true);
    /// let lit2 = Literal::new(VarLabel::new(0), true);
    ///
    /// assert!(lit1.implies_true(&lit2));
    /// ```
    pub fn implies_true(&self, other: &Literal) -> bool {
        self.label() == other.label() && self.polarity() == other.polarity()
    }

    /// ```
    /// use rsdd::repr::var_label::{Literal, VarLabel};
    ///
    /// let lit1 = Literal::new(VarLabel::new(0), true);
    /// let lit2 = Literal::new(VarLabel::new(0), false);
    ///
    /// assert!(lit1.implies_false(&lit2));
    /// ```
    pub fn implies_false(&self, other: &Literal) -> bool {
        self.label() == other.label() && self.polarity() != other.polarity()
    }

    /// ```
    /// use rsdd::repr::var_label::{Literal, VarLabel};
    ///
    /// let lit1 = Literal::new(VarLabel::new(0), true);
    /// let lit2 = Literal::new(VarLabel::new(0), false);
    ///
    /// assert_eq!(lit1.negated(), lit2);
    /// ```
    pub fn negated(&self) -> Literal {
        Literal::new(self.label(), !self.polarity())
    }
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Literal")
            .field("label", &self.label())
            .field("polarity", &self.polarity())
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

impl Display for VarSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", self.b.iter().collect::<Vec<usize>>()))
    }
}

impl VarSet {
    /// create a new VarSet with no items in it
    /// ```
    /// use rsdd::repr::var_label::VarSet;
    ///
    /// let s = VarSet::new();
    ///
    /// assert!(s.is_empty());
    /// ```
    pub fn new() -> VarSet {
        VarSet { b: BitSet::new() }
    }

    pub fn new_with_num_vars(num_vars: usize) -> VarSet {
        VarSet {
            b: BitSet::with_capacity(num_vars),
        }
    }

    /// unions self with other in-place
    /// ```
    /// use rsdd::repr::var_label::{VarLabel, VarSet};
    ///
    /// let mut s1 = VarSet::new();
    /// let mut s2 = VarSet::new();
    ///
    /// s1.insert(VarLabel::new(0));
    /// s2.insert(VarLabel::new(0));
    /// s2.insert(VarLabel::new(1));
    ///
    /// s1.union_with(&s2);
    ///
    /// assert!(s1.len() == 2);
    /// assert!(s1.contains(VarLabel::new(0)));
    /// assert!(s1.contains(VarLabel::new(1)));
    /// ```
    pub fn union_with(&mut self, other: &VarSet) {
        self.b.union_with(&other.b);
    }

    /// iterate over items in varset
    pub fn iter(&self) -> impl Iterator<Item = VarLabel> + '_ {
        self.b.iter().map(VarLabel::new_usize)
    }

    /// unions self with other, returning a new VarSet
    /// ```
    /// use rsdd::repr::var_label::{VarLabel, VarSet};
    ///
    /// let mut s1 = VarSet::new();
    /// let mut s2 = VarSet::new();
    ///
    /// s1.insert(VarLabel::new(0));
    /// s2.insert(VarLabel::new(0));
    /// s2.insert(VarLabel::new(1));
    ///
    /// let s3 = s1.union(&s2);
    ///
    /// assert!(s3.len() == 2);
    /// assert!(s3.contains(VarLabel::new(0)));
    /// assert!(s3.contains(VarLabel::new(1)));
    /// ```
    pub fn union(&self, other: &VarSet) -> VarSet {
        VarSet {
            b: self.b.union(&other.b).collect(),
        }
    }

    /// returns a new VarSet = self \ other, where "\"" is the "set minus" operator
    /// ```
    /// use rsdd::repr::var_label::{VarLabel, VarSet};
    ///
    /// let mut s1 = VarSet::new();
    /// let mut s2 = VarSet::new();
    ///
    /// s1.insert(VarLabel::new(0));
    /// s2.insert(VarLabel::new(0));
    /// s2.insert(VarLabel::new(1));
    ///
    /// let s3 = s2.minus(&s1);
    ///
    /// assert!(s3.len() == 1);
    /// assert!(!s3.contains(VarLabel::new(0)));
    /// assert!(s3.contains(VarLabel::new(1)));
    /// ```
    pub fn minus(&self, other: &VarSet) -> VarSet {
        VarSet {
            b: self.b.difference(&other.b).collect(),
        }
    }

    /// ```
    /// use rsdd::repr::var_label::{VarLabel, VarSet};
    ///
    /// let mut s1 = VarSet::new();
    ///
    /// assert!(!s1.contains(VarLabel::new(0)));
    ///
    /// s1.insert(VarLabel::new(0));
    ///
    /// assert!(s1.contains(VarLabel::new(0)));
    /// ```
    pub fn insert(&mut self, v: VarLabel) {
        self.b.insert(v.value_usize());
    }

    /// ```
    /// use rsdd::repr::var_label::{VarLabel, VarSet};
    ///
    /// let mut s1 = VarSet::new();
    ///
    /// assert!(!s1.contains(VarLabel::new(0)));
    ///
    /// s1.insert(VarLabel::new(0));
    ///
    /// assert!(s1.contains(VarLabel::new(0)));
    /// ```
    pub fn contains(&self, v: VarLabel) -> bool {
        self.b.contains(v.value_usize())
    }

    pub fn intersect<'a>(&'a self, other: &'a VarSet) -> bit_set::Intersection<'a, u32> {
        self.b.intersection(&other.b)
    }

    pub fn remove(&mut self, v: VarLabel) {
        self.b.remove(v.value_usize());
    }

    pub fn difference<'a>(&'a self, other: &'a VarSet) -> impl Iterator<Item = VarLabel> + 'a {
        self.b.difference(&other.b).map(VarLabel::new_usize)
    }

    pub fn intersect_varset<'a>(&'a self, other: &'a VarSet) -> VarSet {
        return VarSet {
            b: self.b.intersection(&other.b).collect(),
        };
    }

    /// ```
    /// use rsdd::repr::var_label::VarSet;
    ///
    /// let s = VarSet::new();
    ///
    /// assert!(s.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.b.is_empty()
    }

    pub fn len(&self) -> usize {
        self.b.len()
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
