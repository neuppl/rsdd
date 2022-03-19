//! A generic data structure for tracking variable labels throughout the library
use std::mem;
use std::fmt;
extern crate quickcheck;
use self::quickcheck::{Arbitrary, Gen};

/// number of bits allocated for variable label (limit on total number of
/// variables)
pub const VAR_BITS: usize = 18;
pub const MAX_VAR_SIZE: usize = 1 << VAR_BITS;

/// a label for each distinct variable in the BDD
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct VarLabel(u64);

impl VarLabel {
    #[inline]
    pub fn new(v: u64) -> VarLabel {
        assert!(v < 1 << VAR_BITS, "Variable identifier overflow");
        VarLabel(v)
    }
    #[inline]
    pub fn value(&self) -> u64 {
        self.0
    }
}

/// Literal, a variable label and its corresponding truth assignment
#[derive(Clone, PartialEq, Eq, Hash, Copy)]
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