//! Data-structure for representing ITEs in a standard form
//! Follows Section 7.1.5, "Standard Triples", in
//! 'Algorithms and Datastructures in VLSI Design' pages 115-117
use crate::repr::DDNNFPtr;

/// Core ITE representation
#[derive(Debug, Clone, Hash, PartialEq, Eq, Copy)]
pub enum Ite<T> {
    /// a standard ite
    IteChoice {
        f: T,
        g: T,
        h: T,
    },
    /// an ite that is complemented
    IteComplChoice {
        f: T,
        g: T,
        h: T,
    },
    IteConst(T),
}
use Ite::*;

impl<'a, T: DDNNFPtr<'a>> Ite<T> {
    /// Returns a new Ite in standard form and a Bool indicating whether to complement the Ite
    /// Arguments are ITE(f, g, h), i.e. if f then g else h
    /// `order(a,b)` is true if a is before b in the decision order
    pub fn new(order: impl Fn(T, T) -> bool, f: T, g: T, h: T) -> Ite<T> {
        // a wise man once said: there are parts of the code that are easier to
        // prove correct than they are to debug or test. This is one of those
        // parts.  Is it proven correct? Unfortunately, no.

        // introduce constants
        let (f, g, h) = match (f, g, h) {
            (f, g, h) if f == h => (f, g, T::false_ptr()),
            (f, g, h) if f == h.neg() => (f, g, T::true_ptr()),
            (f, g, h) if f == g.neg() => (f, T::false_ptr(), h),
            _ => (f, g, h),
        };

        // check for terminal cases
        match (f, g, h) {
            (f, g, _) if f.is_true() => return IteConst(g),
            (f, _, h) if f.is_false() => return IteConst(h),
            (f, g, h) if g.is_true() && h.is_false() => return IteConst(f),
            (f, g, h) if g.is_false() && h.is_true() => return IteConst(f.neg()),
            (_, g, h) if h == g => return IteConst(g),
            _ => (),
        };

        // now, attempt to reorder the ITE to place the top-most node first in the order
        let (f, g, h) = match (f, g, h) {
            (f, g, h) if g.is_true() && order(h, f) => (h, g, f),
            (f, g, h) if h.is_false() && order(g, f) => (g, f, h),
            (f, g, h) if h.is_true() && order(g, f) => (g.neg(), f.neg(), h),
            (f, g, h) if g.is_false() && order(h, f) => (h.neg(), g, f.neg()),
            (f, g, h) if g == h.neg() && order(g, f) => (g, f, f.neg()),
            _ => (f, g, h),
        };

        // now, standardize for negation: ensure f and g are non-negated
        match (f, g, h) {
            (f, g, h) if f.is_neg() && !h.is_neg() => IteChoice {
                f: f.neg(),
                g: h,
                h: g,
            },
            (f, g, h) if !f.is_neg() && g.is_neg() => IteComplChoice {
                f,
                g: g.neg(),
                h: h.neg(),
            },
            (f, g, h) if f.is_neg() && h.is_neg() => IteComplChoice {
                f: f.neg(),
                g: h.neg(),
                h: g.neg(),
            },
            _ => IteChoice { f, g, h },
        }
    }

    pub fn is_compl_choice(&self) -> bool {
        matches!(self, IteComplChoice { f: _, g: _, h: _ })
    }
}
