//! Data-structure for representing ITEs in a standard form
//! Follows Section 7.1.5, "Standard Triples", in
//! 'Algorithms and Datastructures in VLSI Design' pages 115-117
use crate::repr::{bdd::BddPtr, var_order::VarOrder};

/// Core ITE representation
#[derive(Debug, Clone, Hash, PartialEq, Eq, Copy)]
pub enum Ite {
    /// a standard ite
    IteChoice {
        f: BddPtr,
        g: BddPtr,
        h: BddPtr,
    },
    /// an ite that is complemented
    IteComplChoice {
        f: BddPtr,
        g: BddPtr,
        h: BddPtr,
    },
    IteConst(BddPtr),
}
use Ite::*;

/// check if a < b in the variable order
/// in the case that one is a constant, place the constant first in the order
fn lt_safe(order: &VarOrder, a: BddPtr, b: BddPtr) -> bool {
    if a.is_const() {
        return true;
    }
    if b.is_const() {
        return false;
    }
    return order.lt(a.var(), b.var());
}

impl Ite {
    /// Returns a new Ite in standard form and a Bool indicating whether to complement the Ite
    /// Arguments are ITE(f, g, h), i.e. if f then g else h
    pub fn new(order: &VarOrder, f: BddPtr, g: BddPtr, h: BddPtr) -> Ite {
        // a wise man once said: there are parts of the code that are easier to
        // prove correct than they are to debug or test. This is one of those
        // parts.  Is it proven correct? Unfortunately, no.

        // introduce constants
        let (f, g, h) = match (f, g, h) {
            (f, g, h) if f == h => (f, g, BddPtr::false_ptr()),
            (f, g, h) if f == h.compl() => (f, g, BddPtr::true_ptr()),
            (f, g, h) if f == g.compl() => (f, BddPtr::false_ptr(), h),
            _ => (f, g, h),
        };

        // check for terminal cases
        match (f, g, h) {
            (f, g, _) if f.is_true() => return IteConst(g),
            (f, _, h) if f.is_false() => return IteConst(h),
            (f, g, h) if g.is_true() && h.is_false() => return IteConst(f),
            (f, g, h) if g.is_false() && h.is_true() => return IteConst(f.compl()),
            (_, g, h) if h == g => return IteConst(g),
            _ => (),
        };

        // now, attempt to reorder the ITE to place the top-most node first in the order
        let (f, g, h) = match (f, g, h) {
            (f, g, h) if g.is_true() && lt_safe(order, h, f) => (h, g, f),
            (f, g, h) if h.is_false() && lt_safe(order, g, f) => (g, f, h),
            (f, g, h) if h.is_true() && lt_safe(order, g, f) => (g.compl(), f.compl(), h),
            (f, g, h) if g.is_false() && lt_safe(order, h, f) => (h.compl(), g, f.compl()),
            (f, g, h) if g == h.compl() && lt_safe(order, g, f) => (g, f, f.compl()),
            _ => (f, g, h),
        };

        // now, standardize for negation: ensure f and g are non-negated
        match (f, g, h) {
            (f, g, h) if f.is_compl() && !h.is_compl() => {
                return IteComplChoice {
                    f: f.compl(),
                    g: h,
                    h: g,
                }
            }
            (f, g, h) if !f.is_compl() && g.is_compl() => {
                return IteComplChoice {
                    f,
                    g: g.compl(),
                    h: h.compl(),
                }
            }
            (f, g, h) if f.is_compl() && h.is_compl() => {
                return IteComplChoice {
                    f: f.compl(),
                    g: h.compl(),
                    h: g.compl(),
                }
            }
            _ => return IteChoice { f, g, h },
        }
    }

    pub fn is_compl_choice(&self) -> bool {
        match &self {
            IteComplChoice { f: _, g: _, h: _ } => true,
            _ => false,
        }
    }
}
