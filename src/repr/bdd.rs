use std::{cell::RefCell, collections::HashMap, fmt::Debug, hash::Hash};

use bumpalo::Bump;

use super::var_label::VarLabel;

pub trait BddPtr<'a>: Clone + Copy + Debug + Eq + PartialEq + Hash {
    fn is_true(&self) -> bool;
    fn is_false(&self) -> bool;
    fn is_node(&self) -> bool;
    fn low(&self) -> Option<Self>;
    fn high(&self) -> Option<Self>;
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct BackedROBDDNode<'a> {
    low: ROBDDPtr<'a>,
    high: ROBDDPtr<'a>,
    topvar: VarLabel,
    scratch: RefCell<Option<usize>>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum ROBDDPtr<'a> {
    True,
    False,
    Node(&'a BackedROBDDNode<'a>),
    NegatedNode(&'a BackedROBDDNode<'a>),
}

impl<'a> BddPtr<'a> for ROBDDPtr<'a> {
    fn is_true(&self) -> bool {
        matches!(self, ROBDDPtr::True)
    }

    fn is_false(&self) -> bool {
        matches!(self, ROBDDPtr::False)
    }

    fn is_node(&self) -> bool {
        matches!(self, ROBDDPtr::Node(_) | ROBDDPtr::NegatedNode(_))
    }

    fn low(&self) -> Option<Self> {
        match self {
            ROBDDPtr::True => None,
            ROBDDPtr::False => None,
            ROBDDPtr::Node(n) => Some(n.low),
            ROBDDPtr::NegatedNode(n) => Some(n.low),
        }
    }

    fn high(&self) -> Option<Self> {
        match self {
            ROBDDPtr::True => None,
            ROBDDPtr::False => None,
            ROBDDPtr::Node(n) => Some(n.high),
            ROBDDPtr::NegatedNode(n) => Some(n.high),
        }
    }
}

impl<'a> ROBDDPtr<'a> {
    fn negate(&'a self) -> ROBDDPtr<'a> {
        match self {
            ROBDDPtr::True => ROBDDPtr::False,
            ROBDDPtr::False => ROBDDPtr::True,
            ROBDDPtr::Node(n) => ROBDDPtr::NegatedNode(n),
            ROBDDPtr::NegatedNode(n) => ROBDDPtr::Node(n),
        }
    }
}

impl<'a> Hash for BackedROBDDNode<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.low.hash(state);
        self.high.hash(state);
        self.topvar.hash(state);
    }
}

pub trait BddBuilder<'a, TPtr: BddPtr<'a>> {
    fn var(&'a self, v: VarLabel) -> TPtr;
    fn const_true(&'a self) -> TPtr;
    fn const_false(&'a self) -> TPtr;
    fn ite(&'a self, f: TPtr, g: TPtr, h: TPtr) -> TPtr;
    fn and(&'a self, a: TPtr, b: TPtr) -> TPtr {
        self.ite(a, b, self.const_false())
    }
}

struct ROBDDBuilder<'a> {
    alloc_table: Bump,
    compute_table: RefCell<HashMap<BackedROBDDNode<'a>, ROBDDPtr<'a>>>,
}

impl<'a> ROBDDBuilder<'a> {
    fn get_or_insert(&'a self, node: BackedROBDDNode<'a>) -> ROBDDPtr<'a> {
        let mut tbl = self.compute_table.borrow_mut();
        match tbl.get(&node) {
            Some(v) => *v,
            None => {
                // TODO this does not do complementing properly; this is just to get things to typecheck
                let new_n = ROBDDPtr::Node(self.alloc_table.alloc(node.clone()));
                tbl.insert(node, new_n);
                new_n
            }
        }
    }
}

impl<'a> BddBuilder<'a, ROBDDPtr<'a>> for ROBDDBuilder<'a> {
    fn var(&'a self, v: VarLabel) -> ROBDDPtr<'a> {
        let n = BackedROBDDNode {
            low: ROBDDPtr::False,
            high: ROBDDPtr::True,
            topvar: v,
            scratch: RefCell::new(None),
        };
        self.get_or_insert(n)
    }

    fn const_true(&'a self) -> ROBDDPtr<'a> {
        ROBDDPtr::True
    }

    fn const_false(&'a self) -> ROBDDPtr<'a> {
        ROBDDPtr::False
    }

    fn ite(&'a self, f: ROBDDPtr<'a>, g: ROBDDPtr<'a>, h: ROBDDPtr<'a>) -> ROBDDPtr<'a> {
        let v = g.high();
        return v.unwrap();
        // let n : Bdd<ROBDDPtr<'a>> = g.();
        // match n {
        //     Bdd::True => todo!(),
        //     Bdd::False => todo!(),
        //     Bdd::Node { low, high, topvar } => high,
        // }
    }

    fn and(&'a self, a: ROBDDPtr<'a>, b: ROBDDPtr<'a>) -> ROBDDPtr<'a> {
        self.ite(a, b, self.const_false())
    }
}
