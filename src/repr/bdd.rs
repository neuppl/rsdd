use std::{fmt::Debug, hash::Hash, collections::HashMap, cell::RefCell};

use bumpalo::Bump;

use super::var_label::VarLabel;

pub trait BddPtr<'a> : Clone + Copy + Debug + Eq + PartialEq + Hash {
    fn deref_node(&'a self) -> Bdd<Self>;
}

pub enum Bdd<Ptr> {
    True,
    False,
    Node { low: Ptr, high: Ptr, topvar: VarLabel }
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct BackedROBDDNode<'a> {
    low: ROBDDPtr<'a>, 
    high: ROBDDPtr<'a>, 
    topvar: VarLabel,
    scratch: RefCell<Option<usize>>
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum ROBDDPtr<'a> {
    True,
    False,
    Node(&'a BackedROBDDNode<'a>),
    NegatedNode(&'a BackedROBDDNode<'a>)
}

impl<'a> BddPtr<'a> for ROBDDPtr<'a> {
    fn deref_node(&'a self) -> Bdd<Self> {
        match self {
            ROBDDPtr::True => Bdd::True,
            ROBDDPtr::False => Bdd::False,
            ROBDDPtr::Node(n) => Bdd::Node { low: n.low, high: n.high, topvar: n.topvar },
            ROBDDPtr::NegatedNode(n) => Bdd::Node { low: n.low.negate(), high: n.high.negate(), topvar: n.topvar },
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
    compute_table: RefCell<HashMap<BackedROBDDNode<'a>, ROBDDPtr<'a>>>
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
        let n = BackedROBDDNode { low: ROBDDPtr::False, high: ROBDDPtr::True, topvar: v, scratch: RefCell::new(None) };
        self.get_or_insert(n)
    }

    fn const_true(&'a self) -> ROBDDPtr<'a> {
        ROBDDPtr::True
    }

    fn const_false(&'a self) -> ROBDDPtr<'a> {
        ROBDDPtr::False
    }

    fn ite(&'a self, f: ROBDDPtr<'a>, g: ROBDDPtr<'a>, h: ROBDDPtr<'a>) -> ROBDDPtr<'a> {
        todo!()
    }
}