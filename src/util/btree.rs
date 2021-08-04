/// A binary tree with leaves of type L and nodes of type N, represented with
/// child pointers
#[derive(Clone, Debug)]
pub enum BTree<N, L>
where
    N: PartialEq + Eq + Clone,
    L: PartialEq + Eq + Clone,
{
    Leaf(L),
    Node(N, Box<BTree<N, L>>, Box<BTree<N, L>>),
}

pub struct InOrderIter<'a, N: 'a, L: 'a>
where
    N: PartialEq + Eq + Clone,
    L: PartialEq + Eq + Clone,
{
    stack: Vec<&'a BTree<N, L>>,
}

impl<'a, N, L> Iterator for InOrderIter<'a, N, L>
where
    N: PartialEq + Eq + Clone,
    L: PartialEq + Eq + Clone,
{
    type Item = &'a BTree<N, L>;

    fn next(&mut self) -> Option<Self::Item> {
        use self::BTree::*;
        match self.stack.pop() {
            None => return None,
            Some(v) => match v {
                &Leaf(_) => Some(v),
                &Node(_, _, ref r) => {
                    let mut cur : &'a BTree<N, L> = r;
                    loop {
                        match cur {
                            &BTree::Leaf(_) => {
                                self.stack.push(cur);
                                break;
                            },
                            &BTree::Node(_, ref l, ref r) => {
                                self.stack.push(cur);
                                cur = l;
                            }
                        }
                    }
                    Some(v)
                }
            }
        }
    }
}

impl<N, L> BTree<N, L>
where
    N: PartialEq + Eq + Clone,
    L: PartialEq + Eq + Clone,
{
    pub fn in_order_iter<'a>(&'a self) -> InOrderIter<'a, N, L> {
        let mut v = Vec::new();
        let mut cur = self;
        loop {
            match cur {
                &BTree::Leaf(_) => {
                    v.push(cur);
                    return InOrderIter { stack: v };
                },
                &BTree::Node(_, ref l, ref r) => {
                    v.push(cur);
                    cur = l;
                }
            }
        }
    }

    pub fn contains_leaf<F>(&self, f: &F) -> bool
    where
        F: Fn(&L) -> bool,
    {
        match self {
            &BTree::Leaf(ref v) => f(v),
            &BTree::Node(_, ref l, ref r) => l.contains_leaf(f) || r.contains_leaf(f),
        }
    }

    /// Find the depth-first index of a leaf which satisfies F; None
    /// if none is found
    pub fn find_leaf_idx<F>(&self, f: &F) -> Option<usize>
        where F: Fn(&L) -> bool
    {
        for (idx, i) in self.in_order_iter().enumerate() {
            match i {
                &BTree::Node(_, _, _) => (),
                &BTree::Leaf(ref l) =>
                    if f(l) { return Some(idx) } else {}
            }
        }
        None
    }

    /// generates a tree where each node is labeled with its
    /// index according to an in-order traversal of the tree
    pub fn into_order_tree(&self) -> Box<BTree<usize, usize>> {
        use self::BTree::*;
        fn helper<N, L>(t: &BTree<N, L>, cnt: usize) -> (usize, Box<BTree<usize, usize>>)
        where
            N: PartialEq + Eq + Clone,
            L: PartialEq + Eq + Clone,
        {
            match t {
                &Leaf(_) => (cnt + 1, Box::new(Leaf(cnt))),
                &Node(_, ref l, ref r) => {
                    let (l_cnt, l_t) = helper(l, cnt);
                    let (r_cnt, r_t) = helper(r, l_cnt + 1);
                    (r_cnt, Box::new(Node(l_cnt, l_t, r_t)))
                }
            }
        }
        let (_, r) = helper(self, 0);
        r
    }

    /// Attempt to extract the data from a leaf node; panics if not a leaf
    pub fn extract_leaf(&self) -> &L {
        match self {
            Self::Leaf(ref v) => v,
            _ => panic!("extracting non-leaf")
        }
    }

    /// Flatten a BTree into a depth-first iteration
    pub fn flatten(&self) -> Vec<&BTree<N, L>> {
        let mut v = Vec::new();
        for i in self.in_order_iter() {
            v.push(i)
        }
        v
    }
}

#[test]
fn test_traversal() {
    use self::BTree::*;
    let vtree : BTree<i32, i32> =
        Node(4,
             Box::new(Node(2, Box::new(Leaf(1)), Box::new(Leaf(3)))),
             Box::new(Node(6, Box::new(Leaf(5)), Box::new(Leaf(7))),
             ));

    for (idx, v) in vtree.in_order_iter().enumerate() {
        let value = match v {
            &Node(ref v, _, _) => v.clone(),
            &Leaf(ref v) => v.clone()
        };
        assert_eq!((idx + 1) as i32, value);
    }
}

#[test]
fn test_in_order_tree() {
    use self::BTree::*;
    let vtree : BTree<i32, i32> =
        Node(4,
             Box::new(Node(2, Box::new(Leaf(1)), Box::new(Leaf(3)))),
             Box::new(Node(6, Box::new(Leaf(5)), Box::new(Leaf(7))),
             ));

    let in_order = vtree.into_order_tree();
    for (idx, v) in in_order.in_order_iter().enumerate() {
        let value = match v {
            &Node(ref v, _, _) => v.clone(),
            &Leaf(ref v) => v.clone()
        };
        assert_eq!(idx, value);
    }
}
