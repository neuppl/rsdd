/// A binary tree with leaves of type L and nodes of type N
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
        let top_unwrap = match self.stack.pop() {
            None => return None,
            Some(v) => v,
        };
        match top_unwrap {
            &Leaf(_) => Some(&top_unwrap),
            &Node(ref v, ref l, ref r) => {
                self.stack.push(r);
                self.stack.push(l);
                Some(&top_unwrap)
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
        InOrderIter { stack: vec![self] }
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
                    let (l_cnt, l_t) = helper(l, cnt + 1);
                    let (r_cnt, r_t) = helper(r, l_cnt);
                    (r_cnt, Box::new(Node(cnt, l_t, r_t)))
                }
            }
        }
        let (_, r) = helper(self, 0);
        r
    }
}
