extern crate stainless;

enum Tree<T> {
  Empty,
  Leaf(T),
  Node(Box<Tree<T>>, Box<Tree<T>>),
}

impl<T> Tree<T> {
  pub fn rotate_right(&mut self) {
    let self_tree = std::mem::replace(self, Tree::Empty);
    if let Tree::Node(mut left, c) = self_tree {
      let left_tree = std::mem::replace(&mut *left, Tree::Empty);
      if let Tree::Node(a, b) = left_tree {
        *left = Tree::Node(b, c);
        *self = Tree::Node(a, left);
      }
    }
  }
}

pub fn main() {}
