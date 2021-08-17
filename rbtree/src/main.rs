#![allow(dead_code)]

extern crate stainless;
use stainless::*;

enum Color {
  Red,
  Black,
}

enum RBTree<T> {
  Empty,
  Node(Color, Box<RBTree<T>>, T, Box<RBTree<T>>),
}

use Color::*;
use RBTree::*;

impl RBTree<i32> {
  pub fn new() -> Self {
    Empty
  }

  #[pre(self.red_nodes_have_black_children() && self.black_balanced())]
  #[post(set_equals(&self.content(), &old(&self).content().insert(&t))
        && (old(&self).size() == self.size() || old(&self).size() + 1 == self.size())
        && self.red_nodes_have_black_children()
        && self.black_balanced()
     )]
  pub fn insert(&mut self, t: i32) {
    // Insert and color the root black.
    self.ins(t);
    if let Node(ref mut c, _, _, _) = self {
      *c = Black;
    }
  }

  pub fn is_black(&self) -> bool {
    matches!(self, Empty) || matches!(self, Node(Black, _, _, _))
  }

  pub fn is_red(&self) -> bool {
    !self.is_black()
  }

  pub fn size(&self) -> usize {
    match self {
      Empty => 0,
      Node(_, l, _, r) => 1 + l.size() + r.size(),
    }
  }

  fn content(&self) -> Set<&i32> {
    match self {
      Empty => Set::new(),
      Node(_, l, v, r) => Set::singleton(v).union(l.content()).union(r.content()),
    }
  }

  fn red_nodes_have_black_children(&self) -> bool {
    match self {
      Empty => true,
      Node(Black, l, _, r) => {
        l.red_nodes_have_black_children() && r.red_nodes_have_black_children()
      }
      Node(Red, l, _, r) => {
        l.is_black()
          && r.is_black()
          && l.red_nodes_have_black_children()
          && r.red_nodes_have_black_children()
      }
    }
  }

  fn red_desc_have_black_children(&self) -> bool {
    match self {
      Empty => true,
      Node(_, l, _, r) => l.red_nodes_have_black_children() && r.red_nodes_have_black_children(),
    }
  }

  #[allow(dead_code)]
  fn black_balanced(&self) -> bool {
    match self {
      Node(_, l, _, r) => {
        l.black_balanced() && r.black_balanced() && l.black_height() == r.black_height()
      }
      Empty => true,
    }
  }

  fn black_height(&self) -> usize {
    match self {
      Empty => 1,
      Node(Black, l, _, _) => l.black_height() + 1,
      Node(Red, l, _, _) => l.black_height(),
    }
  }

  #[pre(self.red_nodes_have_black_children() && self.black_balanced())]
  #[post(set_equals(&self.content(), &old(&self).content().insert(&t))
    && (old(&self).size() == self.size() || old(&self).size() + 1 == self.size())
    && self.black_balanced()
    && self.red_desc_have_black_children()
  )]
  fn ins(&mut self, t: i32) {
    match self {
      Empty => {
        *self = Node(Red, Box::new(Empty), t, Box::new(Empty));
      }
      Node(_, left, value, right) => {
        if t < *value {
          left.ins(t);
          self.balance();
        } else if t > *value {
          right.ins(t);
          self.balance();
        }
      }
    }
  }

  #[post(
     set_equals(&old(&self).content(), &self.content())
     && self.size() == old(&self).size()
   )]
  fn rotate_right(&mut self) {
    let self_tree = std::mem::replace(self, Empty);
    if let Node(c1, mut left, v1, c) = self_tree {
      let left_tree = std::mem::replace(&mut *left, Empty);
      if let Node(c2, a, v2, b) = left_tree {
        *left = Node(c1, b, v1, c);
        *self = Node(c2, a, v2, left);
      } else {
        *left = left_tree;
        *self = Node(c1, left, v1, c);
      }
    }
  }

  #[post(
     set_equals(&old(&self).content(), &self.content())
     && self.size() == old(&self).size()
   )]
  fn rotate_left(&mut self) {
    let self_tree = std::mem::replace(self, Empty);
    if let Node(c1, a, v1, mut right) = self_tree {
      let right_tree = std::mem::replace(&mut *right, Empty);
      if let Node(c2, b, v2, c) = right_tree {
        *right = Node(c1, a, v1, b);
        *self = Node(c2, right, v2, c);
      } else {
        *right = right_tree;
        *self = Node(c1, a, v1, right);
      }
    }
  }

  #[post(
    set_equals(&old(&self).content(), &self.content())
    && self.size() == old(&self).size()
  )]
  fn recolor(&mut self) {
    if let Node(c, left, _, right) = self {
      if let Node(cl, _, _, _) = &mut **left {
        if let Node(cr, _, _, _) = &mut **right {
          *c = Red;
          *cl = Black;
          *cr = Black;
        }
      }
    }
  }

  #[post(
      set_equals(&old(&self).content(), &self.content())
      && self.size() == old(&self).size()
    )]
  fn balance(&mut self) {
    if let Node(Black, left, _, _) = self {
      if left.is_red() {
        match &mut **left {
          Node(Red, ll, _, _) if ll.is_red() => {
            self.rotate_right();
            self.recolor();
          }
          Node(Red, _, _, lr) if lr.is_red() => {
            left.rotate_left();
            self.rotate_right();
            self.recolor();
          }
          _ => {}
        }
      }
    }
    if let Node(Black, _, _, right) = self {
      if right.is_red() {
        match &mut **right {
          Node(Red, rl, _, _) if rl.is_red() => {
            right.rotate_right();
            self.rotate_left();
            self.recolor();
          }
          Node(Red, _, _, rr) if rr.is_red() => {
            self.rotate_left();
            self.recolor();
          }
          _ => {}
        }
      }
    }
  }
}

fn set_equals<'a>(a: &Set<&'a i32>, b: &Set<&'a i32>) -> bool {
  a.is_subset(b) && b.is_subset(a)
}

pub fn main() {
  let mut tree = RBTree::new();

  #[pre(n >= 0)]
  fn rec(tree: &mut RBTree<i32>, n: i32) {
    if n > 0 {
      println!("{}", n);
      tree.insert(n);
      rec(tree, n - 1);
    }
  }

  rec(&mut tree, 5000);
}
