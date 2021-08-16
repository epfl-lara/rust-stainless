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

  /*
   #[pre(self.red_nodes_have_black_children() && self.black_balanced())]
   #[post(set_equals(&self.content(), &old(&self).content().insert(&t))
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
  */

  pub fn is_black(&self) -> bool {
    matches!(self, Empty) || matches!(self, Node(Black, _, _, _))
  }

  pub fn is_red(&self) -> bool {
    !self.is_black()
  }

  #[post(match self {
    Node(_, l, _, r) => ret == 1 + l.size() + r.size(),
    _ => ret == 0
  })]
  pub fn size(&self) -> usize {
    match self {
      Empty => 0,
      Node(_, l, _, r) => 1 + l.size() + r.size(),
    }
  }

  #[allow(dead_code)]
  fn content(&self) -> Set<&i32> {
    match self {
      Empty => Set::new(),
      Node(_, l, v, r) => Set::singleton(v).union(l.content()).union(r.content()),
    }
  }

  /*
  #[allow(dead_code)]
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

  #[allow(dead_code)]
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

  #[allow(dead_code)]
  fn black_height(&self) -> usize {
    match self {
      Empty => 1,
      Node(Black, l, _, _) => l.black_height() + 1,
      Node(Red, l, _, _) => l.black_height(),
    }
  }
   */

  // #[pre(self.red_nodes_have_black_children() && self.black_balanced())]
  #[post(set_equals(&self.content(), &old(&self).content().insert(&t))
    && old(&self).size() <= self.size() && self.size() <= old(&self).size() + 1
  )]
  fn ins(&mut self, t: i32) {
    match self {
      Empty => {
        *self = Node(Red, Box::new(Empty), t, Box::new(Empty));
      }
      Node(_, left, value, right) => {
        let l = left.size();
        let r = right.size();
        if t < *value {
          left.ins(t);
          let ln = left.size();
          let rn = right.size();
          let sn = self.size();
          assert!(l <= ln);
          assert!(r == rn);
          assert!(ln <= l + 1);
          assert!(l + 1 + r <= ln + 1 + rn);
          assert!(ln + 1 + rn <= r + l + 2);
          assert!(ln + 1 + rn == sn);
          assert!(r + 1 + l <= sn);
          assert!(sn <= r + 2 + l);
          //self.balance();
        } else if t > *value {
          right.ins(t);
          let ln = left.size();
          let rn = right.size();
          let sn = self.size();
          assert!(r <= rn);
          assert!(l == ln);
          assert!(rn <= r + 1);
          assert!(l + 1 + r <= ln + 1 + rn);
          assert!(ln + 1 + rn <= r + l + 2);
          assert!(ln + 1 + rn == sn);
          assert!(r + 1 + l <= sn);
          assert!(sn <= r + 2 + l);
          //self.balance();
        }
      }
    }
  }

  /*
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
   fn balance(&mut self) {
     if self.is_black() {
       match self {
         Node(Black, left, _, _) if left.is_red() => match &mut **left {
           Node(Red, ll, _, _) if ll.is_red() => self.rotate_right(),
           Node(Red, _, _, lr) if lr.is_red() => {
             left.rotate_left();
             self.rotate_right();
           }
           _ => {}
         },

         Node(Black, _, _, right) if right.is_red() => match &mut **right {
           Node(Red, rl, _, _) if rl.is_red() => {
             right.rotate_right();
             self.rotate_left()
           }
           Node(Red, _, _, rr) if rr.is_red() => self.rotate_left(),

           _ => {}
         },
         _ => {}
       }
     }
   }
  */

  /* fn balance(&mut self) {
    if self.is_black() {
      let self_tree = std::mem::replace(self, Empty);
      match self_tree {
        Node(Black, mut left, z, d) if left.is_red() => {
          let left_tree = std::mem::replace(&mut *left, Empty);
          match left_tree {
            Node(Red, mut ll, lv, lr) if ll.is_red() => {
              let ll_tree = std::mem::replace(&mut *ll, Empty);
              if let Node(Red, a, x, b) = ll_tree {
                *ll = Node(Black, a, x, b);
                *left = Node(Black, lr, z, d);
                *self = Node(Red, ll, lv, left);
              }
            }
            Node(Red, ll, lv, mut lr) if lr.is_red() => {
              let lr_tree = std::mem::replace(&mut *lr, Empty);
              if let Node(Red, b, y, c) = lr_tree {
                *lr = Node(Black, ll, lv, b);
                *left = Node(Black, c, z, d);
                *self = Node(Red, lr, y, left);
              }
            }
            _ => {
              *left = left_tree;
              *self = Node(Black, left, z, d);
            }
          }
        }
        Node(Black, a, x, mut right) if right.is_red() => {
          let right_tree = std::mem::replace(&mut *right, Empty);
          match right_tree {
            Node(Red, mut rl, z, d) if rl.is_red() => {
              let rl_tree = std::mem::replace(&mut *rl, Empty);
              if let Node(Red, b, y, c) = rl_tree {
                *right = Node(Black, a, x, b);
                *rl = Node(Black, c, z, d);
                *self = Node(Red, right, y, rl);
              }
            }
            Node(Red, b, y, mut rr) if rr.is_red() => {
              let rr_tree = std::mem::replace(&mut *rr, Empty);
              if let Node(Red, c, z, d) = rr_tree {
                *rr = Node(Black, a, x, b);
                *right = Node(Black, c, z, d);
                *self = Node(Red, rr, y, right);
              }
            }
            _ => {
              *right = right_tree;
              *self = Node(Black, a, x, right);
            }
          }
        }
        _ => {
          *self = self_tree;
        }
      }
    }
  }
   */
}

#[allow(dead_code)]
fn set_equals<'a>(a: &Set<&'a i32>, b: &Set<&'a i32>) -> bool {
  a.is_subset(b) && b.is_subset(a)
}

pub fn main() {
  let mut tree = RBTree::new();

  //tree.insert(1);
  //tree.insert(2);
  //tree.insert(3);
  //tree.insert(4);
  //tree.insert(5);
  //
  //assert!(tree.size() == 5);
}
