extern crate stainless;
use stainless::*;

trait Monoid: Sized {
  fn append(&self, other: &Self) -> Self;
  fn neutral() -> Self;

  // This one is needed because we don't have type class inheritance yet and we
  // need equality in the laws.
  fn equals(&self, other: &Self) -> bool;

  // This is needed because, we can't yet extract std::Copy, or std::Clone
  fn clone(&self) -> Self;

  #[law]
  fn associativity(&self, b: &Self, c: &Self) -> bool {
    self.append(b).append(c).equals(&self.append(&b.append(c)))
  }

  #[law]
  fn left_identity(&self) -> bool {
    Self::neutral().append(self).equals(self)
  }

  #[law]
  fn right_identity(&self) -> bool {
    self.append(&Self::neutral()).equals(self)
  }
}

impl Monoid for i32 {
  fn append(&self, other: &i32) -> i32 {
    *self + *other
  }

  fn neutral() -> Self {
    0
  }

  fn equals(&self, other: &i32) -> bool {
    *self == *other
  }

  fn clone(&self) -> Self {
    *self
  }
}

pub enum List<T> {
  Nil,
  Cons(T, Box<List<T>>),
}

impl Monoid for List<i32> {
  fn append(&self, other: &Self) -> Self {
    match self {
      List::Nil => other.clone(),
      List::Cons(h, tail) => List::Cons(*h, Box::new(tail.append(other))),
    }
  }

  fn neutral() -> Self {
    List::Nil
  }

  // The same implementation is in 'type_class.rs'
  fn equals(&self, other: &List<i32>) -> bool {
    match (self, other) {
      (List::Nil, List::Nil) => true,
      (List::Cons(x, xs), List::Cons(y, ys)) => *x == *y && xs.equals(ys),
      _ => false,
    }
  }

  fn clone(&self) -> Self {
    match self {
      List::Nil => List::Nil,
      List::Cons(h, tail) => List::Cons(*h, Box::new((*tail).clone())),
    }
  }
}
