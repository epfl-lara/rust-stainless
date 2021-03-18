extern crate stainless;
use stainless::*;

pub enum List<T> {
  Nil,
  Cons(T, Box<List<T>>),
}

pub trait Equals {
  fn eq(&self, other: &Self) -> bool;

  #[law]
  fn law_reflexive(x: &Self) -> bool {
    x.eq(x)
  }
}

impl Equals for i32 {
  fn eq(&self, other: &Self) -> bool {
    *self == *other
  }
}

pub trait Hash {
  fn hash(&self) -> i32;
}

impl Hash for i32 {
  fn hash(&self) -> i32 {
    *self - 1000000
  }
}

fn super_hash<T: Hash>(x: &T) -> i32 {
  x.hash() + 123
}

impl<T: Equals> List<T> {
  pub fn contains(&self, x: &T) -> bool {
    match self {
      List::Cons(y, tail) => x.eq(y) || tail.contains(x),
      _ => false,
    }
  }

  pub fn combined_bound<U: Hash>(&self, x: &T, s: &U) -> i32 {
    match self {
      List::Cons(y, _) => {
        if x.eq(y) {
          0
        } else {
          -1
        }
      }
      _ => s.hash(),
    }
  }
}

#[post(ret == 123)]
pub fn fn_with_bound_and_spec<T: Equals>(x: &T) -> i32 {
  if x.eq(x) {
    123
  } else {
    123
  }
}

impl<T: Equals> List<T> {
  #[post(ret == 123)]
  pub fn fn_with_bound_and_spec(&self) -> i32 {
    match self {
      List::Nil => 123,
      List::Cons(x, _) => {
        if x.eq(x) {
          123
        } else {
          123
        }
      }
    }
  }
}

pub fn main() {
  let list = List::Cons(
    -10,
    Box::new(List::Cons(
      2,
      Box::new(List::Cons(
        4,
        Box::new(List::Cons(
          5,
          Box::new(List::Cons(7, Box::new(List::Cons(8, Box::new(List::Nil))))),
        )),
      )),
    )),
  );

  assert!(super_hash(&456) == 456 - 1000000 + 123);

  assert!(list.contains(&2));
  assert!(!list.contains(&3))
}
