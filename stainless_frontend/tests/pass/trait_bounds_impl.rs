extern crate stainless;

pub trait Equals {
  fn eq(&self, other: &Self) -> bool;
}

impl Equals for i32 {
  fn eq(&self, other: &Self) -> bool {
    *self == *other
  }
}

pub enum List<T> {
  Nil,
  Cons(T, Box<List<T>>),
}

impl<T: Equals> List<T> {
  pub fn contains(&self, x: &T) -> bool {
    match self {
      List::Cons(y, tail) => x.eq(y) || tail.contains(x),
      _ => false,
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

  assert!(list.contains(&2));
  assert!(!list.contains(&3))
}
