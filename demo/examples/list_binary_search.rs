extern crate stainless;
use stainless::*;

/// This test was ported to linked lists from the Scala test case [1].
///
/// [1]: https://github.com/epfl-lara/stainless/blob/master/frontends/benchmarks/verification/valid/BinarySearch.scala

pub enum Option<T> {
  None,
  Some(T),
}

impl<T> Option<T> {
  pub fn is_some(&self) -> bool {
    match self {
      Option::None => false,
      Option::Some(..) => true,
    }
  }
}

pub enum List<T> {
  Nil,
  Cons(T, Box<List<T>>),
}

impl<T> List<T> {
  #[measure(self)]
  pub fn size(&self) -> u32 {
    match self {
      List::Nil => 0,
      List::Cons(_, tail) => 1 + tail.size(),
    }
  }

  // Check that we can have a measure where the parameter is consumed.
  #[measure(self)]
  pub fn consume(self) -> () {
    match self {
      List::Nil => (),
      List::Cons(..) => (),
    }
  }

  #[pre(index < self.size())]
  #[post(ret.is_some())]
  pub fn get(&self, index: u32) -> Option<&T> {
    match self {
      List::Nil => Option::None,
      List::Cons(head, _) if index == 0 => Option::Some(head),
      List::Cons(_, tail) => tail.get(index - 1),
    }
  }
}

#[pre(lo <= hi + 1 && hi < list.size())]
#[measure(hi - lo + 1)]
pub fn search(list: &List<i32>, x: i32, lo: u32, hi: u32) -> bool {
  lo <= hi && {
    let i = lo + (hi - lo) / 2;
    match list.get(i) {
      Option::None => false,
      Option::Some(&y) => {
        x == y
          || x < y && i > 0 && search(list, x, lo, i - 1)
          || x > y && search(list, x, i + 1, hi)
      }
    }
  }
}

#[external]
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

  assert!(search(&list, 7, 0, 5));
  assert!(!search(&list, 1, 0, 5));

  let list2 = List::Cons(1, Box::new(List::Nil));
  assert!(!search(&list2, 0, 0, 0));
}
