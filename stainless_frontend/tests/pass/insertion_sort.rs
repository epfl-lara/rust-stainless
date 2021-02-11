extern crate stainless;
use stainless::*;

/// This test case was translated in a quite literal fashion from this Scala
/// test case: [1].
///
/// [1]: https://github.com/epfl-lara/stainless/blob/master/frontends/benchmarks/verification/valid/InsertionSort.scala

pub enum Option<T> {
  None,
  Some(T),
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

  #[measure(self)]
  pub fn contents(&self) -> Set<T> {
    match self {
      List::Nil => Set::empty(),
      List::Cons(head, tail) => tail.contents().union(&Set::singleton(head)),
    }
  }
}

impl List<i32> {
  #[measure(self)]
  pub fn is_sorted(&self) -> bool {
    match self {
      List::Nil => true,
      List::Cons(x, tail) => match &**tail {
        List::Nil => true,

        // FIXME: We *have* to deref the two integers here, because otherwise
        //   their type is &i32 which we can't extract to primitive '<=' for the
        //   moment.
        List::Cons(y, ..) => *x <= *y && tail.is_sorted(),
      },
    }
  }

  #[measure(self)]
  pub fn min(&self) -> Option<i32> {
    match self {
      List::Nil => Option::None,
      List::Cons(x, xs) => match xs.min() {
        Option::None => Option::Some(*x),
        Option::Some(y) if *x < y => Option::Some(*x),
        Option::Some(y) => Option::Some(y),
      },
    }
  }

  /// Inserting element 'e' into a sorted list 'l' produces a sorted list with
  /// the expected content and size
  #[pre(self.is_sorted())]
  #[measure(self)]
  #[post(
    ret.size() == self.size() + 1 &&
    ret.is_sorted() &&
    ret.contents().is_subset_of(&self.contents().add(&e)) &&
    self.contents().add(&e).is_subset_of(&ret.contents())
  )]
  pub fn sorted_insert(self, e: i32) -> List<i32> {
    match self {
      List::Cons(head, tail) if head <= e => List::Cons(head, Box::new(tail.sorted_insert(e))),
      _ => List::Cons(e, Box::new(self)),
    }
  }

  /// Insertion sort yields a sorted list of same size and content as the input
  /// list
  #[measure(self)]
  #[post(
    ret.size() == self.size() &&
    ret.is_sorted() &&
    ret.contents().is_subset_of(&self.contents()) &&
    self.contents().is_subset_of(&ret.contents())
  )]
  pub fn sort(self) -> List<i32> {
    match self {
      List::Nil => self,
      List::Cons(x, xs) => xs.sort().sorted_insert(x),
    }
  }
}

#[external]
pub fn main() {
  let list = List::Cons(
    5,
    Box::new(List::Cons(
      2,
      Box::new(List::Cons(
        4,
        Box::new(List::Cons(
          5,
          Box::new(List::Cons(-1, Box::new(List::Cons(8, Box::new(List::Nil))))),
        )),
      )),
    )),
  );

  assert!(list.sort().is_sorted())
}
