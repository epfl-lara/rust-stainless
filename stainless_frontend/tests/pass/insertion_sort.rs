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
  #[doc(hidden)]
  #[allow(unused_variables)]
  fn __measure_size(&self) -> () {
    self;
  }

  pub fn size(&self) -> u32 {
    match self {
      List::Nil => 0,
      List::Cons(_, tail) => 1 + tail.size(),
    }
  }

  #[doc(hidden)]
  #[allow(unused_variables)]
  fn __measure_contents(&self) -> () {
    self;
  }

  pub fn contents(&self) -> Set<T> {
    match self {
      List::Nil => Set::empty(),
      List::Cons(head, tail) => tail.contents().union(&Set::singleton(head)),
    }
  }
}

#[measure(list)]
pub fn is_sorted(list: &List<i32>) -> bool {
  match list {
    List::Nil => true,
    List::Cons(x, tail) => match &**tail {
      List::Nil => true,

      // FIXME: We *have* to deref the two integers here, because otherwise
      // their type is &i32 which we can't extract to primitive '<=' for the
      // moment.
      List::Cons(y, ..) => *x <= *y && is_sorted(tail),
    },
  }
}

#[measure(list)]
pub fn min(list: &List<i32>) -> Option<i32> {
  match list {
    List::Nil => Option::None,
    List::Cons(x, xs) => match min(&*xs) {
      Option::None => Option::Some(*x),
      Option::Some(y) => {
        if *x < y {
          Option::Some(*x)
        } else {
          Option::Some(y)
        }
      }
    },
  }
}

/// Inserting element 'e' into a sorted list 'l' produces a sorted list with
/// the expected content and size
#[pre(is_sorted(&l))]
#[measure(l)]
#[post(
  ret.size() == l.size() + 1 &&
  is_sorted(&ret) &&
  ret.contents().is_subset_of(&l.contents().add(&e)) &&
  l.contents().add(&e).is_subset_of(&ret.contents())
)]
pub fn sorted_insert(e: i32, l: List<i32>) -> List<i32> {
  match l {
    List::Cons(head, tail) if head <= e => List::Cons(head, Box::new(sorted_insert(e, *tail))),
    _ => List::Cons(e, Box::new(l)),
  }
}

/// Insertion sort yields a sorted list of same size and content as the input
/// list
#[measure(l)]
#[post(
  ret.size() == l.size() &&
  is_sorted(&ret) &&
  ret.contents().is_subset_of(&l.contents()) &&
  l.contents().is_subset_of(&ret.contents())
)]
pub fn sort(l: List<i32>) -> List<i32> {
  match l {
    List::Nil => l,
    List::Cons(x, xs) => sorted_insert(x, sort(*xs)),
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

  assert!(is_sorted(&sort(list)))
}
