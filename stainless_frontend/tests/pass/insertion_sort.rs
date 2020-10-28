extern crate stainless;
use stainless::*;

/// This test case was translated in a quite literal fashion from this Scala
/// test case: [1].
///
/// [1]: https://github.com/epfl-lara/stainless/blob/master/frontends/benchmarks/verification/valid/InsertionSort.scala

pub enum List {
  Nil,
  Cons { head: i32, tail: Box<List> },
}

pub enum IntOption {
  None,
  Some { value: i32 },
}

#[measure(l)]
pub fn size(l: List) -> u32 {
  match l {
    List::Nil => 0,
    List::Cons { tail, .. } => 1 + size(*tail),
  }
}

#[measure(l)]
pub fn is_sorted(l: List) -> bool {
  match l {
    List::Nil => true,
    List::Cons { head: x, tail } => match *tail {
      List::Nil => true,
      List::Cons { head: y, tail: ys } => x <= y && is_sorted(List::Cons { head: y, tail: ys }),
    },
  }
}

#[measure(l)]
pub fn min(l: List) -> IntOption {
  match l {
    List::Nil => IntOption::None,
    List::Cons { head: x, tail: xs } => match min(*xs) {
      IntOption::None => IntOption::Some { value: x },
      IntOption::Some { value: y } => {
        if x < y {
          IntOption::Some { value: x }
        } else {
          IntOption::Some { value: y }
        }
      }
    },
  }
}

#[measure(l)]
pub fn contents(l: List) -> Set<i32> {
  match l {
    List::Nil => Set::empty(),
    List::Cons { head, tail } => contents(*tail).union(Set::singleton(head)),
  }
}

/// Inserting element 'e' into a sorted list 'l' produces a sorted list with
/// the expected content and size
#[pre(is_sorted(l))]
#[measure(l)]
#[post(size(ret) == size(l) + 1)]
#[post(is_sorted(ret))]
#[post(contents(ret).is_subset_of(contents(l).add(e)))]
#[post(contents(l).add(e).is_subset_of(contents(ret)))]
pub fn sorted_insert(e: i32, l: List) -> List {
  match l {
    List::Nil => List::Cons {
      head: e,
      tail: Box::new(List::Nil),
    },
    List::Cons { head: x, tail: xs } => {
      if x <= e {
        List::Cons {
          head: x,
          tail: Box::new(sorted_insert(e, *xs)),
        }
      } else {
        List::Cons {
          head: e,
          tail: Box::new(List::Cons { head: x, tail: xs }),
        }
      }
    }
  }
}

/// Insertion sort yields a sorted list of same size and content as the input
/// list
#[measure(l)]
#[post(size(ret) == size(l))]
#[post(is_sorted(ret))]
#[post(contents(ret).is_subset_of(contents(l)))]
#[post(contents(l).is_subset_of(contents(ret)))]
pub fn sort(l: List) -> List {
  match l {
    List::Nil => l,
    List::Cons { head: x, tail: xs } => sorted_insert(x, sort(*xs)),
  }
}

#[external]
pub fn main() {
  let list = List::Cons {
    head: 5,
    tail: Box::new(List::Cons {
      head: 2,
      tail: Box::new(List::Cons {
        head: 4,
        tail: Box::new(List::Cons {
          head: 5,
          tail: Box::new(List::Cons {
            head: -1,
            tail: Box::new(List::Cons {
              head: 8,
              tail: Box::new(List::Nil),
            }),
          }),
        }),
      }),
    }),
  };

  assert!(is_sorted(sort(list)))
}
