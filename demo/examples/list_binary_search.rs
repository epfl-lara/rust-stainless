extern crate stainless;
use stainless::*;

/// This test was ported to linked lists from the Scala test case [1].
///
/// [1]: https://github.com/epfl-lara/stainless/blob/master/frontends/benchmarks/verification/valid/BinarySearch.scala

pub enum List<T> {
  Nil,
  Cons { head: T, tail: Box<List<T>> },
}

pub enum Option<T> {
  None,
  Some { value: T },
}

pub fn is_some<T>(opt: &Option<T>) -> bool {
  match opt {
    Option::None => false,
    Option::Some { .. } => true,
  }
}

#[measure(l)]
pub fn size<T>(l: &List<T>) -> u32 {
  match l {
    List::Nil => 0,
    List::Cons { tail, .. } => 1 + size(tail),
  }
}

#[pre(index < size(&list))]
#[post(is_some(&ret))]
pub fn get<T>(list: &List<T>, index: u32) -> Option<&T> {
  match list {
    List::Nil => Option::None,
    List::Cons { head, tail } => {
      if index == 0 {
        Option::Some { value: head }
      } else {
        get(tail, index - 1)
      }
    }
  }
}

#[pre(lo <= hi + 1 && hi < size(&list))]
#[measure(hi - lo + 1)]
pub fn search(list: &List<i32>, x: i32, lo: u32, hi: u32) -> bool {
  lo <= hi && {
    let i = lo + (hi - lo) / 2;
    match get(&list, i) {
      Option::None => false,
      Option::Some { value: &y } => {
        x == y
          || x < y && i > 0 && search(list, x, lo, i - 1)
          || x > y && search(list, x, i + 1, hi)
      }
    }
  }
}

#[external]
pub fn main() {
  let list = List::Cons {
    head: -10,
    tail: Box::new(List::Cons {
      head: 2,
      tail: Box::new(List::Cons {
        head: 4,
        tail: Box::new(List::Cons {
          head: 5,
          tail: Box::new(List::Cons {
            head: 7,
            tail: Box::new(List::Cons {
              head: 8,
              tail: Box::new(List::Nil),
            }),
          }),
        }),
      }),
    }),
  };

  assert!(search(&list, 7, 0, 5));
  assert!(!search(&list, 1, 0, 5));

  let list2 = List::Cons {
    head: 1,
    tail: Box::new(List::Nil),
  };
  assert!(!search(&list2, 0, 0, 0));
}
