extern crate stainless;
use stainless::*;

/// This test was ported to linked lists from the Scala test case [1].
///
/// [1]: https://github.com/epfl-lara/stainless/blob/master/frontends/benchmarks/verification/valid/BinarySearch.scala

pub enum List<T> {
  Nil,
  Cons { head: T, tail: Box<List<T>> },
}

#[measure(l)]
pub fn size<T>(l: List<T>) -> u32 {
  match l {
    List::Nil => 0,
    List::Cons { tail, .. } => 1 + size(*tail),
  }
}

#[pre(index < size(list))]
pub fn get<T>(list: List<T>, index: u32) -> T {
  match list {
    List::Nil => panic!(),
    List::Cons { head, tail } => {
      if index == 0 {
        head
      } else {
        get(*tail, index - 1)
      }
    }
  }
}

#[pre(lo <= hi + 1 && hi < size(list))]
#[measure(hi - lo + 1)]
pub fn search(list: List<i32>, x: i32, lo: u32, hi: u32) -> bool {
  lo <= hi && {
    let i = lo + (hi - lo) / 2;
    let y = get(list, i);
    x == y
      || if x < y {
        search(list, x, lo, i - 1)
      } else {
        search(list, x, i + 1, hi)
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

  assert!(search(list, 7, 0, 6))
}
