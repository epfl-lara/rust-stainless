extern crate stainless;
use stainless::*;

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
    List::Cons { head, tail } => contents(*tail).add(head),
  }
}

pub fn main() {
  let list = List::Cons {
    head: 12,
    tail: Box::new(List::Cons {
      head: 3,
      tail: Box::new(List::Cons {
        head: -1,
        tail: Box::new(List::Nil),
      }),
    }),
  };

  assert!(size(list) == 3);
}

/* Scala tests (not yet translated) copied from
  https://github.com/epfl-lara/stainless/blob/master/frontends/benchmarks/verification/valid/InsertionSort.scala

  /* Inserting element 'e' into a sorted list 'l' produces a sorted list with
   * the expected content and size */
  def sortedIns(e: Int, l: List): List = {
    require(isSorted(l))
    decreases(l)
    l match {
      case Nil() => Cons(e,Nil())
      case Cons(x,xs) => if (x <= e) Cons(x,sortedIns(e, xs)) else Cons(e, l)
    }
  } ensuring(res => contents(res) == contents(l) ++ Set::singleton(e)
                    && isSorted(res)
                    && size(res) == size(l) + 1
            )

  /* Insertion sort yields a sorted list of same size and content as the input
   * list */
  def sort(l: List): List = {
    decreases(l)
    l match {
      case Nil() => Nil()
      case Cons(x,xs) => sortedIns(x, sort(xs))
    }
  } ensuring(res => contents(res) == contents(l)
                     && isSorted(res)
                     && size(res) == size(l)
             )

  @extern
  def main(args: Array[String]): Unit = {
    val ls: List = Cons(5, Cons(2, Cons(4, Cons(5, Cons(1, Cons(8,Nil()))))))

    println(ls)
    println(sort(ls))
  }
}
 */
